{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location.Runner (module X) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (getModifiers, modifiedValue, questPointsOf)
import LordOfTheRings.Location.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (ImmuneToCardEffects, ImmuneToPlayerCardEffects, NoProgressFromPlayerCards))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

instance RunMessage LocationAttrs where
  runMessage msg attrs = case msg of
    PlaceLocation lid placement | lid == attrs.id -> pure $ attrs & placementL .~ placement
    -- tokens a player card would place or remove do not reach an immune location
    PlaceTokens source (isTarget attrs -> True) _ _ | source /= GameSource -> unlessImmune source
    RemoveTokens source (isTarget attrs -> True) _ _ | source /= GameSource -> unlessImmune source
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      let attrs' = attrs & tokensL %~ addTokens token n
      questPoints <- modifiedValue questPointsOf attrs attrs'.questPoints
      -- a location with as much progress as its quest points is explored
      when (token == Progress && questPoints > 0 && attrs'.progress >= questPoints)
        $ pushAll
          [ CheckWindows [mkWhen $ Explored attrs.id]
          , -- a location with victory points goes to the victory display
            if isJust (cdVictoryPoints $ toCardDef attrs)
              then AddToVictoryDisplay (toTarget attrs)
              else Discard (toTarget attrs)
          , -- after it is explored, as Idraen answers
            CheckWindows [mkAfter $ Explored attrs.id]
          ]
      pure attrs'
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    unlessImmune source = do
      mods <- getModifiers attrs
      let barred = case msg of
            PlaceTokens _ _ Progress _ -> NoProgressFromPlayerCards `elem` mods
            _ -> False
          immune = ImmuneToCardEffects `elem` mods || (isPlayerCardSource source && (ImmuneToPlayerCardEffects `elem` mods || barred))
      -- the message again, from a source the guard lets by
      if immune then pure attrs else runMessage (asGame msg) attrs
    asGame = \case
      PlaceTokens _ t token n -> PlaceTokens GameSource t token n
      RemoveTokens _ t token n -> RemoveTokens GameSource t token n
      m -> m
