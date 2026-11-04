{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location.Runner (module X) where

import LordOfTheRings.Card (CardDef (..), toCard, toCardDef)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Helpers.Modifiers (getModifiers, immuneToSource, modifiedValue, questPointsOf)
import LordOfTheRings.Location.Types as X
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (NoProgressFromPlayerCards, ProgressLimit, ProgressPerRoundLimit, ProgressPlacedThisRound, ProgressReducedTo))
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
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
    -- a location takes no more progress than it may this round, as The Hills of Emyn
    -- Muil's nightmare text limits the staging area's, and keeps count of what it took
    PlaceTokens source (isTarget attrs -> True) Progress n0 | n0 > 0 -> do
      mods <- getModifiers attrs
      let n = foldr min n0 [k | ProgressReducedTo k <- mods]
          limits = [k | ProgressPerRoundLimit k <- mods]
          placed = sum [k | ProgressPlacedThisRound k <- mods]
          room = foldr (\k -> min (max 0 $ k - placed)) n limits
      if
        | null limits -> placeTokens Progress n
        | room <= 0 -> pure attrs
        | otherwise -> do
            push $ CreateEffect $ Effect source (toTarget attrs) [ProgressPlacedThisRound room] UntilEndOfRound
            placeTokens Progress room
    PlaceTokens _ (isTarget attrs -> True) token n -> placeTokens token n
    PlaceUnderneath (isTarget attrs -> True) cards -> pure $ attrs & underneathL <>~ cards
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    placeTokens token n = do
      let attrs' = attrs & tokensL %~ addTokens token n
      questPoints <- modifiedValue questPointsOf attrs attrs'.questPoints
      -- a location with as much progress as its quest points is explored, but one on the
      -- Island Map stays in its place (Temple of the Deceived)
      when (token == Progress && questPoints > 0 && attrs'.progress >= questPoints && isNothing attrs.position)
        $ pushAll
          [ CheckWindows [mkWhen $ Explored attrs.id]
          , -- a location with victory points goes to the victory display
            if isJust (cdVictoryPoints $ toCardDef attrs)
              then AddToVictoryDisplay (toTarget attrs)
              else Discard (toTarget attrs)
          , -- after it is explored, as Idraen answers
            CheckWindows [mkAfter $ Explored attrs.id, mkAfter $ ExploredCard (toCard attrs)]
          ]
      pure attrs'
    unlessImmune source = do
      mods <- getModifiers attrs
      let barred = case msg of
            PlaceTokens _ _ Progress _ -> NoProgressFromPlayerCards `elem` mods
            _ -> False
          -- no card places progress on a location that takes no more, as Broken Lands bars it
          noMore = case msg of
            PlaceTokens _ _ Progress _ -> ProgressLimit 0 `elem` mods
            _ -> False
      immune <- (|| noMore || (isPlayerCardSource source && barred)) <$> immuneToSource source attrs
      -- the message again, from a source the guard lets by
      if immune then pure attrs else runMessage (asGame msg) attrs
    asGame = \case
      PlaceTokens _ t token n -> PlaceTokens GameSource t token n
      RemoveTokens _ t token n -> RemoveTokens GameSource t token n
      m -> m
