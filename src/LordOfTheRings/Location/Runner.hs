module LordOfTheRings.Location.Runner (module LordOfTheRings.Location.Runner, module X) where

import LordOfTheRings.Card (CardDef (..), toCardDef)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Location.Types as X
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Message
import LordOfTheRings.Modifier (ModifierType (ImmuneToPlayerCardEffects))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

instance RunMessage LocationAttrs where
  runMessage msg attrs = case msg of
    PlaceLocation lid placement | lid == attrs.id -> pure $ attrs & placementL .~ placement
    -- tokens a player card would place or remove do not reach an immune location
    PlaceTokens source (isTarget attrs -> True) _ _ | isPlayerCardSource source -> unlessImmune
    RemoveTokens source (isTarget attrs -> True) _ _ | isPlayerCardSource source -> unlessImmune
    PlaceTokens _ (isTarget attrs -> True) token n -> do
      let attrs' = attrs & tokensL %~ addTokens token n
      -- a location with as much progress as its quest points is explored
      when (token == Progress && attrs'.questPoints > 0 && attrs'.progress >= attrs'.questPoints)
        $ pushAll
          [ CheckWindows [mkWhen $ Explored attrs.id]
          , -- a location with victory points goes to the victory display
            if isJust (cdVictoryPoints $ toCardDef attrs) then AddToVictoryDisplay (toTarget attrs) else Discard (toTarget attrs)
          ]
      pure attrs'
    RemoveTokens _ (isTarget attrs -> True) token n -> do
      pure $ attrs & tokensL %~ subtractTokens token n
    _ -> pure attrs
   where
    unlessImmune = do
      immune <- elem ImmuneToPlayerCardEffects <$> getModifiers attrs
      -- the message again, from a source the guard lets by
      if immune then pure attrs else runMessage (asGame msg) attrs
    asGame = \case
      PlaceTokens _ t token n -> PlaceTokens GameSource t token n
      RemoveTokens _ t token n -> RemoveTokens GameSource t token n
      m -> m
