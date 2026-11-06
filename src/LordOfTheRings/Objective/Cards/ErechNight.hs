module LordOfTheRings.Objective.Cards.ErechNight (eventide, dusk, midnight) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted

newtype ErechNight = ErechNight ObjectiveAttrs
  deriving anyclass (IsObjective, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

eventide, dusk, midnight :: ObjectiveCard ErechNight
eventide = objective ErechNight Cards.eventide
dusk = objective ErechNight Cards.dusk
midnight = objective ErechNight Cards.midnight

instance HasAbilities ErechNight where
  getAbilities (ErechNight a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage ErechNight where
  runMessage msg o@(ErechNight a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      placeProgress a a 1
      when (toCardCode a == toCardCode Cards.midnight) do
        eachPlayer \pid -> raiseThreat pid (a.progress + 1)
      pure o
    PlaceTokens _ (isTarget a -> True) Progress n
      | n > 0, a.progress + n >= 4, toCardCode a /= toCardCode Cards.midnight -> do
          addToVictoryDisplay a
          let next = if toCardCode a == toCardCode Cards.eventide then Cards.dusk else Cards.midnight
          addSetAsideToStaging (cardIs next)
          ErechNight <$> runMessage msg a
    _ -> ErechNight <$> runMessage msg a
