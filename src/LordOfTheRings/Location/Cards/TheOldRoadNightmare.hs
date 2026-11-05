module LordOfTheRings.Location.Cards.TheOldRoadNightmare (theOldRoadNightmare) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AKnifeInTheDark.Helpers (shuffleOutOfPlayRingwraith)

newtype TheOldRoadNightmare = TheOldRoadNightmare LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOldRoadNightmare :: LocationCard TheOldRoadNightmare
theOldRoadNightmare =
  location
    (TheOldRoadNightmare . \a -> a {locationTokens = addTokens Resource 3 a.locationTokens})
    Cards.theOldRoadNightmare

instance HasAbilities TheOldRoadNightmare where
  getAbilities (TheOldRoadNightmare a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage TheOldRoadNightmare where
  runMessage msg l@(TheOldRoadNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      removeTokens a a Resource 1
      push $ DoStep 1 msg
      pure l
    DoStep 1 (UseCardAbility _ (isSource a -> True) 1 _) -> do
      when (countTokens Resource a.locationTokens == 0) do
        lead <- getLead
        shuffleOutOfPlayRingwraith lead a
      pure l
    _ -> TheOldRoadNightmare <$> runMessage msg a
