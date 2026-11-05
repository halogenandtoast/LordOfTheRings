module LordOfTheRings.Location.Cards.ShadowyThicket (shadowyThicket) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveUnderneath))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (getTheBlackGate, cardsUnderTheCrossRoads, revealFromUnderTheCrossRoads)

newtype ShadowyThicket = ShadowyThicket LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

shadowyThicket :: LocationCard ShadowyThicket
shadowyThicket = location ShadowyThicket Cards.shadowyThicket

instance HasModifiersFor ShadowyThicket where
  getModifiersFor (ShadowyThicket a) = do
    gate <- getTheBlackGate
    let n = maybe 0 (length . objectiveUnderneath . toAttrs) gate
    under <- cardsUnderTheCrossRoads
    modifySelf a $ [ThreatModifier (2 * n)] <> [CannotBeTraveledTo | null under]

instance RunMessage ShadowyThicket where
  runMessage msg (ShadowyThicket a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealFromUnderTheCrossRoads Nothing
      ShadowyThicket <$> runMessage msg a
    _ -> ShadowyThicket <$> runMessage msg a
