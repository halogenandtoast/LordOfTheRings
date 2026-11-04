module LordOfTheRings.Location.Cards.FoothillsOfEphelDuath (foothillsOfEphelDuath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (haradEnemy)

newtype FoothillsOfEphelDuath = FoothillsOfEphelDuath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

foothillsOfEphelDuath :: LocationCard FoothillsOfEphelDuath
foothillsOfEphelDuath = location FoothillsOfEphelDuath Cards.foothillsOfEphelDuath

-- | While Foothills of Ephel Dúath is the active location, each Harad enemy gets -1 threat.
-- Travel: The first player engages an enemy in the staging area to travel here: not
-- without one to engage.
instance HasModifiersFor FoothillsOfEphelDuath where
  getModifiersFor (FoothillsOfEphelDuath a) = do
    when (a.placement == ActiveLocation) do
      harad <- select haradEnemy
      for_ harad \e -> modified a e [ThreatModifier (-1)]
    staged <- select EnemyInStagingArea
    when (null staged) $ modifySelf a [CannotBeTraveledTo]

instance RunMessage FoothillsOfEphelDuath where
  runMessage msg (FoothillsOfEphelDuath a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      staged <- select EnemyInStagingArea
      chooseOrRunOneM lead do
        questionLabeled "foothillsOfEphelDuath.engage"
        causedBy a
        targets staged (engageEnemy lead)
      FoothillsOfEphelDuath <$> runMessage msg a
    _ -> FoothillsOfEphelDuath <$> runMessage msg a
