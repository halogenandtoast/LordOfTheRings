module LordOfTheRings.Scenario.Scenarios.TheDunlandTrap (theDunlandTrap) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheDunlandTrap = TheDunlandTrap ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDunlandTrap :: Scenario
theDunlandTrap = Scenario $ scenario TheDunlandTrap "the-dunland-trap" "The Dunland Trap"

-- | The encounter deck is built from The Dunland Trap, Dunland Warriors and Weary
-- Travelers.
instance RunMessage TheDunlandTrap where
  runMessage msg (TheDunlandTrap attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheDunlandTrap, EncounterSet.DunlandWarriors, EncounterSet.WearyTravelers]
      firstStage <- genEncounterCard Quests.theRoadToTharbad
      laterStages <- traverse genEncounterCard [Quests.aWellLaidTrap, Quests.noWayOut]
      putQuestIntoPlay firstStage
      pure $ TheDunlandTrap $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheDunlandTrap <$> runMessage msg attrs
