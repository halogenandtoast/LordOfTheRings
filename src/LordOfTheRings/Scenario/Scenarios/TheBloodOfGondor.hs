module LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor (theBloodOfGondor) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (attackDestroys, mordorLoomsMarks, turnHiddenCardsFaceup)
import LordOfTheRings.Source (Source (EnemySource))

newtype TheBloodOfGondor = TheBloodOfGondor ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBloodOfGondor :: Scenario
theBloodOfGondor = Scenario $ scenario TheBloodOfGondor "the-blood-of-gondor" "The Blood of Gondor"

instance RunMessage TheBloodOfGondor where
  runMessage msg (TheBloodOfGondor attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.TheBloodOfGondor, EncounterSet.RavagingOrcs]
      firstStage <- genEncounterCard Quests.theAmbush
      laterStages <- traverse genEncounterCard [Quests.captured]
      putQuestIntoPlay firstStage
      pure $ TheBloodOfGondor $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Mordor Looms' shadow: "If this attack destroys a character, that character's
    -- controller turns each of his hidden cards faceup."
    DoStep 1 (DealDamage (EnemySource eid) target n) -> do
      whenM (mordorLoomsMarks eid) $ attackDestroys target n >>= traverse_ (void . turnHiddenCardsFaceup)
      TheBloodOfGondor <$> runMessage msg attrs
    _ -> TheBloodOfGondor <$> runMessage msg attrs
