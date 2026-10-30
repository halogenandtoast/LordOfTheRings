module LordOfTheRings.Scenario.Scenarios.IntrudersInChetwood (intrudersInChetwood) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype IntrudersInChetwood = IntrudersInChetwood ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

intrudersInChetwood :: Scenario
intrudersInChetwood = Scenario $ scenario IntrudersInChetwood "intruders-in-chetwood" "Intruders in Chetwood"

-- | Built from Intruders in Chetwood, Eriador Wilds and Angmar Orcs, with Iârion from The
-- Lost Realm set, whom the stage's setup takes out again. One stage.
instance RunMessage IntrudersInChetwood where
  runMessage msg (IntrudersInChetwood attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.IntrudersInChetwood, EncounterSet.EriadorWilds, EncounterSet.AngmarOrcs, EncounterSet.TheLostRealm]
      putQuestIntoPlay =<< genEncounterCard Quests.stopTheWarParty
      pure $ IntrudersInChetwood $ attrs & encounterDeckL .~ encounterDeck
    _ -> IntrudersInChetwood <$> runMessage msg attrs
