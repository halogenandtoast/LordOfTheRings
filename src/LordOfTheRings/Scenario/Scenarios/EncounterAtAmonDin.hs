module LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin (encounterAtAmonDin) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (deadVillagerCount, rescuedVillagerCount)

newtype EncounterAtAmonDin = EncounterAtAmonDin ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

encounterAtAmonDin :: Scenario
encounterAtAmonDin = Scenario $ scenario EncounterAtAmonDin "encounter-at-amon-din" "Encounter at Amon Dîn"

instance RunMessage EncounterAtAmonDin where
  runMessage msg (EncounterAtAmonDin attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- concatMapM (gatherEncounterSet attrs) [EncounterSet.EncounterAtAmonDin, EncounterSet.RavagingOrcs]
      firstStage <- genEncounterCard Quests.savageryOfTheOrcs
      laterStages <- traverse genEncounterCard [Quests.protectTheVillagers]
      putQuestIntoPlay firstStage
      pure $ EncounterAtAmonDin $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Protect the Villagers is defeated and the game ends: the players have won if there are
    -- more villager tokens on Rescued Villagers than damage tokens on Dead Villagers, and
    -- have lost otherwise
    AdvanceQuest | null attrs.questDeck -> do
      rescued <- rescuedVillagerCount
      dead <- deadVillagerCount
      gameOver $ if rescued > dead then Won else Lost
      pure $ EncounterAtAmonDin attrs
    _ -> EncounterAtAmonDin <$> runMessage msg attrs
