module LordOfTheRings.Scenario.Scenarios.PassageThroughMirkwood (passageThroughMirkwood) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype PassageThroughMirkwood = PassageThroughMirkwood ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

passageThroughMirkwood :: Scenario
passageThroughMirkwood = Scenario $ scenario PassageThroughMirkwood "passage-through-mirkwood" "Passage Through Mirkwood"

instance RunMessage PassageThroughMirkwood where
  runMessage msg (PassageThroughMirkwood attrs) = case msg of
    -- the first stage is out from the start, its setup followed at step 7 as ever
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.DolGuldurOrcs, EncounterSet.PassageThroughMirkwood, EncounterSet.SpidersOfMirkwood]
      firstStage <- genEncounterCard Quests.fliesAndSpiders
      laterStages <-
        traverse
          genEncounterCard
          [Quests.aForkInTheRoad, Quests.aChosenPathDontLeaveThePath, Quests.aChosenPathBeornsPath]
      putQuestIntoPlay firstStage
      pure $ PassageThroughMirkwood $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- A Fork in the Road: proceed to one of the 2 A Chosen Path stages, at random.
    AdvanceQuest | stages@[_, _] <- attrs.questDeck, all isChosenPath stages -> do
      push . AdvanceToQuest =<< uniform stages
      pure $ PassageThroughMirkwood $ attrs & questDeckL .~ []
    _ -> PassageThroughMirkwood <$> runMessage msg attrs
   where
    isChosenPath = (`elem` map toCardCode [Quests.aChosenPathDontLeaveThePath, Quests.aChosenPathBeornsPath]) . toCardCode
