{- | Foundations of Stone. The players share one staging area and stage throughout, stage 4
too: see "LordOfTheRings.Scenario.Scenarios.FoundationsOfStone.Helpers".
-}
module LordOfTheRings.Scenario.Scenarios.FoundationsOfStone (foundationsOfStone) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype FoundationsOfStone = FoundationsOfStone ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

foundationsOfStone :: Scenario
foundationsOfStone = Scenario $ scenario FoundationsOfStone "foundations-of-stone" "Foundations of Stone"

-- | The Foundations of Stone set waits out of play until Washed Away! shuffles it into a
-- new encounter deck; Washed Away! draws the stage 4s, so only stage 5 waits behind it.
instance RunMessage FoundationsOfStone where
  runMessage msg (FoundationsOfStone attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TwistsAndTurns, EncounterSet.GoblinsOfTheDeep, EncounterSet.HazardsOfThePit]
      foundations <- gatherEncounterSet attrs EncounterSet.FoundationsOfStone
      firstStage <- genEncounterCard Quests.theDrippingWalls
      laterStages <- traverse genEncounterCard [Quests.theWatersEdge, Quests.washedAway, Quests.outOfTheDepths]
      putQuestIntoPlay firstStage
      pure
        $ FoundationsOfStone
        $ attrs
        & encounterDeckL
        .~ encounterDeck
        & setAsideL
        .~ foundations
        & questDeckL
        .~ laterStages
    _ -> FoundationsOfStone <$> runMessage msg attrs
