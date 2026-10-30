module LordOfTheRings.Scenario.Scenarios.TheThingInTheDepths (theThingInTheDepths) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheThingInTheDepths = TheThingInTheDepths ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theThingInTheDepths :: Scenario
theThingInTheDepths = Scenario $ scenario TheThingInTheDepths "the-thing-in-the-depths" "The Thing in the Depths"

{- | The encounter deck is built from The Stormcaller and Corsair Raiders alone; The Thing
in the Depths and The Stormcaller Elite (enemy side faceup) are set aside, out of play, as
Take the Ship! reads. There is no Corsair deck and no fleet.
-}
instance RunMessage TheThingInTheDepths where
  runMessage msg (TheThingInTheDepths attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- concatMapM (gatherEncounterSet attrs) [EncounterSet.TheStormcaller, EncounterSet.CorsairRaiders]
      outOfPlay <- concatMapM (gatherEncounterSet attrs) [EncounterSet.TheThingInTheDepths, EncounterSet.TheStormcallerElite]
      firstStage <- genEncounterCard Quests.takeTheShip
      laterStages <- traverse genEncounterCard [Quests.changeOfPlans, Quests.defendTheShip]
      putQuestIntoPlay firstStage
      pure
        $ TheThingInTheDepths
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ outOfPlay
    _ -> TheThingInTheDepths <$> runMessage msg attrs
