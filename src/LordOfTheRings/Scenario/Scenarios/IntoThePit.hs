module LordOfTheRings.Scenario.Scenarios.IntoThePit (intoThePit) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype IntoThePit = IntoThePit ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

intoThePit :: Scenario
intoThePit = Scenario $ scenario IntoThePit "into-the-pit" "Into the Pit"

instance RunMessage IntoThePit where
  runMessage msg (IntoThePit attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.IntoThePit, EncounterSet.HazardsOfThePit, EncounterSet.TwistsAndTurns, EncounterSet.GoblinsOfTheDeep]
      firstStage <- genEncounterCard Quests.enteringTheMines
      laterStages <- traverse genEncounterCard [Quests.goblinPatrol, Quests.aWayUp]
      putQuestIntoPlay firstStage
      pure $ IntoThePit $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> IntoThePit <$> runMessage msg attrs
