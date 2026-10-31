module LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros (theSiegeOfCairAndros) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheSiegeOfCairAndros = TheSiegeOfCairAndros ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSiegeOfCairAndros :: Scenario
theSiegeOfCairAndros = Scenario $ scenario TheSiegeOfCairAndros "the-siege-of-cair-andros" "The Siege of Cair Andros"

instance RunMessage TheSiegeOfCairAndros where
  runMessage msg (TheSiegeOfCairAndros attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheSiegeOfCairAndros, EncounterSet.MordorElite, EncounterSet.RavagingOrcs, EncounterSet.Southrons]
      firstStage <- genEncounterCard Quests.theDefense
      laterStages <-
        traverse
          genEncounterCard
          [Quests.reinforcingTheBanks, Quests.breakthroughAtTheApproach, Quests.breakthroughAtTheCitadel, Quests.theLastBattle]
      putQuestIntoPlay firstStage
      pure $ TheSiegeOfCairAndros $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheSiegeOfCairAndros <$> runMessage msg attrs
