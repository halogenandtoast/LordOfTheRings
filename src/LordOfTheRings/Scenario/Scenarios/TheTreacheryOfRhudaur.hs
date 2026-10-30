module LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur (theTreacheryOfRhudaur) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheTreacheryOfRhudaur = TheTreacheryOfRhudaur ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTreacheryOfRhudaur :: Scenario
theTreacheryOfRhudaur = Scenario $ scenario TheTreacheryOfRhudaur "the-treachery-of-rhudaur" "The Treachery of Rhudaur"

-- | The Treachery of Rhudaur, Cursed Dead and Ruins of Arnor encounter sets.
instance RunMessage TheTreacheryOfRhudaur where
  runMessage msg (TheTreacheryOfRhudaur attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.TheTreacheryOfRhudaur, EncounterSet.CursedDead, EncounterSet.RuinsOfArnor]
      firstStage <- genEncounterCard Quests.secretsOfRhudaur
      laterStages <- traverse genEncounterCard [Quests.thaurdirsPursuit]
      putQuestIntoPlay firstStage
      pure $ TheTreacheryOfRhudaur $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheTreacheryOfRhudaur <$> runMessage msg attrs
