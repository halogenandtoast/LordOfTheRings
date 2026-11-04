module LordOfTheRings.Scenario.Scenarios.UnderTheAshMountains (underTheAshMountains) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype UnderTheAshMountains = UnderTheAshMountains ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underTheAshMountains :: Scenario
underTheAshMountains = Scenario $ scenario UnderTheAshMountains "under-the-ash-mountains" "Under the Ash Mountains"

-- | Under the Ash Mountains and Twisted Tunnels make up the encounter deck (MEC81 rules).
-- Stage 1A's setup does the rest.
instance RunMessage UnderTheAshMountains where
  runMessage msg (UnderTheAshMountains attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <- gatherEncounterSets attrs [EncounterSet.UnderTheAshMountains, EncounterSet.TwistedTunnels]
      firstStage <- genEncounterCard Quests.passageIntoMordor
      laterStages <- traverse genEncounterCard [Quests.anEvilPlace]
      putQuestIntoPlay firstStage
      pure $ UnderTheAshMountains $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> UnderTheAshMountains <$> runMessage msg attrs
