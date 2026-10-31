module LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath (assaultOnOsgiliath) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype AssaultOnOsgiliath = AssaultOnOsgiliath ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

assaultOnOsgiliath :: Scenario
assaultOnOsgiliath = Scenario $ scenario AssaultOnOsgiliath "assault-on-osgiliath" "Assault on Osgiliath"

instance RunMessage AssaultOnOsgiliath where
  runMessage msg (AssaultOnOsgiliath attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM (gatherEncounterSet attrs) [EncounterSet.AssaultOnOsgiliath, EncounterSet.MordorElite, EncounterSet.Southrons]
      firstStage <- genEncounterCard Quests.retakeTheCity
      putQuestIntoPlay firstStage
      pure $ AssaultOnOsgiliath $ attrs & encounterDeckL .~ encounterDeck
    _ -> AssaultOnOsgiliath <$> runMessage msg attrs
