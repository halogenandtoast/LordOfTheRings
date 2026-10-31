module LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum (theBattleOfCarnDum) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheBattleOfCarnDum = TheBattleOfCarnDum ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBattleOfCarnDum :: Scenario
theBattleOfCarnDum = Scenario $ scenario TheBattleOfCarnDum "the-battle-of-carn-dum" "The Battle of Carn Dûm"

-- | The encounter deck is built from The Battle of Carn Dûm, Angmar Orcs and Dark Sorcery.
instance RunMessage TheBattleOfCarnDum where
  runMessage msg (TheBattleOfCarnDum attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheBattleOfCarnDum, EncounterSet.AngmarOrcs, EncounterSet.DarkSorcery]
      firstStage <- genEncounterCard Quests.theClutchesOfCarnDum
      laterStages <- traverse genEncounterCard [Quests.midwintersCrux]
      putQuestIntoPlay firstStage
      pure $ TheBattleOfCarnDum $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheBattleOfCarnDum <$> runMessage msg attrs
