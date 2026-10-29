module LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen (theFordsOfIsen) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen.Helpers (runFordsOfIsenStep)

newtype TheFordsOfIsen = TheFordsOfIsen ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theFordsOfIsen :: Scenario
theFordsOfIsen = Scenario $ scenario TheFordsOfIsen "the-fords-of-isen" "The Fords of Isen"

-- | The Fords of Isen, Dunland Raiders and Dunland Warriors make up the encounter deck.
-- The rules sheet bars the Gríma hero from this scenario: see 'barredHeroes'.
instance RunMessage TheFordsOfIsen where
  runMessage msg (TheFordsOfIsen attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [EncounterSet.TheFordsOfIsen, EncounterSet.DunlandRaiders, EncounterSet.DunlandWarriors]
      firstStage <- genEncounterCard Quests.fightAtTheFords
      laterStages <- traverse genEncounterCard [Quests.dunlendingAttack, Quests.holdTheFords]
      putQuestIntoPlay firstStage
      pure $ TheFordsOfIsen $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    ScenarioSpecific key v -> do
      runFordsOfIsenStep key v
      pure $ TheFordsOfIsen attrs
    _ -> TheFordsOfIsen <$> runMessage msg attrs
