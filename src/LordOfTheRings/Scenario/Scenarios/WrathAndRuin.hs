module LordOfTheRings.Scenario.Scenarios.WrathAndRuin (wrathAndRuin) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype WrathAndRuin = WrathAndRuin ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wrathAndRuin :: Scenario
wrathAndRuin = Scenario $ scenario WrathAndRuin "wrath-and-ruin" "Wrath and Ruin"

{- | Wrath and Ruin, Easterling Raiders and City of Rhûn make up the encounter deck (MEC78
rules). Stage 1A takes Thane Ulchor, Gate of Dorwinion and a Makeshift Fortification from
it as it sets up. The players take control of the locations they explore, which then sit
in their play areas (see "LordOfTheRings.Helpers.ControlledLocation").
-}
instance RunMessage WrathAndRuin where
  runMessage msg (WrathAndRuin attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.WrathAndRuin, EncounterSet.EasterlingRaiders, EncounterSet.CityOfRhun]
      firstStage <- genEncounterCard Quests.insideTheWalls
      laterStages <- traverse genEncounterCard [Quests.driveThemOut]
      putQuestIntoPlay firstStage
      pure $ WrathAndRuin $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> WrathAndRuin <$> runMessage msg attrs
