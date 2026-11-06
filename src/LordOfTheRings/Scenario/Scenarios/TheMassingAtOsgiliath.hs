module LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath (theMassingAtOsgiliath) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheMassingAtOsgiliath = TheMassingAtOsgiliath ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMassingAtOsgiliath :: Scenario
theMassingAtOsgiliath = Scenario $ scenario TheMassingAtOsgiliath "the-massing-at-osgiliath" "The Massing at Osgiliath"

instance RunMessage TheMassingAtOsgiliath where
  runMessage msg (TheMassingAtOsgiliath attrs) = case msg of
    DoStep 1 Setup -> do
      deck <- gatherEncounterSets attrs [EncounterSet.TheMassingAtOsgiliath]
      putQuestIntoPlay =<< genEncounterCard Quests.beyondExpectations
      later <- traverse genEncounterCard [Quests.throughTheRuinsOsgiliath, Quests.anduinCrossing, Quests.raceToMinasTirith]
      pure $ TheMassingAtOsgiliath $ attrs & encounterDeckL .~ deck & questDeckL .~ later
    _ -> TheMassingAtOsgiliath <$> runMessage msg attrs
