module LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat (theDungeonsOfCirithGurat) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted

newtype TheDungeonsOfCirithGurat = TheDungeonsOfCirithGurat ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDungeonsOfCirithGurat :: Scenario
theDungeonsOfCirithGurat =
  Scenario $ scenario TheDungeonsOfCirithGurat "the-dungeons-of-cirith-gurat" "The Dungeons of Cirith Gurat"

{- | The encounter deck is built from The Dungeons of Cirith Gurat, Mordor Orcs and Harad
Territory (MEC60). Uruk Chieftain, The Captives of Cirith Gurat, Abaan and 1 copy of
Dungeon Cell are set aside for the first stage's setup, which puts the last three into
play and leaves Uruk Chieftain out of play until Fight Your Way Out.
The players lose if Abaan leaves play (Abaan), and win as they defeat stage 3.
-}
instance RunMessage TheDungeonsOfCirithGurat where
  runMessage msg (TheDungeonsOfCirithGurat attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheDungeonsOfCirithGurat, EncounterSet.MordorOrcs, EncounterSet.HaradTerritory]
      let is def = (== toCardCode def) . toCardCode
          (singles, rest) = partition (\c -> any (`is` c) [Enemies.urukChieftain, Objectives.theCaptivesOfCirithGurat, Objectives.abaan]) cards
          (cells, encounterDeck) = case break (is Locations.dungeonCell) rest of
            (before, cell : after) -> ([cell], before <> after)
            _ -> ([], rest)
      firstStage <- genEncounterCard Quests.rescueThePrisoners
      laterStages <- traverse genEncounterCard [Quests.sneakPastTheGuards, Quests.fightYourWayOut]
      putQuestIntoPlay firstStage
      pure
        $ TheDungeonsOfCirithGurat
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ singles <> cells
    _ -> TheDungeonsOfCirithGurat <$> runMessage msg attrs
