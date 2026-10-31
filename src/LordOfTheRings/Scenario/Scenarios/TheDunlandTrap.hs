module LordOfTheRings.Scenario.Scenarios.TheDunlandTrap (theDunlandTrap) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheDunlandTrap = TheDunlandTrap ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDunlandTrap :: Scenario
theDunlandTrap =
  Scenario
    $ scenario (TheDunlandTrap . (nightmareDeckL ?~ nightmare)) "the-dunland-trap" "The Dunland Trap"
 where
  -- its setup card has no text of its own beyond the deck
  nightmare =
    nightmareDeck
      Nightmare.theDunlandTrapNightmare
      EncounterSet.TheDunlandTrapNightmare
      [ (Enemies.boarClanWarrior, 4)
      , (Locations.hithaeglirStream, 4)
      , (Treacheries.dunlendingAmbush, 1)
      , (Treacheries.frenziedAttack, 1)
      , (Treacheries.lowOnProvisions, 1)
      ]

-- | The encounter deck is built from The Dunland Trap, Dunland Warriors and Weary
-- Travelers.
instance RunMessage TheDunlandTrap where
  runMessage msg (TheDunlandTrap attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheDunlandTrap, EncounterSet.DunlandWarriors, EncounterSet.WearyTravelers]
      firstStage <- genEncounterCard Quests.theRoadToTharbad
      laterStages <- traverse genEncounterCard [Quests.aWellLaidTrap, Quests.noWayOut]
      putQuestIntoPlay firstStage
      pure $ TheDunlandTrap $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    _ -> TheDunlandTrap <$> runMessage msg attrs
