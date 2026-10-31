module LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater (theWatcherInTheWater) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (CannotBeOptionallyEngagedBy, CannotEngagePlayer))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Trait (Trait (Tentacle))
import LordOfTheRings.Window (Window (..), WindowType (EnemyDestroyed))

newtype TheWatcherInTheWater = TheWatcherInTheWater ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWatcherInTheWater :: Scenario
theWatcherInTheWater =
  Scenario
    $ scenario
      (TheWatcherInTheWater . (nightmareDeckL ?~ nightmare))
      "the-watcher-in-the-water"
      "The Watcher in the Water"
 where
  -- the nightmare set's The Watcher takes the standard one's place, set aside the same
  nightmare =
    nightmareDeck
      Nightmare.theWatcherInTheWaterNightmare
      EncounterSet.TheWatcherInTheWaterNightmare
      [ (Enemies.blackUruks, 4)
      , (Enemies.mountainWarg, 3)
      , (Locations.wargLair, 2)
      , (Locations.makeshiftPassage, 2)
      , (Enemies.theWatcher, 1)
      ]

theWatchers :: EnemyMatcher
theWatchers = EnemyWithCardCode (toCardCode Enemies.theWatcherNightmare)

-- | Nightmare: "While Doors of Durin is in play, The Watcher cannot be engaged."
instance HasModifiersFor TheWatcherInTheWater where
  getModifiersFor (TheWatcherInTheWater a) = when a.nightmare do
    doors <- selectAny (LocationWithCardCode $ toCardCode Locations.doorsOfDurin)
    when doors do
      players <- getRemainingPlayers
      watchers <- select theWatchers
      for_ watchers \w -> modified a w $ CannotBeOptionallyEngagedBy AnyPlayer : map CannotEngagePlayer players

instance RunMessage TheWatcherInTheWater where
  runMessage msg (TheWatcherInTheWater attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.TheWatcherInTheWater, EncounterSet.MistyMountains]
      firstStage <- genEncounterCard Quests.toTheWestDoor
      laterStages <- traverse genEncounterCard [Quests.theSeethingLake]
      putQuestIntoPlay firstStage
      pure $ TheWatcherInTheWater $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Forced: When a Tentacle enemy is destroyed, deal 1 damage to The Watcher
    -- if it is in play." The Seething Lake holds off the win.
    CheckWindows ws | attrs.nightmare -> do
      let tentacles = [c | Window After (EnemyDestroyed c) <- ws, cardMatches (CardWithTrait Tentacle) c]
      watchers <- select theWatchers
      for_ tentacles \_ -> for_ watchers \w -> dealDamage ScenarioSource w 1
      TheWatcherInTheWater <$> runMessage msg attrs
    _ -> TheWatcherInTheWater <$> runMessage msg attrs
