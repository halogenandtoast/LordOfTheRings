module LordOfTheRings.Scenario.Scenarios.RoadToRivendell (roadToRivendell) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Keyword (Keyword (Ambush))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithId, EnemyWithKeyword), selectAny)
import LordOfTheRings.Modifier (ModifierType (EngagedEnemiesAddThreat))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (After))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (EnemyEngaged))

newtype RoadToRivendell = RoadToRivendell ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

roadToRivendell :: Scenario
roadToRivendell =
  Scenario
    $ scenario (RoadToRivendell . (nightmareDeckL ?~ nightmare)) "road-to-rivendell" "Road to Rivendell"
 where
  -- Ambush in the Night takes Orc Outpost's place as stage 2
  nightmare =
    nightmareDeck
      Nightmare.roadToRivendellNightmare
      EncounterSet.RoadToRivendellNightmare
      [ (Enemies.blackUruks, 4)
      , (Enemies.mountainWarg, 3)
      , (Treacheries.bitterWind, 3)
      , (Locations.wargLair, 2)
      , (Enemies.goblinSpearman, 5)
      , (Enemies.goblinArcher, 2)
      , (Locations.plunderedArmoury, 2)
      , (Locations.ruinedRoad, 2)
      , (Locations.barrenHills, 2)
      , (Enemies.goblinTaskmaster, 1)
      ]

instance RunMessage RoadToRivendell where
  runMessage msg (RoadToRivendell attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets
          attrs
          [EncounterSet.RoadToRivendell, EncounterSet.MistyMountains, EncounterSet.PlunderingGoblins]
      firstStage <- genEncounterCard Quests.alongTheMistyMountains
      laterStages <-
        traverse
          genEncounterCard
          [nightmareOr attrs Quests.orcOutpost Quests.ambushInTheNight, Quests.approachingRivendell]
      putQuestIntoPlay firstStage
      pure $ RoadToRivendell $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Forced: When an enemy with the ambush keyword engages a player during the
    -- quest phase, it contributes its threat to the total threat in the staging area until
    -- the end of the phase."
    CheckWindows ws | attrs.nightmare -> do
      phase <- getsGame gamePhase
      when (phase == QuestPhase) do
        for_ [eid | Window After (EnemyEngaged eid _) <- ws] \eid -> do
          ambush <- selectAny (EnemyWithId eid <> EnemyWithKeyword Ambush)
          when ambush $ phaseModifier ScenarioSource GameTarget (EngagedEnemiesAddThreat $ EnemyWithId eid)
      RoadToRivendell <$> runMessage msg attrs
    _ -> RoadToRivendell <$> runMessage msg attrs
