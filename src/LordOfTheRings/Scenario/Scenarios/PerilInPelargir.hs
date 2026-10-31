module LordOfTheRings.Scenario.Scenarios.PerilInPelargir (perilInPelargir) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyDefense)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (AnyEnemy), getLead, select)
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (CombatPhase, RefreshPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (enemyProgress, getScroll, scrollEnemy)
import LordOfTheRings.Token (Token (Progress))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Window (..), WindowType (PhaseBegins, PhaseEnds))

newtype PerilInPelargir = PerilInPelargir ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

perilInPelargir :: Scenario
perilInPelargir =
  Scenario
    $ scenario (PerilInPelargir . (nightmareDeckL ?~ nightmare)) "peril-in-pelargir" "Peril in Pelargir"
 where
  nightmare =
    nightmareDeck
      Nightmare.perilInPelargirNightmare
      EncounterSet.PerilInPelargirNightmare
      [ (Enemies.harborThug, 1)
      , (Locations.harborStorehouse, 2)
      , (Treacheries.collateralDamage, 4)
      , (Enemies.pickpocket, 3)
      ]

instance RunMessage PerilInPelargir where
  runMessage msg (PerilInPelargir attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.PerilInPelargir, EncounterSet.StreetsOfGondor, EncounterSet.Brigands]
      firstStage <- genEncounterCard Quests.theLeapingFishQuest
      laterStages <- traverse genEncounterCard [Quests.fightingInTheStreets, Quests.escapeToTheQuays]
      putQuestIntoPlay firstStage
      pure $ PerilInPelargir $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Nightmare: "Forced: At the end of the combat phase, if Alcaron's Scroll is in the
    -- staging area, attach it to the enemy in play with the highest defense."
    CheckWindows ws | attrs.nightmare, any ((== PhaseEnds CombatPhase) . windowType) ws -> do
      scroll <- getScroll
      staged <- getsGame \g -> maybe False ((== StagingArea) . (.placement)) (scroll >>= (`Map.lookup` g.entities.objectives))
      when staged $ for_ scroll \oid -> do
        enemies <- select AnyEnemy
        defenses <- traverse (\e -> (e,) <$> getEnemyDefense e) enemies
        let highest = foldr (max . snd) minBound defenses
        lead <- getLead
        chooseOrRunOneM lead do
          questionLabeled "perilInPelargirNightmare.attachScroll"
          targets [e | (e, d) <- defenses, d == highest] (attachObjective oid)
      PerilInPelargir <$> runMessage msg attrs
    -- Nightmare: "Forced: At the beginning of the refresh phase, if Alcaron's Scroll is
    -- attached to an enemy, place 1 progress token on that enemy."
    CheckWindows ws | attrs.nightmare, any ((== PhaseBegins RefreshPhase) . windowType) ws -> do
      scrollEnemy >>= traverse_ \e -> placeProgress attrs e 1
      PerilInPelargir <$> runMessage msg attrs
    -- Nightmare: "If at any point there are 3 or more progress tokens on an enemy, that
    -- enemy has escaped with the scroll, and the players lose the game." The game still
    -- reads the enemy as it was before the tokens.
    PlaceTokens _ (EnemyTarget eid) Progress n | attrs.nightmare -> do
      progress <- enemyProgress eid
      when (progress + n >= 3) $ gameOver Lost
      PerilInPelargir <$> runMessage msg attrs
    _ -> PerilInPelargir <$> runMessage msg attrs
