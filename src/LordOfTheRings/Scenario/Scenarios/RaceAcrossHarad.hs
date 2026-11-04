{- | Race Across Harad. The Orcs race the players to the River Harnen with a quest deck of
their own, stages 1C to 4C, in the Orcs' area: see
"LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers". The scenario keeps that
deck, plays the text of the Orcs' stages, has the Orcs place progress at the end of each
quest phase, puts the Orcs' area in the staging area while the players are at the Orcs'
stage, and ends the game as the stages say.
-}
module LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad (raceAcrossHarad) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (OrcsArea, StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers
import LordOfTheRings.Source (Source (ScenarioSource))
import LordOfTheRings.Timing (Timing (When))
import LordOfTheRings.Window (Window (..), WindowType (PhaseEnds))
import LordOfTheRings.Classes.HasGame (getsGame)

newtype RaceAcrossHarad = RaceAcrossHarad ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

raceAcrossHarad :: Scenario
raceAcrossHarad = Scenario $ scenario RaceAcrossHarad "race-across-harad" "Race Across Harad"

{- | While the Orcs are at a different quest stage than the players, cards in the Orcs'
area are immune to player card effects and cannot leave the Orcs' area (MEC57 rules).
Stage 1D's "Uruk Chieftain cannot leave the Orcs' area" and stages 2D and 3D's "while the
players are at stage 3B (4B)" are this: the players are past the Orcs' stage then.

Stage 3D: While the players are at stage 4B, each enemy at this stage gets +1 threat.
Stage 4D: Uruk Chieftain cannot take damage while River Harnen is in the staging area.
-}
instance HasModifiersFor RaceAcrossHarad where
  getModifiersFor (RaceAcrossHarad a) = do
    together <- atOrcsStage
    unless together do
      apart <- getsGame \g ->
        [toTarget e | e <- toList g.entities.enemies, e.placement == OrcsArea]
          <> [toTarget l | l <- toList g.entities.locations, l.placement == OrcsArea]
      for_ apart \t -> modified a t [ImmuneToPlayerCardEffects, CannotLeaveStagingArea]
    stage <- getOrcsStage
    players <- getPlayersStage
    when (stage == 3 && players == 4) do
      area <- orcsArea
      for_ [t | t@(EnemyTarget _) <- area] \t -> modified a t [ThreatModifier 1]
    when (stage == 4) do
      river <- selectAny (LocationWithCardCode (toCardCode Locations.riverHarnen) <> LocationInStagingArea)
      when river $ getUrukChieftains >>= traverse_ \e -> modified a e [CannotTakeDamage]

{- | The encounter deck is built from the Race Across Harad, Desert Sands and Mordor Orcs
sets. Stage 1A's setup sets River Harnen and 1 Uruk Warg-rider aside; the Tamed Mumaks and
Uruk Chieftain wait set aside for it too.
-}
instance RunMessage RaceAcrossHarad where
  runMessage msg (RaceAcrossHarad attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.RaceAcrossHarad, EncounterSet.DesertSands, EncounterSet.MordorOrcs]
      let is def = cardMatches (cardIs def)
          (riders, rest) = partition (is Enemies.urukWargRider) cards
          (asides, encounterDeck) = partition (\c -> any (`is` c) [Locations.riverHarnen, Objectives.tamedMumak, Enemies.urukChieftain]) rest
      firstStage <- genEncounterCard Quests.settingOut
      laterStages <- traverse genEncounterCard [Quests.racingNorth, Quests.huntedInHarad, Quests.theFordOfHarnen]
      putQuestIntoPlay firstStage
      pure
        $ RaceAcrossHarad
        $ attrs
        & encounterDeckL .~ (drop 1 riders <> encounterDeck)
        & setAsideL .~ (take 1 riders <> asides)
        & questDeckL .~ laterStages
    -- stage 1A: Each player takes control of a Tamed Mumak. Remove each remaining Tamed
    -- Mumak from the game. Create the Orcs' area with a second quest deck consisting of
    -- stage 1C, 2C, 3C, and 4C as well as Uruk Chieftain.
    ScenarioSpecific "orcs.prepare" _ -> do
      stages <- traverse genEncounterCard [Quests.settingOutOrcs, Quests.racingNorthOrcs, Quests.huntedInHaradOrcs, Quests.theFordOfHarnenOrcs]
      players <- getRemainingPlayers
      let mumaks = filter (cardMatches $ cardIs Objectives.tamedMumak) attrs.scenarioSetAside
          chieftains = filter (cardMatches $ cardIs Enemies.urukChieftain) attrs.scenarioSetAside
      for_ (zip players mumaks) (uncurry putObjectiveAllyIntoPlayUnder)
      for_ chieftains addCardToOrcsArea
      -- the players are at stage 1B only as it advances them to 2A
      let meta' = Meta {orcsStage = headMay stages, orcsDeck = drop 1 stages, orcsProgress = 0, orcsJoined = False, orcsAreaCards = []}
      pure $ withMeta meta' (attrs & setAsideL %~ filter (`notElem` (mumaks <> chieftains)))
    -- each of the Orcs' stages: Forced: At the end of the quest phase, discard the top card
    -- of the encounter deck. Place X progress on this stage, where X is the discarded
    -- card's threat plus the total threat in the Orcs' area (stage 4D: plus the total
    -- threat of each engaged enemy). Stages 2D and 3D return Uruk Chieftain to the Orcs'
    -- area first.
    CheckWindows ws | Just stage <- meta.orcsStage, Window When (PhaseEnds QuestPhase) `elem` ws -> do
      when (stageOf stage `elem` [2, 3]) do
        area <- orcsArea
        chieftains <- getUrukChieftains
        for_ [c | c <- chieftains, toTarget c `notElem` area] addToOrcsArea
      discardTopOfEncounterDeck attrs 1
      RaceAcrossHarad <$> runMessage msg attrs
    DiscardedTopOfEncounterDeck ScenarioSource ScenarioTarget discarded | Just stage <- meta.orcsStage -> do
      x <-
        if stageOf stage == 4
          then sum <$> (traverse getEnemyThreat =<< select (EnemyEngagedWith AnyPlayer))
          else orcsAreaThreat
      placeOrcsProgress $ x + sum (mapMaybe (cdThreat . toCardDef) discarded)
      RaceAcrossHarad <$> runMessage msg attrs
    ScenarioSpecific "orcs.progress" (parseMaybe parseJSON -> Just n) -> case meta.orcsStage of
      Just stage | meta.orcsProgress + n >= fromMaybe 0 (cdQuestPoints $ toCardDef stage) -> do
        players <- getPlayersStage
        case meta.orcsDeck of
          -- stages 2D and 3D: If you are at this stage when it is defeated, you lose the
          -- game. Stage 4D: If the Orcs defeat this stage, the Orcs have caught you.
          next : rest | stageOf stage /= players -> do
            push $ ScenarioSpecific "orcs.advanced" (toJSON next)
            -- stages 2C and 3C: When Revealed: If the players have already defeated stage
            -- 2B (3B), discard cards from the top of the encounter deck until an Orc enemy
            -- is discarded. Add that enemy to the Orcs' area.
            when (players > stageOf next) do
              let n' = maybe (length attrs.encounterDeck) (+ 1) (findIndex (cardMatches orcEnemy) attrs.encounterDeck)
              discardTopOfEncounterDeckFor attrs (CardIdTarget next.id) n'
            push $ ScenarioSpecific "orcs.sync" (toJSON players)
            pure $ withMeta meta {orcsStage = Just next, orcsDeck = rest, orcsProgress = 0} attrs
          _ -> do
            gameOver Lost
            pure $ RaceAcrossHarad attrs
      Just _ -> pure $ withMeta meta {orcsProgress = meta.orcsProgress + n} attrs
      Nothing -> pure $ RaceAcrossHarad attrs
    DiscardedTopOfEncounterDeck ScenarioSource (CardIdTarget _) discarded -> do
      addOrcToOrcsArea discarded
      RaceAcrossHarad <$> runMessage msg attrs
    -- the cards in the Orcs' area are in the staging area while the players are at the
    -- Orcs' stage, and go back as they move on
    ScenarioSpecific "orcs.sync" (parseMaybe parseJSON -> Just players) -> do
      let orcs = maybe 0 stageOf meta.orcsStage
          together = orcs > 0 && orcs == (players :: Int)
      area <- orcsArea
      if
        | together && not meta.orcsJoined -> do
            push $ ScenarioSpecific "orcs.join" Null
            for_ area (`placeIn` StagingArea)
            pure $ withMeta meta {orcsJoined = True, orcsAreaCards = area} attrs
        | not together && meta.orcsJoined -> do
            push $ ScenarioSpecific "orcs.split" Null
            for_ area (`placeIn` OrcsArea)
            pure $ withMeta meta {orcsJoined = False, orcsAreaCards = []} attrs
        | otherwise -> pure $ RaceAcrossHarad attrs
    ScenarioSpecific "orcs.add" (parseMaybe parseJSON -> Just target)
      | meta.orcsJoined -> do
          placeIn target StagingArea
          pure $ withMeta meta {orcsAreaCards = ordNub (target : meta.orcsAreaCards)} attrs
      | otherwise -> do
          placeIn target OrcsArea
          pure $ RaceAcrossHarad attrs
    AdvanceToQuest card -> do
      attrs' <- runMessage msg attrs
      push $ ScenarioSpecific "orcs.sync" (toJSON $ stageOf card)
      pure $ RaceAcrossHarad attrs'
    -- a card of the Orcs' area that leaves the staging area leaves the Orcs' area
    EngageEnemy _ eid -> leaves (EnemyTarget eid)
    PlaceEnemy eid p | p /= StagingArea -> leaves (EnemyTarget eid)
    PlaceLocation lid p | p /= StagingArea -> leaves (LocationTarget lid)
    _ -> RaceAcrossHarad <$> runMessage msg attrs
   where
    meta = getMeta attrs
    withMeta m a = RaceAcrossHarad $ a & metaL .~ toJSON m
    leaves t = RaceAcrossHarad <$> runMessage msg (attrs & metaL .~ toJSON meta {orcsAreaCards = filter (/= t) meta.orcsAreaCards})
    placeIn t p = case t of
      EnemyTarget eid -> placeEnemy eid p
      LocationTarget lid -> placeLocation lid p
      _ -> pure ()
