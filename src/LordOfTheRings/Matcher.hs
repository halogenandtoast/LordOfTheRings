{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Matcher (module LordOfTheRings.Matcher, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.Types
import LordOfTheRings.Attachment.Types
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.Query as X
import LordOfTheRings.Enemy.Types
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Hero.Types
import LordOfTheRings.Id
import LordOfTheRings.Location.Types
import LordOfTheRings.Matcher.Types as X
import LordOfTheRings.Placement
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Target

instance Query PlayerMatcher where
  type QueryElement PlayerMatcher = PlayerId
  select matcher = do
    players <- getsGame (toList . gamePlayers)
    map toId <$> filterM (matchPlayer matcher) players

matchPlayer :: (HasGame m) => PlayerMatcher -> Player -> m Bool
matchPlayer matcher player = case matcher of
  PlayerWithId pid -> pure $ player.id == pid
  FirstPlayer -> (== player.id) <$> getsGame gameFirstPlayer
  ActivePlayer -> (== player.id) <$> getsGame gameActivePlayer
  AnyPlayer -> pure True
  PlayerMatchAll ms -> allM (`matchPlayer` player) ms

instance Query HeroMatcher where
  type QueryElement HeroMatcher = HeroId
  select matcher = do
    heroes <- getsGame (toList . entitiesHeroes . gameEntities)
    map toId <$> filterM (matchHero matcher) heroes

matchHero :: (HasGame m) => HeroMatcher -> Hero -> m Bool
matchHero matcher hero = case matcher of
  HeroWithId hid -> pure $ hero.id == hid
  HeroControlledBy pm -> elem hero.owner <$> select pm
  HeroWithTitle name -> pure $ hero.name == name
  ReadyHero -> pure $ not (heroExhausted $ toAttrs hero)
  ExhaustedHero -> pure $ heroExhausted (toAttrs hero)
  CommittedHero -> pure $ heroCommitted (toAttrs hero)
  AttackingHero -> pure $ heroAttacking (toAttrs hero)
  AnyHero -> pure True
  NotHero m -> not <$> matchHero m hero
  HeroMatchAll ms -> allM (`matchHero` hero) ms

instance Query AllyMatcher where
  type QueryElement AllyMatcher = AllyId
  select matcher = do
    allies <- getsGame (toList . entitiesAllies . gameEntities)
    map toId <$> filterM (matchAlly matcher) allies

matchAlly :: (HasGame m) => AllyMatcher -> Ally -> m Bool
matchAlly matcher ally = case matcher of
  AllyWithId aid -> pure $ ally.id == aid
  AllyControlledBy pm -> case placementController ally.placement of
    Just pid -> elem pid <$> select pm
    Nothing -> pure False
  AllyWithTitle name -> pure $ ally.name == name
  ReadyAlly -> pure $ not (allyExhausted $ toAttrs ally)
  ExhaustedAlly -> pure $ allyExhausted (toAttrs ally)
  CommittedAlly -> pure $ allyCommitted (toAttrs ally)
  AttackingAlly -> pure $ allyAttacking (toAttrs ally)
  AnyAlly -> pure True
  NotAlly m -> not <$> matchAlly m ally
  AllyMatchAll ms -> allM (`matchAlly` ally) ms

instance Query EnemyMatcher where
  type QueryElement EnemyMatcher = EnemyId
  select matcher = do
    enemies <- getsGame (toList . entitiesEnemies . gameEntities)
    map toId <$> filterM (matchEnemy matcher) enemies

matchEnemy :: (HasGame m) => EnemyMatcher -> Enemy -> m Bool
matchEnemy matcher enemy = case matcher of
  EnemyWithId eid -> pure $ enemy.id == eid
  EnemyInStagingArea -> pure $ enemy.placement == StagingArea
  EnemyEngagedWith pm -> case enemy.placement of
    EngagedWith pid -> elem pid <$> select pm
    _ -> pure False
  AnyEnemy -> pure True
  NotEnemy m -> not <$> matchEnemy m enemy
  EnemyMatchAll ms -> allM (`matchEnemy` enemy) ms

instance Query LocationMatcher where
  type QueryElement LocationMatcher = LocationId
  select matcher = do
    locations <- getsGame (toList . entitiesLocations . gameEntities)
    map toId <$> filterM (matchLocation matcher) locations

matchLocation :: (HasGame m) => LocationMatcher -> Location -> m Bool
matchLocation matcher location = case matcher of
  LocationWithId lid -> pure $ location.id == lid
  LocationInStagingArea -> pure $ location.placement == StagingArea
  IsActiveLocation -> pure $ location.placement == ActiveLocation
  AnyLocation -> pure True
  NotLocation m -> not <$> matchLocation m location
  LocationMatchAll ms -> allM (`matchLocation` location) ms

getPlayer :: (HasCallStack, HasGame m) => PlayerId -> m Player
getPlayer pid = fromJustNote ("missing player " <> show pid) . Map.lookup pid <$> getsGame gamePlayers

getHero :: (HasCallStack, HasGame m) => HeroId -> m Hero
getHero hid = fromJustNote ("missing hero " <> show hid) . Map.lookup hid <$> getsGame (entitiesHeroes . gameEntities)

getAlly :: (HasCallStack, HasGame m) => AllyId -> m Ally
getAlly aid = fromJustNote ("missing ally " <> show aid) . Map.lookup aid <$> getsGame (entitiesAllies . gameEntities)

getEnemy :: (HasCallStack, HasGame m) => EnemyId -> m Enemy
getEnemy eid = fromJustNote ("missing enemy " <> show eid) . Map.lookup eid <$> getsGame (entitiesEnemies . gameEntities)

getLocation :: (HasCallStack, HasGame m) => LocationId -> m Location
getLocation lid = fromJustNote ("missing location " <> show lid) . Map.lookup lid <$> getsGame (entitiesLocations . gameEntities)

getLead :: (HasGame m) => m PlayerId
getLead = getsGame gameFirstPlayer

-- | Players in turn order, starting with the first player.
getPlayerOrder :: (HasGame m) => m [PlayerId]
getPlayerOrder = do
  order <- getsGame gamePlayerOrder
  firstPlayer <- getsGame gameFirstPlayer
  let (before, after) = break (== firstPlayer) order
  pure $ after <> before

-- | Heroes and allies matching the given matchers, as targets.
selectCharacters :: (HasGame m) => HeroMatcher -> AllyMatcher -> m [Target]
selectCharacters heroes allies = (<>) <$> (map toTarget <$> select heroes) <*> (map toTarget <$> select allies)

charactersControlledBy :: (HasGame m) => PlayerId -> m [Target]
charactersControlledBy pid = selectCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)

getAttachmentsOn :: (HasGame m) => Target -> m [Target]
getAttachmentsOn host =
  getsGame (map toTarget . filter ((== AttachedTo host) . (.placement)) . toList . entitiesAttachments . gameEntities)

getAttachmentsControlledBy :: (HasGame m) => PlayerId -> m [Target]
getAttachmentsControlledBy pid =
  getsGame (map toTarget . filter ((== pid) . (.owner) . toAttrs) . toList . entitiesAttachments . gameEntities)
