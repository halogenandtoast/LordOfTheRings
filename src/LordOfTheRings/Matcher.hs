{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Matcher (module LordOfTheRings.Matcher, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.Types
import LordOfTheRings.Attachment.Types
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.Query as X
import LordOfTheRings.Enemy.Types
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Hero.Types
import LordOfTheRings.Id
import LordOfTheRings.Location.Types
import LordOfTheRings.Objective.Types (Objective)
import LordOfTheRings.Question
import LordOfTheRings.Matcher.Types as X
import LordOfTheRings.Modifier
import LordOfTheRings.Placement
import LordOfTheRings.Player.Types
import LordOfTheRings.I18n
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Trait
import LordOfTheRings.Treachery.Types (Treachery)

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
  NotPlayer m -> not <$> matchPlayer m player
  PlayerMatchAll ms -> allM (`matchPlayer` player) ms

instance Query HeroMatcher where
  type QueryElement HeroMatcher = HeroId
  select matcher = do
    heroes <- getsGame (toList . entitiesHeroes . gameEntities)
    map toId <$> filterM (matchHero matcher) heroes

matchHero :: (HasGame m) => HeroMatcher -> Hero -> m Bool
matchHero matcher hero = case matcher of
  HeroWithId hid -> pure $ hero.id == hid
  -- a prisoner is face down: no one controls it until it is rescued
  HeroControlledBy pm -> if heroPrisoner (toAttrs hero) then pure False else elem hero.owner <$> select pm
  HeroWithTitle name -> pure $ hero.name == name
  HeroWithTrait trait -> hasTrait trait (toTarget hero) (toCardDef hero)
  HeroWithSphere sphere -> pure $ hero.sphere == sphere
  HeroWithKeyword k -> pure $ k `member` cdKeywords (toCardDef hero)
  ReadyHero -> pure $ not (heroExhausted $ toAttrs hero)
  ExhaustedHero -> pure $ heroExhausted (toAttrs hero)
  CommittedHero -> pure $ heroCommitted (toAttrs hero)
  AttackingHero -> pure $ heroAttacking (toAttrs hero)
  PrisonerHero -> pure $ heroPrisoner (toAttrs hero)
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
  AllyWithTrait trait -> hasTrait trait (toTarget ally) (toCardDef ally)
  AllyWithSphere sphere -> pure $ cdSphere (toCardDef ally) == Just sphere
  AllyWithKeyword k -> pure $ k `member` cdKeywords (toCardDef ally)
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
  EnemyWithTrait trait -> hasTrait trait (toTarget enemy) (toCardDef enemy)
  EnemyWithCardCode code -> pure $ toCardCode enemy == code
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

-- | A unique card cannot enter play while a card with its title is in play.
passesUnique :: (HasGame m) => Card -> m Bool
passesUnique card
  | card.unique =
      andM
        [ selectNone $ AllyWithTitle card.name
        , selectNone $ HeroWithTitle card.name
        , getsGame $ all ((/= card.name) . cdName . toCardDef) . toList . (.entities.attachments)
        ]
  | otherwise = pure True

-- | A printed trait, or one a modifier grants.
hasTrait :: (HasGame m) => Trait -> Target -> CardDef -> m Bool
hasTrait trait target def
  | trait `member` cdCardTraits def = pure True
  | otherwise = elem (GainTrait trait) . map modifierType . Map.findWithDefault [] target <$> getsGame gameModifiers

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
getAttachmentsControlledBy pid = do
  es <- getsGame gameEntities
  heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
  -- a claimed objective counts as an attachment of the hero it is attached to
  pure
    $ [toTarget a | a <- toList es.attachments, (toAttrs a).owner == pid]
    <> [toTarget o | o <- toList es.objectives, objectiveOn o `elem` map Just heroes]
 where
  objectiveOn o = case o.placement of
    AttachedTo host -> Just host
    _ -> Nothing

-- | Players still in the game, in turn order from the first player.
getRemainingPlayers :: (HasGame m) => m [PlayerId]
getRemainingPlayers = filterM (fmap (not . (.eliminated)) . getPlayer) =<< getPlayerOrder

-- | The damage on a character, or 0 for anything else.
getDamage :: (HasGame m) => Target -> m Int
getDamage = \case
  HeroTarget hid -> maybe 0 (.damage) . Map.lookup hid <$> getsGame (entitiesHeroes . gameEntities)
  AllyTarget aid -> maybe 0 (.damage) . Map.lookup aid <$> getsGame (entitiesAllies . gameEntities)
  _ -> pure 0

-- | Characters with damage to heal.
damagedCharacters :: (HasGame m) => HeroMatcher -> AllyMatcher -> m [Target]
damagedCharacters heroes allies = filterM (fmap (> 0) . getDamage) =<< selectCharacters heroes allies

-- | Condition attachments in play: attachments with the Condition trait, and encounter
-- cards attached to something, which in the core set all count as Conditions.
getConditionAttachments :: (HasGame m) => m [Target]
getConditionAttachments = do
  es <- getsGame gameEntities
  conditions <- filterM (\a -> hasTrait Condition (toTarget a) (toCardDef a)) (toList es.attachments)
  pure $ map toTarget conditions <> [toTarget t | t <- toList es.treacheries, isAttached t.placement]
 where
  isAttached = \case
    AttachedTo _ -> True
    _ -> False

-- | The quest card in play, as a target.
getCurrentQuest :: (HasGame m) => m (Maybe Target)
getCurrentQuest = getsGame (fmap toTarget . headMay . toList . entitiesQuests . gameEntities)
