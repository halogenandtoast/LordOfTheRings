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
import LordOfTheRings.Keyword (Keyword)
import LordOfTheRings.Modifier
import LordOfTheRings.Placement
import LordOfTheRings.Player.Types
import LordOfTheRings.I18n
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types ()
import LordOfTheRings.Target
import LordOfTheRings.Trait
import LordOfTheRings.Token (Token (Resource), countTokens)
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
  PlayerWithThreatAtLeast n -> pure $ player.threat >= n
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
  HeroWithKeyword k -> hasKeyword k (toTarget hero) (toCardDef hero)
  ReadyHero -> pure $ not (heroExhausted $ toAttrs hero)
  ExhaustedHero -> pure $ heroExhausted (toAttrs hero)
  CommittedHero -> pure $ heroCommitted (toAttrs hero)
  AttackingHero -> pure $ heroAttacking (toAttrs hero)
  PrisonerHero -> pure $ heroPrisoner (toAttrs hero)
  HeroWithAttached trait -> notNull <$> attachedWithTrait trait (toTarget hero)
  HeroWithResources -> pure $ hero.resources > 0
  AnyHero -> pure True
  NotHero m -> not <$> matchHero m hero
  HeroMatchAll ms -> allM (`matchHero` hero) ms
  HeroMatchAny ms -> anyM (`matchHero` hero) ms

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
  AllyWithCardCode code -> pure $ toCardCode ally == code
  AllyWithTrait trait -> hasTrait trait (toTarget ally) (toCardDef ally)
  AllyWithSphere sphere -> pure $ cdSphere (toCardDef ally) == Just sphere
  AllyWithKeyword k -> hasKeyword k (toTarget ally) (toCardDef ally)
  ReadyAlly -> pure $ not (allyExhausted $ toAttrs ally)
  ExhaustedAlly -> pure $ allyExhausted (toAttrs ally)
  CommittedAlly -> pure $ allyCommitted (toAttrs ally)
  AttackingAlly -> pure $ allyAttacking (toAttrs ally)
  UniqueAlly -> pure $ cdUnique (toCardDef ally)
  AllyWithAttached trait -> notNull <$> attachedWithTrait trait (toTarget ally)
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
  UniqueEnemy -> pure $ cdUnique (toCardDef enemy)
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
  LocationWithTrait trait -> hasTrait trait (toTarget location) (toCardDef location)
  LocationWithCardCode code -> pure $ toCardCode location == code
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

-- | Whether the hero's resources can pay for a card or cost of the sphere: its own
-- sphere's, one a gained resource icon gives it, or any while the game lets resources be
-- spent as that sphere's.
heroPaysFor :: (HasGame m) => Sphere -> Hero -> m Bool
heroPaysFor sphere h
  | sphere == Neutral || h.sphere == sphere = pure True
  | otherwise = do
      mods <- getsGame gameModifiers
      let on t = map modifierType $ Map.findWithDefault [] t mods
      pure $ GainResourceIcon sphere `elem` on (toTarget h) || ResourcesSpendableAs sphere `elem` on GameTarget

-- | Resources of the sphere the player's heroes could spend.
getResourcesOfSphere :: (HasGame m) => PlayerId -> Sphere -> m Int
getResourcesOfSphere pid sphere = do
  heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
  sum . map (.resources) <$> filterM (heroPaysFor sphere) heroes

-- | The player's allies whose resources can pay for the card, with the resources on each.
allyPayersFor :: (HasGame m) => PlayerId -> Card -> m [(AllyId, Int)]
allyPayersFor pid card = do
  allies <- traverse getAlly =<< select (AllyControlledBy $ PlayerWithId pid)
  mods <- getsGame gameModifiers
  pure
    [ (a.id, r)
    | a <- allies
    , let r = countTokens Resource (allyTokens $ toAttrs a)
    , r > 0
    , any (\case ResourcesPayFor m -> cardMatches m card; _ -> False) (map modifierType $ Map.findWithDefault [] (toTarget a) mods)
    ]

-- | A printed keyword, or one a modifier grants.
hasKeyword :: (HasGame m) => Keyword -> Target -> CardDef -> m Bool
hasKeyword k target def
  | k `member` cdKeywords def = pure True
  | otherwise = elem (GainKeyword k) . map modifierType . Map.findWithDefault [] target <$> getsGame gameModifiers

getPlayer :: (HasCallStack, HasGame m) => PlayerId -> m Player
getPlayer pid = fromJustNote ("missing player " <> show pid) . Map.lookup pid <$> getsGame gamePlayers

getHero :: (HasCallStack, HasGame m) => HeroId -> m Hero
getHero hid = fromJustNote ("missing hero " <> show hid) . Map.lookup hid <$> getsGame (entitiesHeroes . gameEntities)

getAlly :: (HasCallStack, HasGame m) => AllyId -> m Ally
getAlly aid = fromJustNote ("missing ally " <> show aid) . Map.lookup aid <$> getsGame (entitiesAllies . gameEntities)

getObjective :: (HasCallStack, HasGame m) => ObjectiveId -> m Objective
getObjective oid = fromJustNote ("missing objective " <> show oid) . Map.lookup oid <$> getsGame (entitiesObjectives . gameEntities)

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

-- | The cards attached to the host with the trait: attachments, objectives and encounter
-- cards attached as conditions.
attachedWithTrait :: (HasGame m) => Trait -> Target -> m [Target]
attachedWithTrait trait host = do
  es <- getsGame gameEntities
  let on :: (HasField "placement" a Placement) => a -> Bool
      on a = a.placement == AttachedTo host
  pure
    $ [toTarget a | a <- toList es.attachments, on a, trait `member` cdCardTraits (toCardDef a)]
    <> [toTarget o | o <- toList es.objectives, on o, trait `member` cdCardTraits (toCardDef o)]
    <> [toTarget t | t <- toList es.treacheries, on t, trait `member` cdCardTraits (toCardDef t)]

-- | Every card attached to the host: attachments, objectives and encounter cards attached
-- as conditions.
getAllAttachmentsOn :: (HasGame m) => Target -> m [Target]
getAllAttachmentsOn host = do
  es <- getsGame gameEntities
  let on :: (HasField "placement" a Placement) => a -> Bool
      on a = a.placement == AttachedTo host
  pure
    $ [toTarget a | a <- toList es.attachments, on a]
    <> [toTarget o | o <- toList es.objectives, on o]
    <> [toTarget t | t <- toList es.treacheries, on t]

-- | What the treacheries with the card code are attached to.
hostsOfTreachery :: (HasGame m) => CardCode -> m [Target]
hostsOfTreachery code = getsGame \g -> [host | t <- toList g.entities.treacheries, toCardCode t == code, AttachedTo host <- [t.placement]]

-- | Objectives in play with the trait: in the staging area or attached.
objectivesWithTrait :: (HasGame m) => Trait -> m [Objective]
objectivesWithTrait trait =
  getsGame (filter (\o -> trait `member` cdCardTraits (toCardDef o) && o.placement /= Unplaced) . toList . entitiesObjectives . gameEntities)

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

-- | The top card of the encounter deck, if there is one.
getTopOfEncounterDeck :: (HasGame m) => m (Maybe EncounterCard)
getTopOfEncounterDeck = getsGame (headMay . (.encounterDeck) . toAttrs . gameScenario)
