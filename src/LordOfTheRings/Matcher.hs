{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Matcher (module LordOfTheRings.Matcher, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability.Types (UsedAbility (..))
import LordOfTheRings.Ally.Types hiding (ally)
import LordOfTheRings.Attachment.Types
import LordOfTheRings.Card hiding (ally, enemy, hero, location)
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.Query as X
import LordOfTheRings.Effect (Effect (..))
import LordOfTheRings.Enemy.Types hiding (enemy)
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Heading as X
import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound, hitPointsOf, modifiedValue)
import LordOfTheRings.Hero.Types hiding (hero)
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (..))
import LordOfTheRings.Location.Types hiding (location)
import LordOfTheRings.Matcher.Types as X
import LordOfTheRings.Modifier
import LordOfTheRings.Name
import LordOfTheRings.Objective.Types (Objective, ObjectiveAttrs (objectiveGuard, objectiveTokens))
import LordOfTheRings.Placement
import LordOfTheRings.Quest.Types (QuestAttrs (questTokens), isEncounterSideQuest, isSideQuest)
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (GameMode (NightmareMode), scenarioGuardedPlayerCards, scenarioMode)
import LordOfTheRings.Source (Source (EnemyAttackSource), Sourceable (..))
import LordOfTheRings.Target
import LordOfTheRings.Token (Token (Damage, Resource, TimeCounter), Tokens, countTokens)
import LordOfTheRings.Trait

instance Query PlayerMatcher where
  type QueryElement PlayerMatcher = PlayerId
  select matcher = do
    players <- getsGame (toList . gamePlayers)
    map toId <$> filterM (matchPlayer matcher) players

matchPlayer :: HasGame m => PlayerMatcher -> Player -> m Bool
matchPlayer matcher player = case matcher of
  PlayerWithId pid -> pure $ player.id == pid
  FirstPlayer -> (== player.id) <$> getsGame gameFirstPlayer
  ActivePlayer -> (== player.id) <$> getsGame gameActivePlayer
  AnyPlayer -> pure True
  NotPlayer m -> not <$> matchPlayer m player
  PlayerWithThreatAtLeast n -> (>= n) <$> getThreat player.id
  PlayerWithCardsInHand n -> pure $ length player.hand >= n
  PlayerMatchAll ms -> allM (`matchPlayer` player) ms
  PlayerWithCardsInDeck n -> pure $ length player.deck >= n
  PlayerUsingErrata -> pure player.errata
  PlayerWithHiddenCards -> getsGame (notNull . Map.findWithDefault [] player.id . (.hiddenCards) . toAttrs . gameScenario)

data Character = HeroCharacter Hero | AllyCharacter Ally

instance Targetable Character where
  toTarget (HeroCharacter h) = toTarget h
  toTarget (AllyCharacter a) = toTarget a

instance HasCardDef Character where
  toCardDef (HeroCharacter h) = toCardDef h
  toCardDef (AllyCharacter a) = toCardDef a

instance HasField "name" Character Name where
  getField (HeroCharacter h) = h.name
  getField (AllyCharacter a) = a.name

instance HasField "id" Character CharacterId where
  getField (HeroCharacter h) = HeroCharacterId h.id
  getField (AllyCharacter a) = AllyCharacterId a.id

instance Query CharacterMatcher where
  type QueryElement CharacterMatcher = CharacterId
  select matcher = do
    heroes <-
      Map.mapKeys HeroCharacterId . map HeroCharacter <$> getsGame (entitiesHeroes . gameEntities)
    allies <-
      Map.mapKeys AllyCharacterId . map AllyCharacter <$> getsGame (entitiesAllies . gameEntities)
    let characters = toList $ heroes <> allies
    map (.id) <$> filterM (matchCharacter matcher) characters

matchCharacter :: HasGame m => CharacterMatcher -> Character -> m Bool
matchCharacter matcher character = case matcher of
  CharacterWithTitle name -> pure $ character.name == name
  CharacterWithTrait trait -> hasTrait trait (toTarget character) (toCardDef character)

instance Query HeroMatcher where
  type QueryElement HeroMatcher = HeroId
  select matcher = do
    -- a captive guarded by an encounter card is a guarded objective, not a hero
    heroes <- getsGame (filter (isNothing . heroGuardedBy . toAttrs) . toList . entitiesHeroes . gameEntities)
    map toId <$> filterM (matchHero matcher) heroes

matchHero :: HasGame m => HeroMatcher -> Hero -> m Bool
matchHero matcher hero = case matcher of
  HeroWithId hid -> pure $ hero.id == hid
  -- a prisoner is face down: no one controls it until it is rescued
  -- and one in the staging area is under no player's control (The Long Arm of Mordor)
  HeroControlledBy pm -> if heroPrisoner (toAttrs hero) || heroInStagingArea (toAttrs hero) then pure False else elem hero.owner <$> select pm
  HeroInStagingArea -> pure $ heroInStagingArea (toAttrs hero)
  HeroWithTitle name -> pure $ hero.name == name
  HeroWithTrait trait -> hasTrait trait (toTarget hero) (toCardDef hero)
  -- a hero belongs to the sphere of each resource icon it gains too, as Song of Wisdom's
  HeroWithSphere sphere ->
    (hero.sphere == sphere ||)
      . elem (GainResourceIcon sphere)
      . map modifierType
      . Map.findWithDefault [] (toTarget hero)
      <$> getsGame gameModifiers
  HeroWithKeyword k -> hasKeyword k (toTarget hero) (toCardDef hero)
  ReadyHero -> pure $ not (heroExhausted $ toAttrs hero)
  ExhaustedHero -> pure $ heroExhausted (toAttrs hero)
  CommittedHero -> pure $ heroCommitted (toAttrs hero)
  AttackingHero -> pure $ heroAttacking (toAttrs hero)
  DefendingHero -> isDefending (toTarget hero)
  PrisonerHero -> pure $ heroPrisoner (toAttrs hero)
  HeroWithAttached trait -> notNull <$> attachedWithTrait trait (toTarget hero)
  HeroWithPlayerAttachment -> notNull <$> playerAttachmentsOn (toTarget hero)
  HeroWithReadyPlayerAttachment -> any (not . attachmentExhausted . toAttrs) <$> playerAttachmentsOn (toTarget hero)
  HeroWithReadyAttached trait ->
    getsGame
      ( any
          ( \a ->
              a.placement
                == AttachedTo (toTarget hero)
                && trait
                `member` cdCardTraits (toCardDef a)
                && not (attachmentExhausted $ toAttrs a)
          )
          . toList
          . entitiesAttachments
          . gameEntities
      )
  -- an objective attached counts, as a Scarred burden
  HeroWithAttachment code ->
    getsGame \g ->
      any (\a -> a.placement == AttachedTo (toTarget hero) && toCardCode a == code) (entitiesAttachments $ gameEntities g)
        || any (\o -> o.placement == AttachedTo (toTarget hero) && toCardCode o == code) (entitiesObjectives $ gameEntities g)
  HeroWithResources -> pure $ hero.resources > 0
  HeroWithPrintedSphere sphere -> pure $ hero.sphere == sphere
  HeroWithResourcesAtLeast n -> pure $ hero.resources >= n
  AnyHero -> pure True
  NotHero m -> not <$> matchHero m hero
  HeroMatchAll ms -> allM (`matchHero` hero) ms
  HeroMatchAny ms -> anyM (`matchHero` hero) ms

instance Query AllyMatcher where
  type QueryElement AllyMatcher = AllyId
  select matcher = do
    -- a captive guarded by an encounter card is a guarded objective, not an ally
    allies <- getsGame (filter (not . guardedAlly) . toList . entitiesAllies . gameEntities)
    -- an ally that has become a hero is still found by its id, as its own text asks after it
    asHeroes <- getsGame (filter (pins matcher . toId) . mapMaybe (.asAlly) . toList . entitiesHeroes . gameEntities)
    map toId <$> filterM (matchAlly matcher) (allies <> asHeroes)
   where
    pins m aid = case m of
      AllyWithId i -> i == aid
      AllyMatchAll ms -> any (`pins` aid) ms
      _ -> False
    guardedAlly a = case a.placement of
      AttachedTo (CardIdTarget _) -> True
      _ -> False

matchAlly :: HasGame m => AllyMatcher -> Ally -> m Bool
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
  DefendingAlly -> isDefending (toTarget ally)
  UniqueAlly -> pure $ cdUnique (toCardDef ally)
  AllyEnteredPlayThisRound ->
    getsGame
      ( any (\e -> effectTarget e == toTarget ally && EnteredPlayThisRound `elem` effectModifiers e)
          . gameEffects
      )
  AllyWithAttached trait -> notNull <$> attachedWithTrait trait (toTarget ally)
  AllyWithPlayerAttachment -> notNull <$> playerAttachmentsOn (toTarget ally)
  AllyWithReadyPlayerAttachment -> any (not . attachmentExhausted . toAttrs) <$> playerAttachmentsOn (toTarget ally)
  AllyWithAttachment code ->
    getsGame (any (\a -> a.placement == AttachedTo (toTarget ally) && toCardCode a == code) . entitiesAttachments . gameEntities)
  AllyWithReadyAttached trait ->
    getsGame
      ( any
          ( \a ->
              a.placement == AttachedTo (toTarget ally)
                && maybe True (`member` cdCardTraits (toCardDef a)) trait
                && not (attachmentExhausted $ toAttrs a)
          )
          . toList
          . entitiesAttachments
          . gameEntities
      )
  AnyAlly -> pure True
  NotAlly m -> not <$> matchAlly m ally
  AllyMatchAll ms -> allM (`matchAlly` ally) ms
  AllyMatchAny ms -> anyM (`matchAlly` ally) ms
  ObjectiveAlly -> pure $ cdCardType (toCardDef ally) == ObjectiveAllyType

instance Query EnemyMatcher where
  type QueryElement EnemyMatcher = EnemyId
  select matcher = do
    -- an enemy attached to a character, as Grasping Tentacle becomes, is an attachment; one
    -- grappled with a location is still an enemy (The Thing in the Depths rules)
    enemies <- getsGame (filter (not . attached . (.placement)) . toList . entitiesEnemies . gameEntities)
    map toId <$> filterM (\e -> andM [notOutOfPlay (toTarget e), matchEnemy matcher e]) enemies
   where
    attached = \case
      AttachedTo (LocationTarget _) -> False
      AttachedTo _ -> True
      _ -> False

-- | Whether the enemy is there, or considered to be, as Tide-runner off-course.
enemyIn :: HasGame m => Placement -> Enemy -> m Bool
enemyIn p enemy
  | enemy.placement == p = pure True
  | otherwise = elem (ConsideredIn p) . map modifierType . Map.findWithDefault [] (toTarget enemy) <$> getsGame gameModifiers

matchEnemy :: HasGame m => EnemyMatcher -> Enemy -> m Bool
matchEnemy matcher enemy = case matcher of
  EnemyWithId eid -> pure $ enemy.id == eid
  EnemyInStagingArea -> enemyIn StagingArea enemy
  EnemyInStormcallerArea -> enemyIn StormcallerArea enemy
  EnemyWithTrait trait -> hasTrait trait (toTarget enemy) (toCardDef enemy)
  EnemyWithCardCode code -> pure $ toCardCode enemy == code
  EnemyWithCardId cid -> pure $ (toCard enemy).id == cid
  EnemyWithFacedownShadow -> pure $ any ((`notElem` (toAttrs enemy).enemyShadowRevealed) . (.id)) enemy.shadowCards
  EnemyWithRevealedShadow -> pure $ notNull (toAttrs enemy).enemyShadowRevealed
  EnemyEngagedWith pm -> case enemy.placement of
    EngagedWith pid -> elem pid <$> select pm
    -- engaged with each matching player while it stays in the staging area, as Arkham's
    -- massive enemies are with each investigator at their location
    StagingArea -> do
      mods <- map modifierType . Map.findWithDefault [] (toTarget enemy) <$> getsGame gameModifiers
      anyM (\m -> selectAny (pm <> m)) [m | EngagedWithEachPlayer m <- mods]
    _ -> pure False
  UniqueEnemy -> pure $ cdUnique (toCardDef enemy)
  EnemyWithAttached trait -> notNull <$> attachedWithTrait trait (toTarget enemy)
  EnemyNotImmuneToPlayerCards -> not <$> immuneToPlayerCards (toTarget enemy)
  EnemyWithAnyAttachment -> notNull <$> getAttachmentsOn (toTarget enemy)
  EnemyWithResources -> pure $ countTokens Resource (toAttrs enemy).enemyTokens > 0
  EnemyGuardingACard -> guardingACard (toCard enemy).id (toTarget enemy)
  EnemyWithNoHitPointsRemaining -> (enemy.damage >=) <$> modifiedValue hitPointsOf (toTarget enemy) enemy.hitPoints
  EnemyGrappledWith lm -> case enemy.placement of
    AttachedTo (LocationTarget lid) -> elem lid <$> select lm
    _ -> pure False
  EnemyWithEngagementCostAboveThreatOf pid -> do
    mods <- map modifierType . Map.findWithDefault [] (toTarget enemy) <$> getsGame gameModifiers
    threat <- getThreat pid
    pure $ max 0 (enemy.engagementCost + sum [n | EngagementCostModifier n <- mods]) > threat
  AnyEnemy -> pure True
  NotEnemy m -> not <$> matchEnemy m enemy
  EnemyMatchAll ms -> allM (`matchEnemy` enemy) ms
  EnemyWithKeyword k -> hasKeyword k (toTarget enemy) (toCardDef enemy)

instance Query LocationMatcher where
  type QueryElement LocationMatcher = LocationId
  select matcher = do
    locations <- getsGame (toList . entitiesLocations . gameEntities)
    map toId <$> filterM (\l -> andM [notOutOfPlay (toTarget l), matchLocation matcher l]) locations

matchLocation :: HasGame m => LocationMatcher -> Location -> m Bool
matchLocation matcher location = case matcher of
  LocationWithId lid -> pure $ location.id == lid
  LocationInStagingArea -> pure $ location.placement == StagingArea
  IsActiveLocation -> pure $ location.placement == ActiveLocation
  LocationWithTrait trait -> hasTrait trait (toTarget location) (toCardDef location)
  LocationWithKeyword k -> hasKeyword k (toTarget location) (toCardDef location)
  LocationWithDiscover -> isJust <$> discoverValue (toTarget location) (toCardDef location)
  LocationWithCardCode code -> pure $ toCardCode location == code
  LocationNotImmuneToPlayerCards -> not <$> immuneToPlayerCards (toTarget location)
  AnyLocation -> pure True
  NotLocation m -> not <$> matchLocation m location
  LocationMatchAll ms -> allM (`matchLocation` location) ms
  LocationMatchAny ms -> anyM (`matchLocation` location) ms
  LocationControlledBy pm -> case location.placement of
    InPlayArea pid -> elem pid <$> select pm
    _ -> pure False
  LocationWithIdBelow lid -> pure $ location.id < lid
  UniqueLocation -> pure $ cdUnique (toCardDef location)
  LocationAtPosition pos -> pure $ location.position == Just pos
  LocationWithActiveLocationTitle -> do
    let title l = case cdName (toCardDef l) of Name t _ -> t
    active <- getsGame (filter ((== ActiveLocation) . (.placement)) . toList . entitiesLocations . gameEntities)
    pure $ any ((== title location) . title) active
  LocationDiscardableByPlayerCards -> notElem CannotBeDiscardedByPlayerCards . map modifierType . Map.findWithDefault [] (toTarget location) <$> getsGame gameModifiers
  LocationCanEnterStagingArea -> notElem CannotEnterStagingArea . map modifierType . Map.findWithDefault [] (toTarget location) <$> getsGame gameModifiers
  LocationGuardingACard -> guardingACard (toCard location).id (toTarget location)
  LocationWithAttachment -> notNull <$> getAttachmentsOn (toTarget location)
  LocationWithoutProgress -> pure $ location.progress == 0
  LocationWithPlayerCardAttachment ->
    getsGame (any ((== AttachedTo (toTarget location)) . (.placement)) . toList . entitiesAttachments . gameEntities)

-- | Not considered out of play, as Saruman's chosen card is.
notOutOfPlay :: HasGame m => Target -> m Bool
notOutOfPlay t = notElem ConsideredOutOfPlay . map modifierType . Map.findWithDefault [] t <$> getsGame gameModifiers

immuneToPlayerCards :: HasGame m => Target -> m Bool
immuneToPlayerCards t =
  elem ImmuneToPlayerCardEffects
    . map modifierType
    . Map.findWithDefault [] t
    <$> getsGame gameModifiers

-- | A unique card cannot enter play while a card with its title is in play.
passesUnique :: HasGame m => Card -> m Bool
passesUnique card
  | card.unique =
      andM
        [ selectNone $ AllyWithTitle card.name
        , selectNone $ HeroWithTitle card.name
        , getsGame $ all ((/= card.name) . cdName . toCardDef) . toList . (.entities.attachments)
        , -- separated players still keep to it across their areas (FAQ 2.36)
          getsGame $ all (notElem card.name . titles) . heldEntities
        ]
  | otherwise = pure True

-- | The titles of the player cards in play.
titles :: Entities -> [Name]
titles es =
  map (cdName . toCardDef) (toList es.heroes)
    <> map (cdName . toCardDef) (toList es.allies)
    <> map (cdName . toCardDef) (toList es.attachments)

-- | Whether the character is defending the attack being resolved.
isDefending :: HasGame m => Target -> m Bool
isDefending target = elem Defending . map modifierType . Map.findWithDefault [] target <$> getsGame gameModifiers

-- | The card's traits as they are now: its printed traits, those it gained, less those it
-- lost, as Chieftain's Brooch reads its hero's.
getTraits :: HasGame m => Target -> CardDef -> m (Set Trait)
getTraits target def = do
  mods <- map modifierType . Map.findWithDefault [] target <$> getsGame gameModifiers
  pure $ setFromList [t | t <- toList (cdCardTraits def) <> [t' | GainTrait t' <- mods], LoseTrait t `notElem` mods]

-- | The characters defending the enemy's attack, as "while attacking a character with less
-- willpower" reads of Uruk Warg-Rider's.
getDefendersAgainst :: HasGame m => EnemyId -> m [Target]
getDefendersAgainst eid = getsGame \g ->
  [t | (t, ms) <- Map.toList (gameModifiers g), any (\m -> m.modifierType == Defending && m.modifierSource == EnemyAttackSource eid) ms]

-- | A printed trait, or one a modifier grants, unless it lost it, as Shot from the Sky
-- takes the Eagle trait.
hasTrait :: HasGame m => Trait -> Target -> CardDef -> m Bool
hasTrait trait target def = member trait <$> getTraits target def

-- | The character's traits: its printed ones and those a modifier grants, less those it
-- lost.
characterTraits :: HasGame m => Target -> m (Set Trait)
characterTraits target = case target of
  HeroTarget hid -> getTraits target . toCardDef =<< getHero hid
  AllyTarget aid -> getTraits target . toCardDef =<< getAlly aid
  _ -> pure mempty

{- | Whether the hero's resources can pay for a card or cost of the sphere: its own
sphere's, one a gained resource icon gives it, or any while the game lets resources be
spent as that sphere's.
-}
heroPaysFor :: HasGame m => Sphere -> Hero -> m Bool
heroPaysFor sphere h = do
  mods <- getsGame gameModifiers
  let on t = map modifierType $ Map.findWithDefault [] t mods
      own = on (toTarget h)
  pure
    $ CannotSpendResources `notElem` own
    && or
      [ sphere == Neutral
      , h.sphere == sphere
      , GainResourceIcon sphere `elem` own
      , ResourcesSpendableAs sphere `elem` on GameTarget
      ]

-- | Whether resources can be spent from the hero's pool at all.
heroCanSpend :: HasGame m => Hero -> m Bool
heroCanSpend h = notElem CannotSpendResources . map modifierType . Map.findWithDefault [] (toTarget h) <$> getsGame gameModifiers

-- | Resources of the sphere the player's heroes could spend.
getResourcesOfSphere :: HasGame m => PlayerId -> Sphere -> m Int
getResourcesOfSphere pid sphere = do
  heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
  sum . map (.resources) <$> filterM (heroPaysFor sphere) heroes

-- | Resources of the sphere the players as a group could spend, as Heed the Dream asks.
getGroupResourcesOfSphere :: HasGame m => Sphere -> m Int
getGroupResourcesOfSphere sphere = sum <$> (traverse (`getResourcesOfSphere` sphere) =<< getRemainingPlayers)

{- | Allies in any player's discard pile, with their owners and printed costs, that the
player has the sphere's resources to pay for and the unique rule lets into play, as
Stand and Fight puts into play.
-}
affordableDiscardedAllies :: HasGame m => PlayerId -> Sphere -> m [(PlayerId, PlayerCard, Int)]
affordableDiscardedAllies = affordableDiscarded AllyType

-- | As 'affordableDiscardedAllies', attachments, as Reforged plays one.
affordableDiscardedAttachments :: HasGame m => PlayerId -> Sphere -> m [(PlayerId, PlayerCard, Int)]
affordableDiscardedAttachments = affordableDiscarded AttachmentType

affordableDiscarded :: HasGame m => CardType -> PlayerId -> Sphere -> m [(PlayerId, PlayerCard, Int)]
affordableDiscarded cType pid sphere = do
  spend <- getResourcesOfSphere pid sphere
  order <- getPlayerOrder
  fmap concat $ for order \owner -> do
    discarded <- filterM passesUnique . (.discard) =<< getPlayer owner
    pure
      [ (owner, pc, x)
      | PlayerCard pc <- discarded
      , cdCardType (toCardDef pc) == cType
      , -- neutral cards belong to no sphere, so "any sphere" leaves them out (FAQ)
      cdSphere (toCardDef pc) /= Just Neutral
      , Just (StaticCost x) <- [cdCost (toCardDef pc)]
      , x <= spend
      ]

-- | Allies Hirgon and similar effects can play from hand at a reduced cost.
affordableAlliesInHand :: HasGame m => PlayerId -> Sphere -> Int -> Int -> m [Card]
affordableAlliesInHand pid sphere reduction minimumCost = do
  player <- getPlayer pid
  resources <- getResourcesOfSphere pid sphere
  mods <- getsGame gameModifiers
  let on t = map modifierType $ Map.findWithDefault [] t mods
      gameMods = on GameTarget
      forbidden c = any (\case CannotPlay m -> cardMatches m c; _ -> False) (gameMods <> on (PlayerTarget pid))
      cost c = maybe 0 costAsResources c.cost
      reducedCost c = min (cost c) $ max minimumCost (cost c - reduction)
  entryBarred <- getsGame allyEntryBarred
  allyPlayed <- any (\(_, _, t) -> t == AllyType) <$> cardsPlayedThisRound
  if (OneAllyPerRound `elem` gameMods && allyPlayed) || entryBarred
    then pure []
    else filterM passesUnique
      [ c
      | c <- player.hand
      , c.cardType == AllyType
      , c.sphere == Just sphere
      , resources >= reducedCost c
      , not $ forbidden c
      ]

-- | Allies in hand matching the matcher that the player's heroes could pay for with
-- resources of any sphere, as Thranduil plays a Silvan ally needing no resource match.
affordableAlliesInHandWithoutMatch :: HasGame m => PlayerId -> CardMatcher -> m [Card]
affordableAlliesInHandWithoutMatch pid matcher = do
  player <- getPlayer pid
  heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
  resources <- sum . map (.resources) <$> filterM heroCanSpend heroes
  mods <- getsGame gameModifiers
  let on t = map modifierType $ Map.findWithDefault [] t mods
      gameMods = on GameTarget
      forbidden c = any (\case CannotPlay m -> cardMatches m c; _ -> False) (gameMods <> on (PlayerTarget pid))
      cost c = maybe 0 costAsResources c.cost
  entryBarred <- getsGame allyEntryBarred
  allyPlayed <- any (\(_, _, t) -> t == AllyType) <$> cardsPlayedThisRound
  if (OneAllyPerRound `elem` gameMods && allyPlayed) || entryBarred
    then pure []
    else
      filterM passesUnique
        [c | c <- player.hand, c.cardType == AllyType, cardMatches matcher c, resources >= cost c, not $ forbidden c]

-- | The player's allies whose resources can pay for the card, with the resources on each.
allyPayersFor :: HasGame m => PlayerId -> Card -> m [(AllyId, Int)]
allyPayersFor pid card = do
  allies <- traverse getAlly =<< select (AllyControlledBy $ PlayerWithId pid)
  mods <- getsGame gameModifiers
  pure
    [ (a.id, r)
    | a <- allies
    , let r = countTokens Resource (allyTokens $ toAttrs a)
    , r > 0
    , any
        (\case ResourcesPayFor m -> cardMatches m card; _ -> False)
        (map modifierType $ Map.findWithDefault [] (toTarget a) mods)
    ]

-- | A printed keyword, or one a modifier grants.
hasKeyword :: HasGame m => Keyword -> Target -> CardDef -> m Bool
hasKeyword k target def = do
  mods <- map modifierType . Map.findWithDefault [] target <$> getsGame gameModifiers
  -- a blank text box has no printed keywords; losing a keyword wins over gaining it, as
  -- Arkham applies removes after adds
  pure $ LoseKeyword k `notElem` mods && (GainKeyword k `elem` mods || (k `member` cdKeywords def && Blank `notElem` mods))

{- | The card's Discover value, if it has the keyword, printed or gained: moved by its
modifiers, as Hidden Staircase's X and Crypt Key's 1, and never below 0 (MEC69 rules).
-}
discoverValue :: HasGame m => Target -> CardDef -> m (Maybe Int)
discoverValue target def = do
  mods <- map modifierType . Map.findWithDefault [] target <$> getsGame gameModifiers
  let values = [n | Blank `notElem` mods, Discover n <- toList (cdKeywords def)] <> [n | GainKeyword (Discover n) <- mods]
  pure $ if null values then Nothing else Just $ max 0 $ sum values + sum [k | DiscoverValueModifier k <- mods]

-- | The Discover value of the location in play, if it has the keyword.
getDiscoverValue :: HasGame m => LocationId -> m (Maybe Int)
getDiscoverValue lid =
  getsGame (Map.lookup lid . entitiesLocations . gameEntities) >>= \case
    Just l -> discoverValue (toTarget l) (toCardDef l)
    Nothing -> pure Nothing

-- | A keyword's value on the card, printed or gained, as an X a card computes reads.
keywordValue :: HasGame m => (Keyword -> Maybe Int) -> Target -> CardDef -> m Int
keywordValue value target def = do
  mods <- map modifierType . Map.findWithDefault [] target <$> getsGame gameModifiers
  let printed = [n | Blank `notElem` mods, Just n <- map value (toList (cdKeywords def))]
  pure $ sum $ printed <> [n | GainKeyword k <- mods, Just n <- [value k]]

-- | The archery the card has, printed or gained, as Archery X reads.
getArchery :: HasGame m => Target -> CardDef -> m Int
getArchery = keywordValue \case Archery n -> Just n; _ -> Nothing

-- | The quest stage, location or enemy in play, as its card and its tokens.
timeCard :: Entities -> Target -> Maybe (CardDef, Tokens)
timeCard es = \case
  QuestTarget qid -> (\q -> (toCardDef q, questTokens $ toAttrs q)) <$> Map.lookup qid es.quests
  LocationTarget lid -> (\l -> (toCardDef l, locationTokens $ toAttrs l)) <$> Map.lookup lid es.locations
  EnemyTarget eid -> (\e -> (toCardDef e, enemyTokens $ toAttrs e)) <$> Map.lookup eid es.enemies
  _ -> Nothing

-- | The time counters on the card.
timeCountersIn :: Entities -> Target -> Int
timeCountersIn es = maybe 0 (countTokens TimeCounter . snd) . timeCard es

getTimeCounters :: HasGame m => Target -> m Int
getTimeCounters t = getsGame ((`timeCountersIn` t) . gameEntities)

-- | Whether a time counter can be removed from the card: it has one, and nothing bars it, as
-- an "either ... or remove 1 time counter" choice asks.
canRemoveTimeCounter :: HasGame m => Target -> m Bool
canRemoveTimeCounter t = do
  mods <- map modifierType . Map.findWithDefault [] t <$> getsGame gameModifiers
  (&& CannotRemoveTimeCounters `notElem` mods) . (> 0) <$> getTimeCounters t

canRemoveTimeCounterFromCurrentQuest :: HasGame m => m Bool
canRemoveTimeCounterFromCurrentQuest = maybe (pure False) canRemoveTimeCounter =<< getCurrentQuest

{- | The card's Time X value, printed or gained (a computed X is a 'GainKeyword' of 'Time'
the card gives itself, as Archery X): 0 for a card without it.
-}
getTimeX :: HasGame m => Target -> m Int
getTimeX t =
  getsGame ((`timeCard` t) . gameEntities)
    >>= maybe (pure 0) (keywordValue (\case Time n -> Just n; _ -> Nothing) t . fst)

-- | The archery of every card in play, the current quest's too, and any bonus, which the players deal
-- among the characters in play as the combat phase begins.
getArcheryTotal :: (HasGame m) => m Int
getArcheryTotal = do
  let inPlay p = p `notElem` [InVictoryDisplay, Unplaced]
  enemies <- getsGame (filter (inPlay . (.placement)) . toList . entitiesEnemies . gameEntities)
  locations <- getsGame (filter (inPlay . (.placement)) . toList . entitiesLocations . gameEntities)
  fromEnemies <- for enemies \e -> getArchery (toTarget e) (toCardDef e)
  fromLocations <- for locations \l -> getArchery (toTarget l) (toCardDef l)
  onQuest <- getCurrentQuest >>= \case
    Just t@(QuestTarget qid) -> maybe (pure 0) (getArchery t . toCardDef) =<< getsGame (Map.lookup qid . entitiesQuests . gameEntities)
    _ -> pure 0
  bonus <- sum . (\ms -> [n | ArcheryBonus n <- map modifierType ms]) . Map.findWithDefault [] GameTarget <$> getsGame gameModifiers
  pure $ sum fromEnemies + sum fromLocations + onQuest + bonus

-- | The current quest has the keyword, printed or gained, as battle or siege.
currentQuestHas :: HasGame m => Keyword -> m Bool
currentQuestHas k =
  getCurrentQuest >>= \case
    Just t@(QuestTarget qid) ->
      maybe (pure False) (hasKeyword k t . toCardDef) . Map.lookup qid =<< getsGame (entitiesQuests . gameEntities)
    _ -> pure False

-- | Whether the current quest has any keyword, printed or gained, as "if the current quest
-- card has a keyword" reads.
currentQuestHasAKeyword :: HasGame m => m Bool
currentQuestHasAKeyword = notNull <$> currentQuestKeywords

-- | The keywords the current quest has, printed and not lost, or gained.
currentQuestKeywords :: HasGame m => m [Keyword]
currentQuestKeywords =
  getCurrentQuest >>= \case
    Just t@(QuestTarget qid) -> do
      mods <- map modifierType . Map.findWithDefault [] t <$> getsGame gameModifiers
      printed <- maybe [] (toList . cdKeywords . toCardDef) . Map.lookup qid <$> getsGame (entitiesQuests . gameEntities)
      let kept = [k | Blank `notElem` mods, k <- printed] <> [k | GainKeyword k <- mods]
      pure $ ordNub $ filter (\k -> LoseKeyword k `notElem` mods) kept
    _ -> pure []

getPlayer :: (HasCallStack, HasGame m) => PlayerId -> m Player
getPlayer pid = fromJustNote ("missing player " <> show pid) . Map.lookup pid <$> getsGame gamePlayers

-- | Whether the top card of the player's discard pile is of the type, as "while the top
-- card of your discard pile is an ally" reads.
topOfDiscardIs :: HasGame m => CardType -> PlayerId -> m Bool
topOfDiscardIs t pid = maybe False ((== t) . (.cardType)) . headMay . (.discard) <$> getPlayer pid

-- | The copies of the card in its player's discard pile, the card itself aside, as
-- "resolve that effect again for each copy of it currently in your discard pile" reads.
copiesInDiscard :: HasGame m => PlayerId -> PlayerCard -> m Int
copiesInDiscard pid card =
  length . filter (\c -> toCardCode c == toCardCode card && c.id /= card.id) . (.discard) <$> getPlayer pid
-- | The player's threat as cards read it: no higher than an effect treats it as, as Vanish
-- from Sight treats it as 20. Raising or lowering it moves the dial itself.
getThreat :: HasGame m => PlayerId -> m Int
getThreat pid = do
  threat <- (.threat) <$> getPlayer pid
  mods <- map modifierType . Map.findWithDefault [] (PlayerTarget pid) <$> getsGame gameModifiers
  pure $ foldr min threat [n | ThreatCountsAsAtMost n <- mods]

-- | The player's threat as an engagement check reads it, an enemy's in the discard pile
-- too: more by what he is considered to have for engagement checks, as Vicious Trackers
-- reads.
getEngagementThreat :: HasGame m => PlayerId -> m Int
getEngagementThreat pid = do
  threat <- getThreat pid
  mods <- map modifierType . Map.findWithDefault [] (PlayerTarget pid) <$> getsGame gameModifiers
  pure $ threat + sum [n | ThreatForEngagementChecks n <- mods]

-- | The player whose set-aside cards hold the card, as a Ranger of the North shuffled
-- into the encounter deck stays its owner's.
setAsideOwner :: HasGame m => CardId -> m (Maybe PlayerId)
setAsideOwner cid = fmap (.id) . find (any ((== cid) . (.id)) . (.setAsideCards)) . toList <$> getsGame gamePlayers

getHero :: (HasCallStack, HasGame m) => HeroId -> m Hero
getHero hid =
  fromJustNote ("missing hero " <> show hid)
    . Map.lookup hid
    <$> getsGame (entitiesHeroes . gameEntities)

getAlly :: (HasCallStack, HasGame m) => AllyId -> m Ally
getAlly aid =
  fromJustNote ("missing ally " <> show aid)
    . Map.lookup aid
    <$> getsGame (entitiesAllies . gameEntities)

getObjective :: (HasCallStack, HasGame m) => ObjectiveId -> m Objective
getObjective oid =
  fromJustNote ("missing objective " <> show oid)
    . Map.lookup oid
    <$> getsGame (entitiesObjectives . gameEntities)

getEnemy :: (HasCallStack, HasGame m) => EnemyId -> m Enemy
getEnemy eid =
  fromJustNote ("missing enemy " <> show eid)
    . Map.lookup eid
    <$> getsGame (entitiesEnemies . gameEntities)

getLocation :: (HasCallStack, HasGame m) => LocationId -> m Location
getLocation lid =
  fromJustNote ("missing location " <> show lid)
    . Map.lookup lid
    <$> getsGame (entitiesLocations . gameEntities)

-- | The progress tokens on the active location, none when there is none.
activeLocationProgress :: HasGame m => m Int
activeLocationProgress = sum . map (.progress) <$> (traverse getLocation =<< select IsActiveLocation)

-- | The attachments on the active location, as "for each attachment on the active
-- location" reads: none without one.
activeLocationAttachments :: HasGame m => m Int
activeLocationAttachments = sum <$> (traverse (fmap length . getAttachmentsOn . toTarget) =<< select IsActiveLocation)

-- | Whether the game is dealt in nightmare mode, for a card whose standard text the
-- nightmare setup card changes.
inNightmareMode :: HasGame m => m Bool
inNightmareMode = getsGame ((== NightmareMode) . scenarioMode . toAttrs . gameScenario)

getLead :: HasGame m => m PlayerId
getLead = getsGame gameFirstPlayer

-- | The last player: the last still in the game in turn order from the first player.
getLastPlayer :: HasGame m => m PlayerId
getLastPlayer = maybe getLead pure . lastMay =<< getRemainingPlayers

-- | Players in turn order, starting with the first player.
getPlayerOrder :: HasGame m => m [PlayerId]
getPlayerOrder = do
  -- separated, only the players in the area in focus
  present <- getsGame gamePlayers
  order <- getsGame (filter (`Map.member` present) . gamePlayerOrder)
  firstPlayer <- getsGame gameFirstPlayer
  let (before, after) = break (== firstPlayer) order
  pure $ after <> before

-- | Heroes and allies matching the given matchers, as targets.
-- | Whether the player's cards follow the FAQ's errata, as a card reads it as it resolves.
playsWithErrata :: HasGame m => PlayerId -> m Bool
playsWithErrata pid = selectAny (PlayerWithId pid <> PlayerUsingErrata)

selectCharacters :: HasGame m => HeroMatcher -> AllyMatcher -> m [Target]
selectCharacters heroes allies = (<>) <$> (map toTarget <$> select heroes) <*> (map toTarget <$> select allies)

charactersControlledBy :: HasGame m => PlayerId -> m [Target]
charactersControlledBy pid = selectCharacters (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)

-- | The number of characters controlled by the player who controls the most characters, as
-- Hills of Wilderland's X reads.
mostCharactersControlled :: HasGame m => m Int
mostCharactersControlled = foldr max 0 <$> (traverse (fmap length . charactersControlledBy) =<< getRemainingPlayers)

exhaustedCharactersOf :: HasGame m => PlayerId -> m [Target]
exhaustedCharactersOf pid =
  selectCharacters (ExhaustedHero <> HeroControlledBy (PlayerWithId pid)) (ExhaustedAlly <> AllyControlledBy (PlayerWithId pid))

-- | The player attachments attached to the host.
playerAttachmentsOn :: HasGame m => Target -> m [Attachment]
playerAttachmentsOn host = getsGame \g -> [a | a <- toList (entitiesAttachments $ gameEntities g), a.placement == AttachedTo host]

-- | Every card attached to the host: player attachments, and encounter cards attached as
-- conditions or claimed objectives too, as "discard 1 attachment" may take them (Driven by
-- Shadow, Black Uruks rulings).
getAttachmentsOn :: HasGame m => Target -> m [Target]
getAttachmentsOn host = getsGame \g ->
  let es = gameEntities g
      on' :: HasField "placement" x Placement => x -> Bool
      on' x = x.placement == AttachedTo host
   in [toTarget a | a <- toList (entitiesAttachments es), on' a]
        <> [toTarget t | t <- toList (entitiesTreacheries es), on' t]
        <> [toTarget o | o <- toList (entitiesObjectives es), on' o]

{- | The characters the enemy's attack destroyed, as "if this attack destroys a character"
reads: while the attack resolves, and once it is over until the enemy attacks again. Only
those its damage destroyed count, as the attacking character's damage must destroy an
enemy for Foe-hammer (FAQ): not one its shadow card's damage destroyed, a card effect, nor
one that left play another way during the attack.
-}
attackDestroyed :: HasGame m => EnemyId -> m [AttackDestroyed]
attackDestroyed eid = getsGame (filter ((== EnemyAttackSource eid) . attackDestroyedBy) . gameAttackDestroyed)

-- | The damage the enemy's last attack dealt, and to what, as "each damage dealt by this
-- attack" reads: kept until it attacks again.
attackDamaged :: HasGame m => EnemyId -> m [(Target, Int)]
attackDamaged = damageDealtBy . EnemyAttackSource

-- | The damage the source has dealt this phase, and to what, as "each character damaged by
-- this effect" reads.
damageDealtBy :: (HasGame m, Sourceable source) => source -> m [(Target, Int)]
damageDealtBy source = getsGame (\g -> [(t, n) | (s, t, n) <- gameDamageDealt g, s == toSource source])

-- | Whether the enemy's attack destroyed a character: see 'attackDestroyed'.
attackDestroyedACharacter :: HasGame m => EnemyId -> m Bool
attackDestroyedACharacter = fmap notNull . attackDestroyed

-- | The allies the enemy's attack destroyed: see 'attackDestroyed'.
attackDestroyedAllies :: HasGame m => EnemyId -> m [AttackDestroyed]
attackDestroyedAllies = fmap (filter \d -> case d.target of AllyTarget _ -> True; _ -> False) . attackDestroyed

-- | The player who controls the character, if it is one.
characterController :: HasGame m => Target -> m (Maybe PlayerId)
characterController = \case
  HeroTarget hid -> (\h -> guard (not $ heroInStagingArea $ toAttrs h) $> h.owner) <$> getHero hid
  AllyTarget aid -> placementController . (.placement) <$> getAlly aid
  _ -> pure Nothing

getAttachmentsControlledBy :: HasGame m => PlayerId -> m [Target]
getAttachmentsControlledBy pid = do
  es <- getsGame gameEntities
  heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
  -- a claimed objective counts as an attachment of the hero it is attached to
  pure
    $ [toTarget a | a <- toList es.attachments, (toAttrs a).controller == pid]
    <> [toTarget o | o <- toList es.objectives, objectiveOn o `elem` map Just heroes]
 where
  objectiveOn o = case o.placement of
    AttachedTo host -> Just host
    _ -> Nothing

{- | The cards attached to the host with the trait: attachments, objectives and encounter
cards attached as conditions.
-}
attachedWithTrait :: forall m. HasGame m => Trait -> Target -> m [Target]
attachedWithTrait trait host = do
  es <- getsGame gameEntities
  let on :: HasField "placement" a Placement => a -> Bool
      on a = a.placement == AttachedTo host
      -- printed or gained, as Mutual Accord gives
      with :: (Targetable a, HasCardDef a) => [a] -> m [Target]
      with = fmap (map toTarget) . filterM (\a -> hasTrait trait (toTarget a) (toCardDef a))
  concat
    <$> sequence
      [ with (filter on $ toList es.attachments)
      , with (filter on $ toList es.objectives)
      , with (filter on $ toList es.treacheries)
      , with (filter on $ toList es.enemies)
      ]

{- | Every card attached to the host: attachments, objectives and encounter cards attached
as conditions.
-}
getAllAttachmentsOn :: HasGame m => Target -> m [Target]
getAllAttachmentsOn host = do
  es <- getsGame gameEntities
  let on :: HasField "placement" a Placement => a -> Bool
      on a = a.placement == AttachedTo host
  pure
    $ [toTarget a | a <- toList es.attachments, on a]
    <> [toTarget o | o <- toList es.objectives, on o]
    <> [toTarget t | t <- toList es.treacheries, on t]
    <> [toTarget e | e <- toList es.enemies, on e]

-- | What the treacheries with the card code are attached to.
hostsOfTreachery :: HasGame m => CardCode -> m [Target]
hostsOfTreachery code = getsGame \g ->
  [host | t <- toList g.entities.treacheries, toCardCode t == code, AttachedTo host <- [t.placement]]

-- | The treacheries with the card code attached to the host.
treacheriesOn :: HasGame m => CardCode -> Target -> m [TreacheryId]
treacheriesOn code host = getsGame \g ->
  [toId t | t <- toList g.entities.treacheries, toCardCode t == code, t.placement == AttachedTo host]

-- | Whether a player has used the ability since its limit last reset.
abilityUsed :: HasGame m => Source -> Int -> m Bool
abilityUsed source idx = getsGame \g ->
  any
    (any (\u -> u.usedAbilitySource == source && u.usedAbilityIndex == idx) . ((.usedAbilities)))
    g.players

-- | Objectives in play with the trait: in the staging area or attached.
objectivesWithTrait :: HasGame m => Trait -> m [Objective]
objectivesWithTrait trait =
  filterM (\o -> hasTrait trait (toTarget o) (toCardDef o))
    =<< getsGame (filter ((/= Unplaced) . (.placement)) . toList . entitiesObjectives . gameEntities)

-- | Players still in the game, in turn order from the first player.
-- | The threat at which the player is eliminated, which may move, as Double Dealings
-- lowers it.
getEliminationLevel :: HasGame m => PlayerId -> m Int
getEliminationLevel pid = do
  mods <- map modifierType . Map.findWithDefault [] (PlayerTarget pid) <$> getsGame gameModifiers
  pure $ 50 + sum [n | EliminationThreatModifier n <- mods]

getRemainingPlayers :: HasGame m => m [PlayerId]
getRemainingPlayers = filterM (fmap (not . (.eliminated)) . getPlayer) =<< getPlayerOrder

-- | The damage on a character, or 0 for anything else.
getDamage :: HasGame m => Target -> m Int
getDamage = \case
  HeroTarget hid -> maybe 0 (.damage) . Map.lookup hid <$> getsGame (entitiesHeroes . gameEntities)
  AllyTarget aid -> maybe 0 (.damage) . Map.lookup aid <$> getsGame (entitiesAllies . gameEntities)
  EnemyTarget eid -> maybe 0 (.damage) . Map.lookup eid <$> getsGame (entitiesEnemies . gameEntities)
  LocationTarget lid -> maybe 0 ((.damage) . toAttrs) . Map.lookup lid <$> getsGame (entitiesLocations . gameEntities)
  -- a side quest, as Protect the Innocent
  QuestTarget qid -> maybe 0 (countTokens Damage . questTokens . toAttrs) . Map.lookup qid <$> getsGame (entitiesQuests . gameEntities)
  -- an objective ally the staging area commits, as Celador
  ObjectiveTarget oid ->
    maybe 0 (countTokens Damage . objectiveTokens . toAttrs) . Map.lookup oid <$> getsGame (entitiesObjectives . gameEntities)
  _ -> pure 0

-- | Characters with damage to heal.
damagedCharacters :: HasGame m => HeroMatcher -> AllyMatcher -> m [Target]
damagedCharacters heroes allies = filterM (fmap (> 0) . getDamage) =<< selectCharacters heroes allies

{- | Condition attachments in play: attachments with the Condition trait, encounter cards
attached to something, which in the core set all count as Conditions, and an enemy
attached as one, as Strangling Python counts as a Condition attachment.
-}
getConditionAttachments :: HasGame m => m [Target]
getConditionAttachments = do
  es <- getsGame gameEntities
  conditions <- filterM (\a -> hasTrait Condition (toTarget a) (toCardDef a)) (toList es.attachments)
  enemies <- filterM (\e -> hasTrait Condition (toTarget e) (toCardDef e)) [e | e <- toList es.enemies, isAttached e.placement]
  pure $ map toTarget conditions <> [toTarget t | t <- toList es.treacheries, isAttached t.placement] <> map toTarget enemies
 where
  isAttached = \case
    AttachedTo _ -> True
    _ -> False

-- | The current quest, as a target: a side quest the players chose to quest against this
-- phase, or else the quest card in play.
getCurrentQuest :: HasGame m => m (Maybe Target)
getCurrentQuest = do
  side <- getCurrentSideQuest
  mains <- getMainQuests
  pure $ side <|> headMay mains

-- | The quest cards in play, side quests left out.
getMainQuests :: HasGame m => m [Target]
getMainQuests = getsGame (map toTarget . filter (not . isSideQuest) . toList . entitiesQuests . gameEntities)

-- | The progress on the main quest, not a side quest's, as "if there are no progress
-- tokens on the main quest" reads.
getMainQuestProgress :: HasGame m => m Int
getMainQuestProgress = getsGame (sum . map (.progress) . filter (not . isSideQuest) . toList . entitiesQuests . gameEntities)

-- | The card is in the victory display, as a player side quest's "while this quest is in
-- the victory display" reads: built from its card there, it gives its modifiers.
inVictoryDisplay :: HasGame m => CardId -> m Bool
inVictoryDisplay cid = getsGame (any ((== cid) . (.id)) . (.victoryDisplay) . toAttrs . gameScenario)

-- | The side quests in the victory display, as "X is the number of side quests in the
-- victory display" reads.
getSideQuestsInVictoryDisplay :: HasGame m => m Int
getSideQuestsInVictoryDisplay = getsGame (length . filter (cardMatches sideQuestCard) . (.victoryDisplay) . toAttrs . gameScenario)

-- | The objectives in play no hero has claimed: those in the staging area.
unclaimedObjectives :: HasGame m => m Int
unclaimedObjectives = getsGame (length . filter ((== StagingArea) . (.placement)) . toList . entitiesObjectives . gameEntities)

-- | The stage of the main quest in play, as "if the players are on stage 3" reads; 0 with
-- none in play.
getMainStage :: HasGame m => m Int
getMainStage =
  getsGame (fromMaybe 0 . headMay . mapMaybe (cdStage . toCardDef) . filter (not . isSideQuest) . toList . entitiesQuests . gameEntities)

-- | The side quest in the staging area chosen as the current quest this phase, if any.
getCurrentSideQuest :: HasGame m => m (Maybe Target)
getCurrentSideQuest = do
  mods <- getsGame gameModifiers
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  pure $ headMay
    [ toTarget q
    | q <- quests
    , isSideQuest q
    , CurrentQuest `elem` map modifierType (Map.findWithDefault [] (toTarget q) mods)
    ]

-- | The side quests in the staging area, the players' and the encounter deck's.
getSideQuests :: HasGame m => m [Target]
getSideQuests = getsGame (map toTarget . filter isSideQuest . toList . entitiesQuests . gameEntities)

-- | The encounter side quests in play, as "if there are no encounter side quests in play"
-- reads.
getEncounterSideQuests :: HasGame m => m [Target]
getEncounterSideQuests = getsGame (map toTarget . filter isEncounterSideQuest . toList . entitiesQuests . gameEntities)

-- | Every quest card in play, the main quest and each side quest, as "X is the number of
-- quest cards in play" reads.
getQuestCards :: HasGame m => m [Target]
getQuestCards = getsGame (map toTarget . toList . entitiesQuests . gameEntities)

-- | The number of quest cards in play.
questCardsInPlay :: HasGame m => m Int
questCardsInPlay = length <$> getQuestCards

-- | The progress placed on the quest card this round, as "if no progress was placed on Lie
-- Low this round" reads.
progressPlacedThisRound :: HasGame m => Target -> m Int
progressPlacedThisRound t = do
  mods <- map modifierType . Map.findWithDefault [] t <$> getsGame gameModifiers
  pure $ sum [n | ProgressPlacedThisRound n <- mods]

-- | The progress placed on the quest card this phase, as "if no progress was placed on
-- Protect the Bree-landers" reads at the end of the quest phase.
progressPlacedThisPhase :: HasGame m => Target -> m Int
progressPlacedThisPhase t = do
  mods <- map modifierType . Map.findWithDefault [] t <$> getsGame gameModifiers
  pure $ sum [n | ProgressPlacedThisPhase n <- mods]

-- | The current quest is a side quest, as "if the current quest is a side quest" reads.
currentQuestIsSideQuest :: HasGame m => m Bool
currentQuestIsSideQuest = isJust <$> getCurrentSideQuest

-- | The encounter discard pile, topmost first.
getEncounterDiscard :: HasGame m => m [EncounterCard]
getEncounterDiscard = getsGame ((.encounterDiscard) . toAttrs . gameScenario)

-- | The top card of the encounter deck, if there is one.
-- | The top N cards of the encounter deck, as "search the top 5 cards" reads.
getTopOfEncounterDeckN :: HasGame m => Int -> m [EncounterCard]
getTopOfEncounterDeckN n = getsGame (take n . (.encounterDeck) . toAttrs . gameScenario)

getTopOfEncounterDeck :: HasGame m => m (Maybe EncounterCard)
getTopOfEncounterDeck = getsGame (headMay . (.encounterDeck) . toAttrs . gameScenario)

-- | Match a card using current game state, including traits granted to heroes.
matchCard :: (HasGame m, HasCardDef a) => CardMatcher -> a -> m Bool
matchCard matcher card = case matcher of
  CardSharesTraitWithHero heroes ->
    anyM (\trait -> selectAny (heroes <> HeroWithTrait trait)) (toList $ cdCardTraits $ toCardDef card)
  NotCard m -> not <$> matchCard m card
  CardMatchAll ms -> allM (`matchCard` card) ms
  CardMatchAny ms -> anyM (`matchCard` card) ms
  _ -> pure $ cardMatches matcher card

-- | The next player clockwise from the player, still in the game; alone, there is none.
getNextPlayer :: HasGame m => PlayerId -> m (Maybe PlayerId)
getNextPlayer pid = do
  players <- getRemainingPlayers
  pure $ find (/= pid) $ take 1 $ drop 1 $ dropWhile (/= pid) (players <> players)

-- | The characters the player controls with the trait, printed or gained, as "each Outlands
-- character you control" reads.
controlledCharactersWithTrait :: HasGame m => PlayerId -> Trait -> m [Target]
controlledCharactersWithTrait pid trait =
  selectCharacters (HeroControlledBy (PlayerWithId pid) <> HeroWithTrait trait) (AllyControlledBy (PlayerWithId pid) <> AllyWithTrait trait)

-- * The Dream-chaser's heading and its ships (Dream-chaser rules)

-- | The heading, once the players have prepared their fleet.
getHeading :: HasGame m => m (Maybe Heading)
getHeading = getsGame ((.heading) . toAttrs . gameScenario)

-- | Whether the heading reads as the matcher says; with no heading, it reads as nothing.
headingMatches :: HasGame m => HeadingMatcher -> m Bool
headingMatches m = maybe False (headingIs m) <$> getHeading

-- | "If you are on-course": the heading is at its best setting.
isOnCourse :: HasGame m => m Bool
isOnCourse = headingMatches OnCourseHeading

-- | "If you are off-course": the heading is at any setting but the best.
isOffCourse :: HasGame m => m Bool
isOffCourse = headingMatches OffCourseHeading

-- | "If your heading is at the worst setting".
atWorstHeading :: HasGame m => m Bool
atWorstHeading = headingMatches WorstHeading

-- | A Ship objective: an objective ally with the Ship trait, which counts as an ally and a
-- character.
shipObjective :: AllyMatcher
shipObjective = ObjectiveAlly <> AllyWithTrait Ship

shipEnemy :: EnemyMatcher
shipEnemy = EnemyWithTrait Ship

-- | The Ship objectives the player controls.
shipObjectivesOf :: HasGame m => PlayerId -> m [AllyId]
shipObjectivesOf pid = select (shipObjective <> AllyControlledBy (PlayerWithId pid))

-- | The enemy's Boarding X: printed and gained, as Calphon's Divination gives it, then
-- raised or lowered, as Cobas Haven reduces it; never below 0.
getBoarding :: HasGame m => EnemyId -> m Int
getBoarding eid = do
  e <- getEnemy eid
  base <- keywordValue (\case Boarding n -> Just n; _ -> Nothing) (toTarget eid) (toCardDef e)
  mods <- map modifierType . Map.findWithDefault [] (toTarget eid) <$> getsGame gameModifiers
  pure $ max 0 $ base + sum [n | BoardingModifier n <- mods]

-- | The location's Aflame X, 0 when it has none (Raid on the Grey Havens).
getAflame :: HasGame m => LocationId -> m Int
getAflame lid = do
  l <- getLocation lid
  keywordValue (\case Aflame n -> Just n; _ -> Nothing) (toTarget lid) (toCardDef l)

-- | Each location in play with the Aflame keyword, with its Aflame X.
getAflameLocations :: HasGame m => m [(LocationId, Int)]
getAflameLocations = filter ((> 0) . snd) <$> (traverse (\lid -> (lid,) <$> getAflame lid) =<< select AnyLocation)
-- | An Uncharted location: one with its Lost Island side faceup (The Grey Havens rules insert).
unchartedLocation :: LocationMatcher
unchartedLocation = LocationWithKeyword Uncharted

-- | The card's printed cost, as "printed cost 2 or less" reads: an X, or a card with none,
-- counts as 0.
printedCostOf :: HasCardDef a => a -> Int
printedCostOf = maybe 0 costAsResources . cdCost . toCardDef

-- | The resources on a card, as it is considered to have them: none while it is
-- considered to have no resources, as Captain's Quarters reads of Raider characters.
resourcesHeld :: (HasGame m, Targetable a) => a -> Int -> m Int
resourcesHeld a n = do
  mods <- map modifierType . Map.findWithDefault [] (toTarget a) <$> getsGame gameModifiers
  pure $ if HasNoResources `elem` mods then 0 else n

{- | Whether the encounter card, by its card id and as in play, guards a card: an objective
guarding it or attached to it, a location or a captive attached to it, or a player card it
holds (see 'GuardPlayerCard').
-}
guardingACard :: HasGame m => CardId -> Target -> m Bool
guardingACard cid target = do
  g <- getGame
  let es = gameEntities g
  captives <- charactersGuardedBy cid
  pure
    $ notNull captives
    || any (\o -> objectiveGuard (toAttrs o) == Just cid || o.placement == AttachedTo target) (entitiesObjectives es)
    || any ((== AttachedTo (CardIdTarget cid)) . (.placement)) (entitiesLocations es)
    || Map.member cid (scenarioGuardedPlayerCards $ toAttrs $ gameScenario g)

-- | The player cards the encounter card, by its card id, guards (see 'GuardPlayerCard'), as
-- Hobgoblin reads the cost of the one it holds.
playerCardsGuardedBy :: HasGame m => CardId -> m [PlayerCard]
playerCardsGuardedBy cid = getsGame (map snd . Map.findWithDefault [] cid . scenarioGuardedPlayerCards . toAttrs . gameScenario)

-- | The player whose hand holds the card, as a treachery there reads it (Lost in the Wild).
handHolding :: HasGame m => CardId -> m (Maybe PlayerId)
handHolding cid = getsGame (fmap toId . find (any ((== cid) . (.id)) . (.hand)) . toList . gamePlayers)

-- | The characters the encounter card guards as captives (see 'GuardCharacter').
charactersGuardedBy :: HasGame m => CardId -> m [Target]
charactersGuardedBy cid = do
  es <- getsGame gameEntities
  pure
    $ [toTarget h | h <- toList (entitiesHeroes es), heroGuardedBy (toAttrs h) == Just cid]
    <> [toTarget a | a <- toList (entitiesAllies es), a.placement == AttachedTo (CardIdTarget cid)]

-- | Every captive an encounter card guards, with the player who owns it: none for an
-- objective ally, an encounter card (see 'GuardCharacter').
guardedCharacters :: HasGame m => m [(Target, Maybe PlayerId)]
guardedCharacters = do
  es <- getsGame gameEntities
  pure
    $ [(toTarget h, Just h.owner) | h <- toList (entitiesHeroes es), isJust (heroGuardedBy (toAttrs h))]
    <> [ (toTarget a, owner (toCard a))
       | a <- toList (entitiesAllies es)
       , AttachedTo (CardIdTarget _) <- [a.placement]
       ]
 where
  owner = \case
    PlayerCard pc -> pc.owner
    EncounterCard _ -> Nothing
