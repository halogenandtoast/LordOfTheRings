{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Matcher (module LordOfTheRings.Matcher, module X) where

import Data.List (elemIndex)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability.Types (UsedAbility (..))
import LordOfTheRings.Ally.Types hiding (ally)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
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
import LordOfTheRings.Helpers.Modifiers (cardsPlayedThisRound, defenseOf, modifiedEngagementCost, getModifiers, hitPointsOf, modifiedValue)
import LordOfTheRings.Hero.Types hiding (hero)
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (..))
import LordOfTheRings.Location.Types hiding (location)
import LordOfTheRings.Matcher.Types as X
import LordOfTheRings.Modifier
import LordOfTheRings.Name
import LordOfTheRings.Objective.Types (Objective, ObjectiveAttrs (objectiveGuard, objectiveTokens))
import LordOfTheRings.Placement
import LordOfTheRings.Quest.Types (Quest, QuestAttrs (questTokens), isEncounterSideQuest, isSideQuest)
import LordOfTheRings.Player.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (GameMode (NightmareMode), scenarioCampaign, scenarioGuardedPlayerCards, scenarioMode)
import LordOfTheRings.Campaign (CampaignLogKey (CardUses), recordedCount, seatUseCode)
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
  PlayerAtValourThreat -> do
    dire <- mainQuestIsDire
    (>= if dire then 80 else 40) <$> getThreat player.id
  ControllerOfHero hid -> selectAny (HeroWithId hid <> HeroControlledBy (PlayerWithId player.id))
  PlayerWithCardsInHand n -> pure $ length player.hand >= n
  PlayerMatchAll ms -> allM (`matchPlayer` player) ms
  PlayerWithCardsInDeck n -> pure $ length player.deck >= n
  PlayerUsingErrata -> pure player.errata
  PlayerWithCampaignCardUsesBelow code n -> do
    l <- getsGame (scenarioCampaign . toAttrs . gameScenario)
    pure $ maybe False (\lg -> recordedCount CardUses code lg < n) l
  PlayerWithCampaignUsesBelow code n -> do
    seat <- elemIndex player.id <$> getsGame gamePlayerOrder
    l <- getsGame (scenarioCampaign . toAttrs . gameScenario)
    pure $ case (seat, l) of
      (Just s, Just lg) -> recordedCount CardUses (seatUseCode code s) lg < n
      _ -> False
  PlayerWithThreatAtLeastEngagementCostOf eid -> getsGame (Map.lookup eid . (.enemies) . gameEntities) >>= \case
    Nothing -> pure False
    Just e -> (<=) <$> modifiedEngagementCost (toTarget eid) e.engagementCost <*> getThreat player.id
  PlayerAtThisStage -> elem player.id <$> getStagePlayers
  PlayerWithHiddenCards -> getsGame (notNull . Map.findWithDefault [] player.id . (.hiddenCards) . toAttrs . gameScenario)
  PlayerEngagedWith m -> selectAny (m <> EnemyEngagedWith (PlayerWithId player.id))

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
  HeroWithSphere sphere -> heroHasResourceIcon sphere hero
  HeroWithKeyword k -> hasKeyword k (toTarget hero) (toCardDef hero)
  HeroWithPrintedKeyword k -> pure $ k `member` cdKeywords (toCardDef hero)
  ReadyHero -> pure $ not (heroExhausted $ toAttrs hero)
  ExhaustableHero -> (not (heroExhausted $ toAttrs hero) &&) . notElem CannotExhaust <$> getModifiers (toTarget hero)
  HeroCanHaveEncounterAttachment traits -> do
    mods <- getModifiers (toTarget hero)
    pure $ CannotHaveAttachments `notElem` mods && all (`member` traits) [trait | CannotHaveAttachmentsWithoutTrait trait <- mods]
  DamagedHero -> pure $ hero.damage > 0
  ExhaustedHero -> pure $ heroExhausted (toAttrs hero)
  PoisonedHero -> pure $ notNull (heroPoison $ toAttrs hero)
  UnconsciousHero -> pure $ heroUnconscious (toAttrs hero)
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
  -- an objective attached counts, as the saga's The One Ring on the Ring-bearer
  -- an objective too, as a saga's The One Ring is
  HeroWithReadyAttachment code ->
    getsGame \g ->
      any (\a -> a.placement == AttachedTo (toTarget hero) && toCardCode a == code && not (attachmentExhausted $ toAttrs a)) (entitiesAttachments $ gameEntities g)
        || any (\o -> o.placement == AttachedTo (toTarget hero) && toCardCode o == code && not o.exhausted) (entitiesObjectives $ gameEntities g)
  HeroWithResources -> pure $ hero.resources > 0
  HeroWithPrintedSphere sphere -> pure $ hero.sphere == sphere
  HeroWithResourcesAtLeast n -> pure $ hero.resources >= n
  HeroWithRestrictedAttachmentsAtLeast n -> (>= n) <$> restrictedAttachmentsOn (toTarget hero)
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
  PoisonedAlly -> pure $ notNull (allyPoison $ toAttrs ally)
  UnconsciousAlly -> pure $ allyUnconscious (toAttrs ally)
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
    enemies <-
      getsGame (filter (not . attached . (.placement)) . toList . entitiesEnemies . gameEntities)
    map toId
      <$> filterM
        ( \e ->
            andM
              [ pure (isNothing (toAttrs e).enemyAttackLocation || namesEnemy matcher (toId e))
              , notOutOfPlay (toTarget e)
              , matchEnemy matcher e
              ]
        )
        enemies
   where
    namesEnemy m eid = case m of
      EnemyWithId eid' -> eid == eid'
      EnemyMatchAll ms -> any (`namesEnemy` eid) ms
      _ -> False
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
  EnemyWithTitle title -> pure $ cardMatches (CardWithTitle title) enemy
  EnemyWithTitleContaining part -> let Name title _ = cdName (toCardDef enemy) in pure $ part `isInfixOf` title
  EnemyWithCardId cid -> pure $ (toCard enemy).id == cid
  EnemyWithFacedownShadow -> any ((`notElem` (toAttrs enemy).enemyShadowRevealed) . (.id)) <$> shadowsPlayerCardsCanAffect enemy
  EnemyWithRevealedShadow -> pure $ notNull (toAttrs enemy).enemyShadowRevealed
  EnemyEngagedWith pm | isJust (toAttrs enemy).enemyAttackLocation ->
    maybe (pure False) (\pid -> elem pid <$> select pm) (toAttrs enemy).enemyOwner
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
  EnemyGuardingAHero -> any (\case HeroTarget _ -> True; _ -> False) <$> charactersGuardedBy (toCard enemy).id
  EnemyWithNoHitPointsRemaining -> (enemy.damage >=) <$> modifiedValue hitPointsOf (toTarget enemy) enemy.hitPoints
  EnemyGrappledWith lm -> case enemy.placement of
    AttachedTo (LocationTarget lid) -> elem lid <$> select lm
    _ -> pure False
  EnemyWithEngagementCostAboveThreatOf pid -> do
    threat <- getThreat pid
    (> threat) <$> modifiedEngagementCost (toTarget enemy) enemy.engagementCost
  AnyEnemy -> pure True
  NotEnemy m -> not <$> matchEnemy m enemy
  EnemyMatchAll ms -> allM (`matchEnemy` enemy) ms
  EnemyWithKeyword k -> hasKeyword k (toTarget enemy) (toCardDef enemy)
  EnemyWithDefense n -> (== n) <$> getEnemyDefense (toId enemy)

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

{- | The shadow cards dealt to the enemy that a player card can choose or affect: not one
immune to player card effects, as Draw Her Fire makes those dealt to Dagnir. Immunity
protects the shadow card itself, whatever the enemy is (Rules Reference, "Immune").
-}
shadowsPlayerCardsCanAffect :: HasGame m => Enemy -> m [EncounterCard]
shadowsPlayerCardsCanAffect enemy = filterM (fmap not . immuneToPlayerCards . CardIdTarget . (.id)) enemy.shadowCards

immuneToPlayerCards :: HasGame m => Target -> m Bool
immuneToPlayerCards t =
  elem ImmuneToPlayerCardEffects
    . map modifierType
    . Map.findWithDefault [] t
    <$> getsGame gameModifiers

-- | A unique card cannot enter play while a card with its title is in play.
passesUnique :: HasGame m => Card -> m Bool
passesUnique = passesUniqueWith AnyHero

-- | The unique rule for a hero coming into play as the given hero leaves it, as Helm of
-- Secrecy swaps them: the leaving hero's title does not bar it.
passesUniqueReplacing :: HasGame m => HeroId -> Card -> m Bool
passesUniqueReplacing hid = passesUniqueWith (not_ $ HeroWithId hid)

passesUniqueWith :: HasGame m => HeroMatcher -> Card -> m Bool
passesUniqueWith others card
  | card.unique =
      andM
        [ selectNone $ AllyWithTitle card.name
        , selectNone $ HeroWithTitle card.name <> others
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

-- | A current printed or gained icon. Removing an icon wins over gaining it.
heroHasResourceIcon :: HasGame m => Sphere -> Hero -> m Bool
heroHasResourceIcon sphere h = do
  own <- map modifierType . Map.findWithDefault [] (toTarget h) <$> getsGame gameModifiers
  pure $ LoseResourceIcon sphere `notElem` own && (h.sphere == sphere || GainResourceIcon sphere `elem` own)

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
      , LoseResourceIcon sphere `notElem` own && (h.sphere == sphere || GainResourceIcon sphere `elem` own)
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
  entryBarred <- getsGame (allyEntryBarred pid)
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
  entryBarred <- getsGame (allyEntryBarred pid)
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

-- | Burn values from multiple sources add, including West Pier.
getBurn :: HasGame m => Target -> CardDef -> m Int
getBurn = keywordValue \case Burn n -> Just n; _ -> Nothing

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
  -- sharing the game split between staging areas, an enemy engaged with a player at
  -- another stage adds to that stage's
  atStage <- getStagePlayers
  let here = \case
        EngagedWith p -> p `elem` atStage
        p -> inPlay p
  enemies <- getsGame (filter (here . (.placement)) . toList . entitiesEnemies . gameEntities)
  locations <- getsGame (filter (inPlay . (.placement)) . toList . entitiesLocations . gameEntities)
  fromEnemies <- for enemies \e -> getArchery (toTarget e) (toCardDef e)
  fromLocations <- for locations \l -> getArchery (toTarget l) (toCardDef l)
  onQuest <- getCurrentQuest >>= \case
    Just t@(QuestTarget qid) -> maybe (pure 0) (getArchery t . toCardDef) =<< getsGame (Map.lookup qid . entitiesQuests . gameEntities)
    _ -> pure 0
  bonus <- sum . (\ms -> [n | ArcheryBonus n <- map modifierType ms] <> [n | ArcheryBonusAtStage p n <- map modifierType ms, p `elem` atStage]) . Map.findWithDefault [] GameTarget <$> getsGame gameModifiers
  -- a reduction, as The Hornburg's, takes it no lower than 0
  pure $ max 0 $ sum fromEnemies + sum fromLocations + onQuest + bonus

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

-- | The "you" of an encounter card being revealed: in The Lord of the Rings saga's staging
-- step the player revealing it, otherwise the first player at the stage in focus (see
-- 'getStageLead').
getRevealer :: HasGame m => m PlayerId
getRevealer = maybe getStageLead pure =<< getsGame gameRevealer

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

{- | The players at the stage in focus, in turn order: while the players share the game
split between staging areas (Breaking of the Fellowship), those of the area in focus,
otherwise every player, as 'getPlayerOrder'.
-}
getStagePlayers :: HasGame m => m [PlayerId]
getStagePlayers = do
  order <- getPlayerOrder
  sep <- getsGame gameSeparation
  pure $ case sep of
    Just s | separationShared s, Just a <- find areaInFocus (separationAreas s) -> filter (`elem` areaPlayers a) order
    _ -> order

-- | The first of the players at the stage in focus: see 'getStagePlayers'.
getStageLead :: HasGame m => m PlayerId
getStageLead = maybe getLead pure . headMay =<< getStagePlayers

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

-- | How many restricted cards are attached to the host: attachments printed with the
-- keyword or given it, as Wild Stallion's Mount text gives it, and claimed restricted
-- objectives, as the restricted limit counts them.
restrictedAttachmentsOn :: HasGame m => Target -> m Int
restrictedAttachmentsOn host = getsGame \g ->
  let es = gameEntities g
      on' :: HasField "placement" a Placement => a -> Bool
      on' x = x.placement == AttachedTo host
      printed :: HasCardDef a => a -> Bool
      printed = member Restricted . cdKeywords . toCardDef
      gained t = GainKeyword Restricted `elem` map modifierType (Map.findWithDefault [] t (gameModifiers g))
   in length [() | a <- toList (entitiesAttachments es), on' a, printed a || gained (toTarget a)]
        + length [() | o <- toList (entitiesObjectives es), on' o, printed o]

-- | The player cards attached facedown to the character, as Eagles of the Misty Mountains
-- holds them; they count as attachments.
facedownAttachmentsOn :: HasGame m => Target -> m [PlayerCard]
facedownAttachmentsOn host = getsGame \g -> case host of
  AllyTarget aid -> maybe [] (allyFacedown . toAttrs) (Map.lookup aid g.entities.allies)
  HeroTarget hid -> maybe [] (heroFacedown . toAttrs) (Map.lookup hid g.entities.heroes)
  _ -> []

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

-- | The ready attachments with the trait on the host, as "exhaust a Weapon card attached to
-- that hero" reads.
readyAttachedWithTrait :: HasGame m => Trait -> Target -> m [Target]
readyAttachedWithTrait trait host = do
  attached <- attachedWithTrait trait host
  es <- getsGame gameEntities
  pure [t | t@(AttachmentTarget aid) <- attached, maybe False (not . attachmentExhausted . toAttrs) (Map.lookup aid es.attachments)]

-- | The ready attachments with the trait the player controls, wherever they are attached,
-- as "exhaust a Weapon attachment" reads.
readyControlledAttachmentsWithTrait :: HasGame m => PlayerId -> Trait -> m [Target]
readyControlledAttachmentsWithTrait pid trait = do
  es <- getsGame gameEntities
  map toTarget
    <$> filterM
      (\a -> hasTrait trait (toTarget a) (toCardDef a))
      [a | a <- toList es.attachments, (toAttrs a).controller == pid, not (attachmentExhausted $ toAttrs a)]

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
-- | The One Ring in play: only one copy ever is (A Shadow in the East rules, "The One
-- Ring & The Master Trait").
getTheOneRing :: HasGame m => m (Maybe AttachmentId)
getTheOneRing = getsGame (fmap toId . find ((== toCardCode Attachments.theOneRing) . toCardCode) . toList . entitiesAttachments . gameEntities)

-- | Bilbo Baggins, while he is in play: in The Hobbit's scenarios only the Baggins sphere
-- Bilbo may be (The Hobbit: Over Hill and Under Hill rules insert, "Bilbo Baggins").
getBilbo :: HasGame m => m (Maybe HeroId)
getBilbo = headMay <$> select (HeroWithTitle "Bilbo Baggins")

-- | The Baggins resources: those in Bilbo Baggins' resource pool.
getBagginsResources :: HasGame m => m Int
getBagginsResources = maybe (pure 0) (fmap (.resources) . getHero) =<< getBilbo

-- | Whether the player controls The One Ring, as Master cards may only be used by him.
-- It is controlled by whoever controls the hero it is attached to (FAQ 1.38).
controlsTheOneRing :: HasGame m => PlayerId -> m Bool
controlsTheOneRing pid = selectAny (heroWithTheOneRing <> HeroControlledBy (PlayerWithId pid))

getEliminationLevel :: HasGame m => PlayerId -> m Int
getEliminationLevel pid = do
  mods <- map modifierType . Map.findWithDefault [] (PlayerTarget pid) <$> getsGame gameModifiers
  dire <- mainQuestIsDire
  -- Dire: each player's threat elimination level "is increased to 99" (The Mountain of
  -- Fire rules insert). Read literally it is set to 99, so modifiers that move the level,
  -- as The One Ring's -5, do not apply while the main quest is dire. No ruling found.
  pure $ if dire then 99 else 50 + sum [n | EliminationThreatModifier n <- mods]

-- | The main quest has the dire keyword (The Mountain of Fire rules insert, "Dire").
mainQuestIsDire :: HasGame m => m Bool
mainQuestIsDire = do
  quests <- getsGame (filter (not . isSideQuest) . toList . entitiesQuests . gameEntities)
  anyM (\q -> hasKeyword Dire (toTarget q) (toCardDef q)) quests

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
attached to something, which in the core set all count as Conditions, and an enemy or an
objective attached as one, as Strangling Python and Grievous Wound count as a Condition
attachment.
-}
getConditionAttachments :: HasGame m => m [Target]
getConditionAttachments = do
  es <- getsGame gameEntities
  conditions <- filterM (\a -> hasTrait Condition (toTarget a) (toCardDef a)) (toList es.attachments)
  enemies <- filterM (\e -> hasTrait Condition (toTarget e) (toCardDef e)) [e | e <- toList es.enemies, isAttached e.placement]
  objectives <- filterM (\o -> hasTrait Condition (toTarget o) (toCardDef o)) [o | o <- toList es.objectives, isAttached o.placement]
  -- Older attached treacheries implement their Condition text through attachment
  -- placement. Adaptation burdens explicitly have another attachment trait instead.
  treacheries <- filterM (\t -> do
    adaptation <- hasTrait Adaptation (toTarget t) (toCardDef t)
    condition <- hasTrait Condition (toTarget t) (toCardDef t)
    pure $ not adaptation || condition) [t | t <- toList es.treacheries, isAttached t.placement]
  pure
    $ map toTarget conditions
    <> map toTarget treacheries
    <> map toTarget enemies
    <> map toTarget objectives
    -- poison, facedown on characters (The Hobbit: On the Doorstep rules insert)
    <> [CardIdTarget c.id | h <- toList es.heroes, c <- heroPoison (toAttrs h)]
    <> [CardIdTarget c.id | a <- toList es.allies, c <- allyPoison (toAttrs a)]
 where
  isAttached = \case
    AttachedTo _ -> True
    _ -> False

-- | The current quest, as a target: a side quest the players chose to quest against this
-- phase, or else the quest card in play, the one chosen while several are.
getCurrentQuest :: HasGame m => m (Maybe Target)
getCurrentQuest = do
  chosen <- getChosenQuest
  mains <- getMainQuests
  pure $ chosen <|> headMay mains

{- | The quest card chosen as the current quest: a side quest chosen for the phase, else
one of several main quests in play chosen for the round, as The Battle of Five Armies'
stages 2 to 4 are (The Hobbit: On the Doorstep rules insert, "Current Quest Stage").
-}
getChosenQuest :: HasGame m => m (Maybe Target)
getChosenQuest = do
  side <- getCurrentSideQuest
  mods <- getsGame gameModifiers
  quests <- getsGame (toList . entitiesQuests . gameEntities)
  let main = headMay [toTarget q | q <- quests, not (isSideQuest q), CurrentQuest `elem` map modifierType (Map.findWithDefault [] (toTarget q) mods)]
  pure $ side <|> main

-- | The quest cards in play, side quests left out.
getMainQuests :: HasGame m => m [Target]
getMainQuests = map toTarget <$> getMainQuestCards

{- | The main quest cards in play: neither a side quest nor a stage that cannot be the
current quest, as Riddles in the Dark in the riddle area beside the quest stage the players
quest against (Dungeons Deep and Caverns Dim).
-}
getMainQuestCards :: HasGame m => m [Quest]
getMainQuestCards = getsGame \g ->
  [ q
  | q <- toList (entitiesQuests $ gameEntities g)
  , not (isSideQuest q)
  , CannotBeCurrentQuest `notElem` map modifierType (Map.findWithDefault [] (toTarget q) (gameModifiers g))
  ]

-- | The progress on the main quest, not a side quest's, as "if there are no progress
-- tokens on the main quest" reads.
getMainQuestProgress :: HasGame m => m Int
getMainQuestProgress = sum . map (.progress) <$> getMainQuestCards

-- | The resources on the main quest, as The City of Ulfast's "X is 1 more than the number of
-- resources on the main quest" reads.
getMainQuestResources :: HasGame m => m Int
getMainQuestResources = sum . map (countTokens Resource . questTokens . toAttrs) <$> getMainQuestCards

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

getEnemyDefense :: (HasGame m) => EnemyId -> m Int
getEnemyDefense eid = do
  enemy <- getEnemy eid
  -- one whose defense cannot be reduced, as Rupert, ignores what would
  floored <- elem CannotHaveDefenseReduced <$> getModifiers eid
  modifiedValue (if floored then \m -> defenseOf m >>= \n -> n <$ guard (n > 0) else defenseOf) (toTarget eid) enemy.defense

-- | The Dwarf characters the player controls, as "if you control at least 5 Dwarf
-- characters" counts them: Bombur counts as two.
dwarfCount :: HasGame m => PlayerId -> m Int
dwarfCount pid = do
  dwarves <- controlledCharactersWithTrait pid Dwarf
  sum <$> for dwarves \d -> (\two -> if two then 2 else 1) . elem CountsAsTwoDwarves <$> getModifiers d

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

-- | Pillage values stack just like Archery and Boarding; blanking removes printed values.
getPillage :: HasGame m => EnemyId -> m Int
getPillage eid = do
  e <- getEnemy eid
  keywordValue (\case Pillage n -> Just n; _ -> Nothing) (toTarget eid) (toCardDef e)

-- | The location's Aflame X, 0 when it has none (Raid on the Grey Havens).
getAflame :: HasGame m => LocationId -> m Int
getAflame lid = do
  l <- getLocation lid
  keywordValue (\case Aflame n -> Just n; _ -> Nothing) (toTarget lid) (toCardDef l)

-- | The location's Mire X, 0 when it has none (The Land of Shadow rules insert).
getMire :: HasGame m => LocationId -> m Int
getMire lid = do
  l <- getLocation lid
  keywordValue (\case Mire n -> Just n; _ -> Nothing) (toTarget lid) (toCardDef l)

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
