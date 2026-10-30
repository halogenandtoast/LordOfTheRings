module LordOfTheRings.Helpers.Window (module LordOfTheRings.Helpers.Window) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability
import LordOfTheRings.Ally.Types (allyTokens)
import LordOfTheRings.Attachment.Types
import LordOfTheRings.Card (cdCardTraits, cdHitPoints, cdName, cdVictoryPoints, cdWhenRevealed, toCardCode, toCardDef)
import LordOfTheRings.Effect (Effect (..))
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Enemy.Types (enemyCardId)
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Surge))
import LordOfTheRings.Location.Types (locationCardId)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Modifier (
  ModifierType (Blank, BlankAttachments, CannotBeTraveledTo, CannotCancelEncounterEffects, CannotExhaust, CannotTriggerAbilities, Defending, WhenRevealedCannotBeCanceled),
  ModifierType (Blank, BlankAttachments, CannotBeTraveledTo, CannotCancelEncounterEffects, CannotExhaust, CannotTriggerAbilities, StagingStepBegun, TravelableWithActiveLocation),
 )
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

getAllAbilities :: HasGame m => m [Ability]
getAllAbilities = do
  es <- getsGame gameEntities
  -- an ally that just left play answers only its own leaving, as Arkham's cards in discard
  departed <- getsGame gameLeftPlay
  let leftPlayAbilities =
        [ ab
        | a <- toList departed
        , ab <- getAbilities a
        , Just (LeftPlayWindow _ t) <- [abilityWindow ab]
        , t == toTarget a
        ]
  -- attachments whose text is blank have no abilities
  blank <- elem BlankAttachments <$> getModifiers GameTarget
  let abilities =
        concat
          [ concatMap getAbilities (toList es.heroes)
          , concatMap getAbilities (toList es.allies)
          , concatMap getAbilities (toList es.enemies)
          , concatMap getAbilities (toList es.locations)
          , concatMap getAbilities (toList es.treacheries)
          , concatMap getAbilities (toList es.quests)
          , if blank then [] else concatMap getAbilities (toList es.attachments)
          , concatMap getAbilities (toList es.objectives)
          , leftPlayAbilities
          ]
  -- a card a condition has blanked offers none
  flip filterM abilities \ab ->
    not . any (`elem` [CannotTriggerAbilities, Blank]) <$> maybe (pure []) getModifiers (sourceTarget ab.abilitySource)

-- | Abilities whose window matches one of the windows and that can be used right now.
getWindowAbilities :: HasGame m => [Window] -> m [Ability]
getWindowAbilities windows = do
  abilities <- getAllAbilities
  flip filterM abilities \ab -> case abilityWindow ab of
    Just matcher ->
      andM
        [ anyM (windowMatchesM matcher) windows
        , passesCriteriaFrom ab.abilitySource ab.abilityCriteria
        , canPay (abilityCost ab)
        , withinLimit ab
        ]
    _ -> pure False

-- | Whether the window matches, reading the game for the matchers that need it.
windowMatchesM :: HasGame m => WindowMatcher -> Window -> m Bool
windowMatchesM matcher w@(Window timing wType) = case (matcher, wType) of
  (LeftPlaySharingTraitWindow t host, LeftPlay _ _ traits) | t == timing -> do
    def <- hostDef host
    anyM (\trait -> hasTrait trait host def) (toList traits)
  (EnemyDestroyedByHeroWindow t m, EnemyDestroyedByAttack _ attackers _)
    | t == timing ->
        anyM (\case HeroTarget hid -> selectAny (HeroWithId hid <> m); _ -> pure False) attackers
  (EnemyWithTraitRevealedWindow t trait, Revealed (EnemyTarget eid)) | t == timing -> selectAny (EnemyWithId eid <> EnemyWithTrait trait)
  (EnemyWithTraitEntersStagingAreaWindow t trait, EntersStagingArea (EnemyTarget eid)) | t == timing -> selectAny (EnemyWithId eid <> EnemyWithTrait trait)
  (TreacheryWithTraitRevealedWindow t trait, Revealed target@(TreacheryTarget tid)) | t == timing ->
    maybe (pure False) (hasTrait trait target . toCardDef) . Map.lookup tid . (.treacheries) =<< getsGame gameEntities
  (EnemyWithModifierEngagedWindow t m, EnemyEngaged eid _) | t == timing -> elem m . Map.findWithDefault [] (toTarget eid) <$> getsGame gameModifiers
  -- only a card with When Revealed effects leaves A Test of Will something to cancel
  (CardRevealedWindow t, Revealed target) | t == timing -> do
    es <- getsGame gameEntities
    cancelable <- notElem WhenRevealedCannotBeCanceled <$> getModifiers target
    pure $ cancelable && maybe False cdWhenRevealed case target of
      EnemyTarget i -> toCardDef <$> Map.lookup i es.enemies
      LocationTarget i -> toCardDef <$> Map.lookup i es.locations
      TreacheryTarget i -> toCardDef <$> Map.lookup i es.treacheries
      ObjectiveTarget i -> toCardDef <$> Map.lookup i es.objectives
      QuestTarget i -> toCardDef <$> Map.lookup i es.quests
      _ -> Nothing
  -- only while the enemy is still in play, to be dealt more
  (AttackDealtDamageWithTraitWindow t trait, AttackDealtDamage eid attackers _) | t == timing ->
    andM
      [ selectAny (EnemyWithId eid)
      , flip anyM attackers \case
          HeroTarget hid -> selectAny (HeroWithId hid <> HeroWithTrait trait)
          AllyTarget aid -> selectAny (AllyWithId aid <> AllyWithTrait trait)
          _ -> pure False
      ]
  (TraveledToWindow t m, Traveled lid) | t == timing -> selectAny (LocationWithId lid <> m)
  (LocationFlippedWindow t m, LocationFlipped lid) | t == timing -> selectAny (LocationWithId lid <> m)
  (LocationBecameActiveWindow t m, BecameActiveLocation lid) | t == timing -> selectAny (LocationWithId lid <> m)
  (EnemyAttacksPlayerWindow t pid m, EnemyAttacking eid pid') | t == timing && pid == pid' -> selectAny (EnemyWithId eid <> m)
  (MatchingEnemyAttacksWindow t m, EnemyAttacking eid _) | t == timing -> selectAny (EnemyWithId eid <> m)
  (PlayerDiscardedFromDeckWindow t m, DiscardedFromDeck pid _) | t == timing -> selectAny (PlayerWithId pid <> m)
  -- the attachment played is in play by now, on its host
  (AttachmentPlayedOnHeroWindow t pid, PlayedFromHand (CardIdTarget cid)) | t == timing -> do
    es <- getsGame gameEntities
    case find ((== cid) . (.cardId) . toAttrs) (toList es.attachments) of
      Just a | (toAttrs a).owner /= pid, AttachedTo (HeroTarget hid) <- a.placement ->
        selectAny (HeroWithId hid <> HeroControlledBy (PlayerWithId pid))
      _ -> pure False
  (ExploredWithoutVictoryWindow t, Explored lid) | t == timing -> isNothing . cdVictoryPoints . toCardDef <$> getLocation lid
  (ActiveLocationExploredWindow t, Explored lid) | t == timing -> selectAny (LocationWithId lid <> IsActiveLocation)
  (CharacterWithTraitDealtDamageWindow t trait, DealtDamage target _) | t == timing -> case target of
    HeroTarget hid -> selectAny (HeroWithId hid <> HeroWithTrait trait)
    AllyTarget aid -> selectAny (AllyWithId aid <> AllyWithTrait trait)
    _ -> pure False
  (HeroGainedResourcesWindow t pid, GainedResources (HeroTarget hid)) | t == timing ->
    selectAny (HeroWithId hid <> HeroControlledBy (PlayerWithId pid))
  -- surge printed or gained, read off the card as it was revealed
  (SurgeCardRevealedWindow t, Revealed target) | t == timing -> do
    es <- getsGame gameEntities
    let def = case target of
          EnemyTarget i -> toCardDef <$> Map.lookup i es.enemies
          LocationTarget i -> toCardDef <$> Map.lookup i es.locations
          TreacheryTarget i -> toCardDef <$> Map.lookup i es.treacheries
          QuestTarget i -> toCardDef <$> Map.lookup i es.quests
          _ -> Nothing
    maybe (pure False) (hasKeyword Surge target) def
  (HeroDefenderDeclaredWindow t m other, DeclaredAsDefender (HeroTarget hid) eid) | t == timing ->
    andM [selectAny (HeroWithId hid <> m), selectAny (other <> not_ (EnemyWithId eid))]
  -- only while the enemy is still in play
  (AttackDealtDamageByWindow t who, AttackDealtDamage eid attackers n)
    | t == timing && who `elem` attackers && n > 0 -> selectAny (EnemyWithId eid)
  -- the enemy is gone; its card is read from what this attack destroyed
  (EnemyWithPrintedHitPointsDestroyedByAttackWindow t n who, EnemyDestroyedByAttack eid attackers _)
    | t == timing && who `elem` attackers -> do
        destroyed <- getsGame gameAttackDestroyed
        pure $ any (\d -> d.target == EnemyTarget eid && maybe False (>= n) (cdHitPoints $ toCardDef d.card)) destroyed
  (ExploredMatchingWindow t m, Explored lid) | t == timing -> cardMatches m . toCardDef <$> getLocation lid
  (HeroDeclaredAsDefenderWindow t m, DeclaredAsDefender (HeroTarget hid) _) | t == timing -> selectAny (HeroWithId hid <> m)
  (CharacterDefenderDeclaredWindow t hm am em, DeclaredAsDefender c eid) | t == timing -> do
    character <- case c of
      HeroTarget hid -> selectAny (HeroWithId hid <> hm)
      AllyTarget aid -> selectAny (AllyWithId aid <> am)
      _ -> pure False
    andM [pure character, selectAny (EnemyWithId eid <> em)]
  (RevealedTitleInVictoryDisplayWindow t, Revealed target) | t == timing -> do
    es <- getsGame gameEntities
    shown <- getsGame (map (title . toCardDef) . (.victoryDisplay) . toAttrs . gameScenario)
    pure $ maybe False ((`elem` shown) . title) case target of
      EnemyTarget i -> toCardDef <$> Map.lookup i es.enemies
      LocationTarget i -> toCardDef <$> Map.lookup i es.locations
      TreacheryTarget i -> toCardDef <$> Map.lookup i es.treacheries
      ObjectiveTarget i -> toCardDef <$> Map.lookup i es.objectives
      QuestTarget i -> toCardDef <$> Map.lookup i es.quests
      _ -> Nothing
  (ThreatRaisedWindow t m, ThreatRaised pid) | t == timing -> selectAny (PlayerWithId pid <> m)
  -- the character is gone: it was defending as the attack's effects still say, and its
  -- traits are its card's, read from what the attack destroyed
  (DefenderWithTraitDestroyedWindow t trait, CharacterDestroyedByAttack _ target _) | t == timing -> do
    effects <- getsGame gameEffects
    destroyed <- getsGame gameAttackDestroyed
    pure
      $ any (\e -> e.effectTarget == target && Defending `elem` e.effectModifiers) effects
      && any (\d -> d.target == target && trait `member` cdCardTraits (toCardDef d.card)) destroyed
  (PlayerCommittedCharactersWindow t pid, CommittedToQuest target) | t == timing -> (== Just pid) <$> characterController target
  (ErrataWindow pid errata printed, _) -> do
    followsErrata <- selectAny (PlayerWithId pid <> PlayerUsingErrata)
    windowMatchesM (if followsErrata then errata else printed) w
  _ -> pure $ windowMatches matcher w
 where
  title def = case cdName def of Name t _ -> t
  hostDef = \case
    HeroTarget hid -> toCardDef <$> getHero hid
    AllyTarget aid -> toCardDef <$> getAlly aid
    _ -> error "not a character"

{- | Whether an ability's limit leaves it a use: its controller's uses for a player
limit, everyone's for a group limit.
-}
withinLimit :: HasGame m => Ability -> m Bool
withinLimit ab = withinLimitOf ab =<< abilityController ab

{- | As 'withinLimit', for the player who would use it: one any player may trigger is
limited by each player's own uses.
-}
withinLimitFor :: HasGame m => PlayerId -> Ability -> m Bool
withinLimitFor pid ab
  | ab.abilityAnyPlayer = withinLimitOf ab (Just pid)
  | otherwise = withinLimit ab

withinLimitOf :: HasGame m => Ability -> Maybe PlayerId -> m Bool
withinLimitOf ab controller = case ab.abilityLimit of
  NoLimit -> pure True
  PlayerLimit _ n -> do
    players <- getsGame (toList . gamePlayers)
    source <- limitSource ab
    pure $ uses source [p | p <- players, Just p.id == controller] < n
  GroupLimit _ n -> do
    source <- limitSource ab
    (< n) . uses source . toList <$> getsGame gamePlayers
 where
  uses source players =
    sum
      [ u.usedTimes
      | p <- players
      , u <- p.usedAbilities
      , u.usedAbilitySource == source
      , u.usedAbilityIndex == ab.abilityIndex
      ]

{- | The source an ability's uses are counted against: a once-per-game limit is counted
across every copy of the card, as the card coming back into play, or another copy of it,
is limited by the uses before (each player's own, for a player limit).
-}
limitSource :: HasGame m => Ability -> m Source
limitSource ab = case abilityLimitType ab.abilityLimit of
  Just PerGame -> do
    es <- getsGame gameEntities
    let code = case ab.abilitySource of
          AllySource aid -> toCardCode <$> Map.lookup aid es.allies
          HeroSource hid -> toCardCode <$> Map.lookup hid es.heroes
          AttachmentSource aid -> toCardCode <$> Map.lookup aid es.attachments
          _ -> Nothing
    pure $ maybe ab.abilitySource CardCodeSource code
  _ -> pure ab.abilitySource

passesCriteria :: HasGame m => Criterion -> m Bool
passesCriteria = \case
  NoCriteria -> pure True
  LocationExists m -> selectAny m
  EnemyExists m -> selectAny m
  HeroExists m -> selectAny m
  AllyExists m -> selectAny m
  PlayerExists m -> selectAny m
  DuringPhase phase -> (== phase) <$> getsGame gamePhase
  CardNotInPlay cid -> do
    es <- getsGame gameEntities
    pure
      $ cid
      `notElem` map (enemyCardId . toAttrs) (toList es.enemies)
      && cid
      `notElem` map (locationCardId . toAttrs) (toList es.locations)
  CardInHand pid cType -> anyM passesUnique . filter ((== cType) . (.cardType)) . (.hand) =<< getPlayer pid
  CardInHandMatching pid matcher -> do
    cards <- filterM (matchCard matcher) . (.hand) =<< getPlayer pid
    anyM passesUnique cards
  ResourcesOfSphere pm sphere n -> anyM (\p -> (>= n) <$> getResourcesOfSphere p sphere) =<< select pm
  AffordableAllyInDiscard pid sphere -> notNull <$> affordableDiscardedAllies pid sphere
  AffordableAllyInHand pid sphere reduction minimumCost -> notNull <$> affordableAlliesInHand pid sphere reduction minimumCost
  BeforeCommitting ->
    andM [(== QuestPhase) <$> getsGame gamePhase, selectNone CommittedHero, selectNone CommittedAlly]
  BeforeStaging ->
    andM [(== QuestPhase) <$> getsGame gamePhase, notElem StagingStepBegun <$> getModifiers GameTarget]
  EncounterEffectsCancelable -> notElem CannotCancelEncounterEffects <$> getModifiers GameTarget
  NotCriterion c -> not <$> passesCriteria c
  TargetHasModifier t m -> elem m <$> getModifiers t
  TargetLacksModifier t m -> notElem m <$> getModifiers t
  CharactersAtLeast n heroes allies -> (>= n) . length <$> selectCharacters heroes allies
  DistinctCharacters characterPairs -> distinct [] <$> traverse (uncurry selectCharacters) characterPairs
  CardInVictoryDisplay m -> getsGame (any (cardMatches m) . (.victoryDisplay) . toAttrs . gameScenario)
  FewerInVictoryDisplay n m -> getsGame ((< n) . length . filter (cardMatches m) . (.victoryDisplay) . toAttrs . gameScenario)
  CardInDiscard pid m -> any (cardMatches m) . (.discard) <$> getPlayer pid
  Criteria cs -> allM passesCriteria cs
  EnemiesAtLeast n m -> (>= n) . length <$> select m
  HeadingMatches m -> headingMatches m
  AnyCriteria cs -> anyM passesCriteria cs

-- | Whether each list can give a character not taken by an earlier one.
distinct :: [Target] -> [[Target]] -> Bool
distinct _ [] = True
distinct taken (cs : rest) = any (\c -> distinct (c : taken) rest) [c | c <- cs, c `notElem` taken]

canPay :: HasGame m => AbilityCost -> m Bool
canPay = \case
  Free -> pure True
  ExhaustCost t@(HeroTarget hid) -> andM [selectAny (HeroWithId hid <> ReadyHero), notElem CannotExhaust <$> getModifiers t]
  ExhaustCost t@(AllyTarget aid) -> andM [selectAny (AllyWithId aid <> ReadyAlly), notElem CannotExhaust <$> getModifiers t]
  ExhaustCost (AttachmentTarget aid) ->
    maybe False (not . attachmentExhausted . toAttrs)
      . Map.lookup aid
      <$> getsGame (entitiesAttachments . gameEntities)
  ExhaustCost (ObjectiveTarget oid) ->
    maybe False (not . (.exhausted)) . Map.lookup oid <$> getsGame (entitiesObjectives . gameEntities)
  ExhaustCost _ -> pure False
  ResourceCost (HeroTarget hid) n -> (>= n) . (.resources) <$> getHero hid
  -- an ally's own pool, as Treebeard's
  ResourceCost (AllyTarget aid) n -> (>= n) . countTokens Resource . allyTokens . toAttrs <$> getAlly aid
  ResourceCost _ _ -> pure False
  DiscardFromHandCost pid n -> (>= n) . length . (.hand) <$> getPlayer pid
  RaiseThreatCost _ -> pure True
  SphereResourceCost pid sphere n -> (>= n) <$> getResourcesOfSphere pid sphere
  ExhaustHeroCost m -> notNull <$> exhaustableHeroes m
  Costs cs -> allM canPay cs
  ExhaustCharacterCost hm am -> notNull <$> exhaustableCharacters hm am
  SpendResourcesCost sphere n -> do
    pid <- getsGame gameActivePlayer
    (>= n) <$> getResourcesOfSphere pid sphere

-- | As 'canPay', for the player paying, whom 'ActivePlayer' matches.
canPayFor :: HasGame m => PlayerId -> AbilityCost -> m Bool
canPayFor pid = asPlayer pid . canPay

-- | A query read with the player as the one 'ActivePlayer' matches.
asPlayer :: HasGame m => PlayerId -> ReadGame a -> m a
asPlayer pid q = runReadGame q . (\g -> g {gameActivePlayer = pid}) <$> getGame

-- | The ready heroes matching that can exhaust.
exhaustableHeroes :: HasGame m => HeroMatcher -> m [HeroId]
exhaustableHeroes m = filterM (\h -> notElem CannotExhaust <$> getModifiers (HeroTarget h)) =<< select (m <> ReadyHero)

-- | The ready heroes and allies matching that can exhaust.
exhaustableCharacters :: HasGame m => HeroMatcher -> AllyMatcher -> m [Target]
exhaustableCharacters hm am = filterM (fmap (notElem CannotExhaust) . getModifiers) =<< selectCharacters (hm <> ReadyHero) (am <> ReadyAlly)

{- | The messages that pay the cost, then the rest: a cost paid by a choice holds the rest
until it is made.
-}
payCost :: PlayerId -> AbilityCost -> [Message] -> [Message]
payCost pid cost rest = case cost of
  ExhaustHeroCost m -> [PayExhaustHeroCost pid m rest]
  ExhaustCharacterCost hm am -> [PayExhaustCharacterCost pid hm am rest]
  Costs cs -> foldr (payCost pid) rest cs
  _ -> costMessages (PlayerSource pid) cost <> rest

costMessages :: Source -> AbilityCost -> [Message]
costMessages source = \case
  Free -> []
  ExhaustCost target -> [Exhaust target]
  ResourceCost target n -> [RemoveTokens source target Resource n]
  DiscardFromHandCost pid n -> [ChooseAndDiscardFromHand pid source n]
  RaiseThreatCost n -> case source of
    PlayerSource pid -> [RaiseThreat pid n]
    _ -> []
  SphereResourceCost pid sphere n -> [SpendResources pid sphere n]
  ExhaustHeroCost _ -> [] -- chosen as it is paid, by 'payCost'
  Costs cs -> concatMap (costMessages source) cs
  ExhaustCharacterCost _ _ -> []
  SpendResourcesCost sphere n -> case source of
    PlayerSource pid -> [SpendResources pid sphere n]
    _ -> []

-- | The player who controls an ability's source, if anyone does.
abilityController :: HasGame m => Ability -> m (Maybe PlayerId)
abilityController ab = go ab.abilitySource
 where
  go = \case
    AbilitySource s _ -> go s
    HeroSource hid -> Just . (.owner) <$> getHero hid
    -- one that just left play is still asked of its controller
    AllySource aid -> do
      g <- getGame
      pure
        $ placementController
        . (.placement)
        =<< (Map.lookup aid g.entities.allies <|> Map.lookup aid (gameLeftPlay g))
    PlayerSource pid -> pure $ Just pid
    AttachmentSource aid ->
      fmap ((.controller) . toAttrs) . Map.lookup aid <$> getsGame (entitiesAttachments . gameEntities)
    -- an encounter card attached to a character, as a condition or a claimed objective, is
    -- its controller's to use
    TreacherySource tid -> attachedTo . fmap (.placement) . Map.lookup tid =<< getsGame (entitiesTreacheries . gameEntities)
    ObjectiveSource oid -> attachedTo . fmap (.placement) . Map.lookup oid =<< getsGame (entitiesObjectives . gameEntities)
    -- encounter cards' responses are the first player's to make
    _ -> Just <$> getLead
  attachedTo = \case
    Just (AttachedTo host@(HeroTarget _)) -> characterController host
    Just (AttachedTo host@(AllyTarget _)) -> characterController host
    _ -> Just <$> getLead

-- | The Action abilities a player can use right now.
getActionAbilities :: HasGame m => PlayerId -> m [Ability]
getActionAbilities pid = do
  abilities <- getAllAbilities
  flip filterM abilities \ab -> case ab.abilityType of
    ActionAbility cost ->
      andM
        [ (ab.abilityAnyPlayer ||) . (== Just pid) <$> abilityController ab
        , passesCriteriaFor pid (immuneAware ab.abilitySource ab.abilityCriteria)
        , canPayFor pid cost
        , withinLimitFor pid ab
        ]
    _ -> pure False

{- | The criteria as they stand for the player an action is offered to, whom
'ActivePlayer' matches.
| Criteria read for an ability of the source: a player card's ability finds no use in
a location or enemy immune to player card effects, so those do not count as existing.
-}
passesCriteriaFrom :: HasGame m => Source -> Criterion -> m Bool
passesCriteriaFrom source = passesCriteria . immuneAware source

immuneAware :: Source -> Criterion -> Criterion
immuneAware source c
  | isPlayerCardSource source = go c
  | otherwise = c
 where
  go = \case
    LocationExists m -> LocationExists (m <> LocationNotImmuneToPlayerCards)
    EnemyExists m -> EnemyExists (m <> EnemyNotImmuneToPlayerCards)
    NotCriterion x -> NotCriterion (go x)
    Criteria xs -> Criteria (map go xs)
    AnyCriteria xs -> AnyCriteria (map go xs)
    x -> x

passesCriteriaFor :: HasGame m => PlayerId -> Criterion -> m Bool
passesCriteriaFor pid c = runReadGame (passesCriteria c) . (\g -> g {gameActivePlayer = pid}) <$> getGame

abilityKey :: Ability -> (Source, Int)
abilityKey ab = (ab.abilitySource, ab.abilityIndex)

-- | The locations the travel phase may go to: none while a location is active, but one
-- that may become a second active location, as Ruined Tower.
travelableLocations :: HasGame m => m [LocationId]
travelableLocations = do
  active <- length <$> select IsActiveLocation
  staged <- filterM (fmap (notElem CannotBeTraveledTo) . getModifiers) =<< select LocationInStagingArea
  case active of
    0 -> pure staged
    1 -> filterM (fmap (elem TravelableWithActiveLocation) . getModifiers) staged
    _ -> pure []
