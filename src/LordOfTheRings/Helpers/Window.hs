module LordOfTheRings.Helpers.Window (module LordOfTheRings.Helpers.Window) where

import LordOfTheRings.Ability
import Data.Map.Strict qualified as Map
import LordOfTheRings.Attachment.Types
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Hero.Types
import LordOfTheRings.Ally.Types (allyCardId)
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Enemy.Types (enemyCardId)
import LordOfTheRings.Location.Types (locationCardId)
import LordOfTheRings.Id
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (BlankAttachments, CannotCancelEncounterEffects, CannotTriggerAbilities))
import LordOfTheRings.Message
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

getAllAbilities :: (HasGame m) => m [Ability]
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
  flip filterM abilities \ab -> notElem CannotTriggerAbilities <$> maybe (pure []) getModifiers (sourceTarget ab.abilitySource)

-- | Abilities whose window matches one of the windows and that can be used right now.
getWindowAbilities :: (HasGame m) => [Window] -> m [Ability]
getWindowAbilities windows = do
  abilities <- getAllAbilities
  flip filterM abilities \ab -> case abilityWindow ab of
    Just matcher -> andM [anyM (windowMatchesM matcher) windows, passesCriteriaFrom ab.abilitySource ab.abilityCriteria, canPay (abilityCost ab), withinLimit ab]
    _ -> pure False

-- | Whether the window matches, reading the game for the matchers that need it.
windowMatchesM :: (HasGame m) => WindowMatcher -> Window -> m Bool
windowMatchesM matcher w@(Window timing wType) = case (matcher, wType) of
  (LeftPlaySharingTraitWindow t host, LeftPlay _ _ traits) | t == timing -> do
    def <- hostDef host
    anyM (\trait -> hasTrait trait host def) (toList traits)
  (EnemyDestroyedByHeroWindow t m, EnemyDestroyedByAttack _ attackers) | t == timing ->
    anyM (\case HeroTarget hid -> selectAny (HeroWithId hid <> m); _ -> pure False) attackers
  _ -> pure $ windowMatches matcher w
 where
  hostDef = \case
    HeroTarget hid -> toCardDef <$> getHero hid
    AllyTarget aid -> toCardDef <$> getAlly aid
    _ -> error "not a character"

-- | Whether an ability's limit leaves it a use: its controller's uses for a player
-- limit, everyone's for a group limit.
withinLimit :: (HasGame m) => Ability -> m Bool
withinLimit ab = case ab.abilityLimit of
  NoLimit -> pure True
  PlayerLimit _ n -> do
    controller <- abilityController ab
    players <- getsGame (toList . gamePlayers)
    source <- limitSource ab
    pure $ uses source [p | p <- players, Just p.id == controller] < n
  GroupLimit _ n -> do
    source <- limitSource ab
    (< n) . uses source . toList <$> getsGame gamePlayers
 where
  uses source players =
    sum [u.usedTimes | p <- players, u <- p.usedAbilities, u.usedAbilitySource == source, u.usedAbilityIndex == ab.abilityIndex]

-- | The source an ability's uses are counted against: a once-per-game limit follows the
-- ally's card, as the ally comes back into play under a new id.
limitSource :: (HasGame m) => Ability -> m Source
limitSource ab = case (abilityLimitType ab.abilityLimit, ab.abilitySource) of
  (Just PerGame, AllySource aid) ->
    maybe ab.abilitySource (CardIdSource . allyCardId . toAttrs) . Map.lookup aid <$> getsGame (entitiesAllies . gameEntities)
  _ -> pure ab.abilitySource

passesCriteria :: (HasGame m) => Criterion -> m Bool
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
      $ cid `notElem` map (enemyCardId . toAttrs) (toList es.enemies)
      && cid `notElem` map (locationCardId . toAttrs) (toList es.locations)
  CardInHand pid cType -> anyM passesUnique . filter ((== cType) . (.cardType)) . (.hand) =<< getPlayer pid
  ResourcesOfSphere pm sphere n -> anyM (\p -> (>= n) <$> getResourcesOfSphere p sphere) =<< select pm
  BeforeCommitting -> andM [(== QuestPhase) <$> getsGame gamePhase, selectNone CommittedHero, selectNone CommittedAlly]
  EncounterEffectsCancelable -> notElem CannotCancelEncounterEffects <$> getModifiers GameTarget
  NotCriterion c -> not <$> passesCriteria c
  TargetHasModifier t m -> elem m <$> getModifiers t
  TargetLacksModifier t m -> notElem m <$> getModifiers t
  CharactersAtLeast n heroes allies -> (>= n) . length <$> selectCharacters heroes allies
  DistinctCharacters pairs -> distinct [] <$> traverse (uncurry selectCharacters) pairs
  Criteria cs -> allM passesCriteria cs
  AnyCriteria cs -> anyM passesCriteria cs

-- | Whether each list can give a character not taken by an earlier one.
distinct :: [Target] -> [[Target]] -> Bool
distinct _ [] = True
distinct taken (cs : rest) = any (\c -> distinct (c : taken) rest) [c | c <- cs, c `notElem` taken]

canPay :: (HasGame m) => AbilityCost -> m Bool
canPay = \case
  Free -> pure True
  ExhaustCost (HeroTarget hid) -> selectAny (HeroWithId hid <> ReadyHero)
  ExhaustCost (AllyTarget aid) -> selectAny (AllyWithId aid <> ReadyAlly)
  ExhaustCost (AttachmentTarget aid) ->
    maybe False (not . attachmentExhausted . toAttrs) . Map.lookup aid <$> getsGame (entitiesAttachments . gameEntities)
  ExhaustCost _ -> pure False
  ResourceCost (HeroTarget hid) n -> (>= n) . (.resources) <$> getHero hid
  ResourceCost _ _ -> pure False
  DiscardFromHandCost pid n -> (>= n) . length . (.hand) <$> getPlayer pid
  RaiseThreatCost _ -> pure True
  SphereResourceCost pid sphere n -> (>= n) <$> getResourcesOfSphere pid sphere
  Costs cs -> allM canPay cs

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
  Costs cs -> concatMap (costMessages source) cs

-- | The player who controls an ability's source, if anyone does.
abilityController :: (HasGame m) => Ability -> m (Maybe PlayerId)
abilityController ab = go ab.abilitySource
 where
  go = \case
    AbilitySource s _ -> go s
    HeroSource hid -> Just . (.owner) <$> getHero hid
    -- one that just left play is still asked of its controller
    AllySource aid -> do
      g <- getGame
      pure $ placementController . (.placement) =<< (Map.lookup aid g.entities.allies <|> Map.lookup aid (gameLeftPlay g))
    PlayerSource pid -> pure $ Just pid
    AttachmentSource aid -> fmap (attachmentOwner . toAttrs) . Map.lookup aid <$> getsGame (entitiesAttachments . gameEntities)
    -- encounter cards' responses are the first player's to make
    _ -> Just <$> getLead

-- | The Action abilities a player can use right now.
getActionAbilities :: (HasGame m) => PlayerId -> m [Ability]
getActionAbilities pid = do
  abilities <- getAllAbilities
  flip filterM abilities \ab -> case ab.abilityType of
    ActionAbility cost -> andM [(ab.abilityAnyPlayer ||) . (== Just pid) <$> abilityController ab, passesCriteriaFor pid (immuneAware ab.abilitySource ab.abilityCriteria), canPay cost, withinLimit ab]
    _ -> pure False

-- | The criteria as they stand for the player an action is offered to, whom
-- 'ActivePlayer' matches.
-- | Criteria read for an ability of the source: a player card's ability finds no use in
-- a location or enemy immune to player card effects, so those do not count as existing.
passesCriteriaFrom :: (HasGame m) => Source -> Criterion -> m Bool
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

passesCriteriaFor :: (HasGame m) => PlayerId -> Criterion -> m Bool
passesCriteriaFor pid c = runReadGame (passesCriteria c) . (\g -> g {gameActivePlayer = pid}) <$> getGame

abilityKey :: Ability -> (Source, Int)
abilityKey ab = (ab.abilitySource, ab.abilityIndex)
