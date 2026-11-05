module LordOfTheRings.Helpers.Modifiers (module LordOfTheRings.Helpers.Modifiers) where

import Control.Monad.Writer.Strict (execWriterT)
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import LordOfTheRings.Card (Card (EncounterCard), CardCode, CardType (AllyType, ObjectiveAllyType), IsCard (toCard), cdCardTraits, cdCardType, cdSphere, isTreasure, toCardCode, toCardDef)
import LordOfTheRings.Sphere (Sphere (Fellowship))
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Ally.Types (allyCardId, allyExhausted, allyTokens)
import LordOfTheRings.Hero.Types (heroCardId, heroExhausted, heroTokens)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Effect
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Location.Types (locationTokens)
import LordOfTheRings.Objective.Types (objectiveTokens)
import LordOfTheRings.Quest.Types (questTokens)
import LordOfTheRings.Token (Token (Damage), countTokens)
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher.Types (cardMatches)
import LordOfTheRings.Modifier
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Source (..), isPlayerCardSource, isPlayerEventSource, sourceTarget)
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Master, Modification))

-- | Collect every constant ability and lasting effect into the modifier cache.
preloadModifiers :: (HasGame m) => Game -> m Game
preloadModifiers g = do
  let es = g.entities
  -- a location may blank the attachments' text, so the locations go first
  MonoidalMap fromLocations <- execWriterT $ traverse_ getModifiersFor es.locations
  let blank = BlankAttachments `elem` map modifierType (Map.findWithDefault [] GameTarget fromLocations)
  MonoidalMap fromEntities <-
    fmap (MonoidalMap fromLocations <>) . execWriterT $ do
      traverse_ getModifiersFor es.heroes
      traverse_ getModifiersFor es.allies
      traverse_ getModifiersFor es.enemies
      traverse_ getModifiersFor (filter (hostInPlay es . (.placement)) $ toList es.treacheries)
      traverse_ getModifiersFor es.quests
      -- a card whose host has just left play is about to follow it, and adds nothing
      unless blank $ traverse_ getModifiersFor (filter (hostInPlay es . (.placement)) $ toList es.attachments)
      traverse_ getModifiersFor (filter (hostInPlay es . (.placement)) $ toList es.objectives)
      traverse_ getModifiersFor es.contracts
      -- the scenario's own, as a nightmare setup card's lasting effect
      getModifiersFor (gameScenario g)
  let -- what is out of play only while its source is in play, as Saruman's chosen card,
      -- comes back as the source leaves
      lasting e =
        ( ConsideredOutOfPlay `notElem` effectModifiers e
            || maybe True (hostInPlay es . AttachedTo) (sourceTarget $ effectSource e)
        )
          && all (shows (effectTarget e)) [c | WhileShowing c <- effectModifiers e]
      -- the card's faceup side, as a two-sided card flipped shows the other's code
      shows t c = case t of
        ObjectiveTarget oid -> maybe False ((== c) . toCardCode) (Map.lookup oid es.objectives)
        QuestTarget qid -> maybe False ((== c) . toCardCode) (Map.lookup qid es.quests)
        LocationTarget lid -> maybe False ((== c) . toCardCode) (Map.lookup lid es.locations)
        EnemyTarget eid -> maybe False ((== c) . toCardCode) (Map.lookup eid es.enemies)
        _ -> False
      fromEffects =
        Map.fromListWith
          (<>)
          [(effectTarget e, map (Modifier (effectSource e)) (effectModifiers e)) | e <- gameEffects g, lasting e]
      -- an effect on a card follows it into play as an ally or a hero
      followed =
        Map.fromListWith (<>)
          $ [(AllyTarget (toId a), ms) | a <- toList es.allies, Just ms <- [Map.lookup (CardIdTarget $ allyCardId $ toAttrs a) fromEffects]]
          <> [(HeroTarget (toId h), ms) | h <- toList es.heroes, Just ms <- [Map.lookup (CardIdTarget $ heroCardId $ toAttrs h) fromEffects]]
      -- each card attached to a card considered out of play is too (FAQ 2.75), as Saruman's
      -- chosen enemy's guarded objective
      outOfPlay = Map.filter notNull $ Map.map (filter ((`elem` [ConsideredOutOfPlay, Blank]) . modifierType)) fromEffects
      outOfPlayHost p = case p of
        AttachedTo host | Just ms <- Map.lookup host outOfPlay, any ((== ConsideredOutOfPlay) . modifierType) ms -> Just ms
        _ -> Nothing
      attachedOut =
        Map.fromListWith (<>)
          $ [(toTarget x, ms) | x <- toList es.attachments, Just ms <- [outOfPlayHost x.placement]]
          <> [(toTarget x, ms) | x <- toList es.objectives, Just ms <- [outOfPlayHost x.placement]]
          <> [(toTarget x, ms) | x <- toList es.treacheries, Just ms <- [outOfPlayHost x.placement]]
          <> [(toTarget x, ms) | x <- toList es.enemies, Just ms <- [outOfPlayHost x.placement]]
      -- a card whose text box is blank, by its own text or a lasting effect, gives no
      -- modifiers from its constant effects, as Arkham's applyBlank; lasting effects it
      -- already created stay
      blankIn = Map.keysSet . Map.filter (any ((== Blank) . modifierType))
      -- Resolve blanking dependencies first: a blanked Wooded Glade cannot blank
      -- attachments in turn. For a cycle (Glade and Thrór's Key), retain the
      -- blanking effect that was already active before the other became applicable.
      -- Immunity is checked before blanking can remove the text that grants it,
      -- as Thrór's Key cannot blank an immune Cobwebbed Tunnel.
      unblocked = Map.mapWithKey unaffected $ Map.unionsWith (<>) [fromEntities, fromEffects, attachedOut, wearied]
      eligible t m = m `elem` Map.findWithDefault [] t unblocked
      effective = Map.mapWithKey (\t -> filter (eligible t))
      fixedBlank = blankIn (effective fromEffects) <> blankIn (effective attachedOut) <> blankIn (effective wearied)
      blankers = Map.fromListWith (<>)
        [(sourceTarget m.modifierSource, Set.singleton t) | (t, ms) <- Map.toList fromEntities, m <- ms, m.modifierType == Blank, eligible t m]
      established = Set.fromList
        [sourceTarget m.modifierSource | ms <- toList g.gameModifiers, m <- ms, m.modifierType == Blank]
      resolveBlanks blocked pending =
        let live = Map.filterWithKey (\s _ -> maybe True (`notMember` blocked) s) pending
            incoming = mconcat (Map.elems live)
            roots = [s | s <- Map.keys live, maybe True (`notMember` incoming) s]
            preferred = filter (`member` established) (Map.keys live)
         in case headMay (roots <> preferred <> Map.keys live) of
              Nothing -> blocked
              Just s -> resolveBlanks (blocked <> Map.findWithDefault mempty s live) (Map.delete s live)
      blanked = resolveBlanks fixedBlank blankers
      ownText m = maybe False (`member` blanked) (sourceTarget $ modifierSource m)
      -- a lasting effect on each location in the staging area reaches one that enters it
      -- after the effect began (FAQ 2.43)
      staged =
        Map.fromListWith
          (<>)
          [ (toTarget (toId l), [Modifier (effectSource e) m])
          | e <- gameEffects g
          , (matches, m) <- [(const True, m) | EachLocationInStagingArea m <- effectModifiers e] <> [(cardMatches cm, m) | EachMatchingLocationInStagingArea cm m <- effectModifiers e]
          , l <- toList es.locations
          , l.placement == StagingArea
          , matches l
          ]
      stagedEnemies = Map.fromListWith (<>)
        [(toTarget e, [Modifier effect.effectSource m])
        | effect <- gameEffects g
        , EachMatchingEnemyInStagingArea cm m <- effect.effectModifiers
        , e <- toList es.enemies
        , e.placement == StagingArea
        , cardMatches cm e]
      -- a lasting effect on each ally reaches one that enters play after it began
      allied =
        Map.fromListWith
          (<>)
          [ (toTarget (toId a), [Modifier (effectSource e) m])
          | e <- gameEffects g
          , EachAlly m <- effectModifiers e
          , a <- toList es.allies
          ]
      -- and on each character while it is exhausted
      wearied =
        Map.fromListWith
          (<>)
          $ [ (toTarget (toId h), [Modifier (effectSource e) m])
            | e <- gameEffects g
            , h <- toList es.heroes
            , m <- [m | EachCharacter m <- effectModifiers e] <> [m | EachExhaustedCharacter m <- effectModifiers e, heroExhausted (toAttrs h)]
            ]
          <> [ (toTarget (toId a), [Modifier (effectSource e) m])
             | e <- gameEffects g
             , a <- toList es.allies
             , m <- [m | EachCharacter m <- effectModifiers e] <> [m | EachExhaustedCharacter m <- effectModifiers e, allyExhausted (toAttrs a)]
             ]
          -- or damaged
          <> [ (toTarget (toId h), [Modifier (effectSource e) m])
             | e <- gameEffects g
             , EachDamagedCharacter m <- effectModifiers e
             , h <- toList es.heroes
             , countTokens Damage (heroTokens $ toAttrs h) > 0
             ]
          <> [ (toTarget (toId a), [Modifier (effectSource e) m])
             | e <- gameEffects g
             , EachDamagedCharacter m <- effectModifiers e
             , a <- toList es.allies
             , countTokens Damage (allyTokens $ toAttrs a) > 0
             ]
      all0 = Map.unionsWith (<>) [Map.map (filter (not . ownText)) fromEntities, fromEffects, followed, staged, stagedEnemies, allied, wearied, attachedOut]
      -- an encounter card's lasting effect counted from tokens is recalculated as they
      -- change (FAQ 1.55): each such modifier adds its count as the plain modifier
      counted t = \case
        Modifier source (ThreatPerToken token) -> [Modifier source (ThreatModifier $ tokensOn t token)]
        Modifier source (ThreatWithToken token n) -> [Modifier source (ThreatModifier n) | tokensOn t token > 0]
        Modifier source (StagingThreatPerToken on token) -> [Modifier source (StagingThreatBonus $ tokensOn on token)]
        Modifier source StagingThreatPerActiveLocationDamage -> [Modifier source (StagingThreatBonus activeDamage)]
        Modifier source AttackPerActiveLocationDamage -> [Modifier source (AttackModifier activeDamage)]
        _ -> []
      activeDamage = sum [countTokens Damage (locationTokens $ toAttrs l) | l <- toList es.locations, l.placement == ActiveLocation]
      all' = Map.mapWithKey (\t ms -> ms <> concatMap (counted t) ms) all0
      tokensOn t token = maybe 0 (countTokens token) case t of
        EnemyTarget i -> enemyTokens . toAttrs <$> Map.lookup i es.enemies
        ObjectiveTarget i -> objectiveTokens . toAttrs <$> Map.lookup i es.objectives
        LocationTarget i -> locationTokens . toAttrs <$> Map.lookup i es.locations
        QuestTarget i -> questTokens . toAttrs <$> Map.lookup i es.quests
        _ -> Nothing
      -- a card immune to player card effects keeps none of their modifiers, but its own, as
      -- an objective ally under a player's control (Chief Turch) is a player card itself
      immune ms = ImmuneToPlayerCardEffects `elem` map modifierType ms
      -- nor, immune to all but Fellowship sphere player cards, of theirs but those
      fellowshipOnly t ms kept
        | ImmuneToNonFellowshipPlayerCardEffects `elem` map modifierType ms =
            filter (\m -> sourceTarget (modifierSource m) == Just t || not (fromPlayerCard m) || maybe False (fellowshipCard g) (sourceTarget m.modifierSource)) kept
        | otherwise = kept
      -- Modification attachments are encounter cards (Dream-chaser rules, "Gaining XP"), so a
      -- ship-objective's immunity lets theirs through
      modifications = setFromList [toTarget x | x <- toList es.attachments, Modification `member` cdCardTraits (toCardDef x)] :: Set Target
      fromPlayerCard m = isPlayerCardSource (modifierSource m) && maybe True (`notMember` modifications) (sourceTarget $ modifierSource m)
      unaffected t ms =
        let ms' = if immune ms then filter (\m -> sourceTarget (modifierSource m) == Just t || not (fromPlayerCard m)) ms else ms
            types = map modifierType ms'
         in fellowshipOnly t ms $ onlyMaster t ms $ filter (\m -> sourceTarget (modifierSource m) == Just t || not (immuneToEncounterSource g types (modifierSource m))) ms'
      -- The One Ring keeps its own modifiers, the game's and Master cards' only
      onlyMaster t ms kept
        | ImmuneToNonMasterCardEffects `elem` map modifierType ms =
            filter (\m -> m.modifierSource == GameSource || maybe False (\s -> s == t || masterCard g s) (sourceTarget m.modifierSource)) kept
        | otherwise = kept
      -- no willpower from player cards, as Frozen Silverlode forbids
      frozen = NoWillpowerFromPlayerCards `elem` map modifierType (Map.findWithDefault [] GameTarget all')
      gained m = case m.modifierType of
        WillpowerModifier n -> frozen && n > 0 && isPlayerCardSource m.modifierSource
        _ -> False
  pure $ g {gameModifiers = Map.mapWithKey (\t -> filter (not . gained) . unaffected t) all'}

-- | Whether the character commits to the quest without exhausting.
doesNotExhaustToCommit :: (HasGame m) => Target -> m Bool
doesNotExhaustToCommit target = elem DoesNotExhaustToCommit <$> getModifiers target

getModifiers :: (HasGame m, Targetable a) => a -> m [ModifierType]
getModifiers a = map modifierType . Map.findWithDefault [] (toTarget a) <$> getsGame gameModifiers

-- | Whether an event card may shuffle cards from a discard pile into a deck: not while
-- 'EventsCannotShuffleDiscardIntoDeck' holds, as Passage into Mordor reads.
eventsMayShuffleDiscardIntoDeck :: HasGame m => m Bool
eventsMayShuffleDiscardIntoDeck = notElem EventsCannotShuffleDiscardIntoDeck <$> getModifiers GameTarget

-- | The cards each player played this round, from wherever, one entry per card played.
-- Read from the effects, so it serves a 'getModifiersFor' as the modifiers are gathered.
cardsPlayedThisRound :: (HasGame m) => m [(PlayerId, CardCode, CardType)]
cardsPlayedThisRound = getsGame \g -> [(pid, code, t) | Effect _ (PlayerTarget pid) ms _ <- gameEffects g, PlayedCardThisRound code t <- ms]

-- | What forbids the player playing the card: a 'CannotPlay' on the game or on them, or
-- a one-ally-per-round rule once an ally has been played, kept whole so the board can
-- name the source.
playProhibitions :: (HasGame m) => PlayerId -> Card -> m [Modifier]
playProhibitions pid c = do
  mods <- getsGame gameModifiers
  entryBarred <- getsGame (allyEntryBarred pid)
  let on t = Map.findWithDefault [] t mods
  allyPlayed <- any (\(_, _, t) -> t == AllyType) <$> cardsPlayedThisRound
  pure
    [ m
    | m <- on GameTarget <> on (PlayerTarget pid)
    , case m.modifierType of
        CannotPlay matcher -> cardMatches matcher c
        OneAllyPerRound -> allyPlayed && c.cardType == AllyType
        OneAllyEntersPlayPerRound -> entryBarred && c.cardType == AllyType
        AlliesCannotEnterPlay -> c.cardType == AllyType
        _ -> False
    ]

-- | Whether the card ignores what the source does to it, as East-gate ignores Cave In.
immuneToSource :: (HasGame m, Targetable a) => Source -> a -> m Bool
immuneToSource GameSource _ = pure False
immuneToSource source a = do
  mods <- getModifiers a
  g <- getGame
  master <- maybe (pure False) isMasterCard (sourceTarget source)
  pure
    $ ImmuneToCardEffects `elem` mods
    || (isPlayerCardSource source && not (maybe False (objectiveAllyTarget g) (sourceTarget source)) && ImmuneToPlayerCardEffects `elem` mods)
    || (isPlayerEventSource source && ImmuneToPlayerEvents `elem` mods)
    || immuneToEncounterSource g mods source
    || (not master && ImmuneToNonMasterCardEffects `elem` mods)
    || (isPlayerCardSource source && ImmuneToNonFellowshipPlayerCardEffects `elem` mods && not (maybe False (fellowshipCard g) (sourceTarget source)))

{- | Whether the card is an objective ally: an encounter card under a player's control, not
a player card, which has the player card back, so a card immune to player card effects is
not immune to its effects, as Farin's damage reaches a Warden (The Land of Sorrow).
-}
objectiveAllyTarget :: Game -> Target -> Bool
objectiveAllyTarget g = \case
  AllyTarget aid -> maybe False ((== ObjectiveAllyType) . cdCardType . toCardDef) (Map.lookup aid g.entities.allies)
  _ -> False

-- | Whether the card is one of those whose effects The One Ring is not immune to: a Master
-- card in play, or a Master event, known by its card id, wherever its owner holds it.
isMasterCard :: HasGame m => Target -> m Bool
isMasterCard t = getsGame (`masterCard` t)

masterCard :: Game -> Target -> Bool
masterCard g t = maybe False (member Master . cdCardTraits . toCardDef) (cardCodeOf g t)

-- | Whether the card is a Fellowship sphere card, in play or, an event, in a player's hand.
fellowshipCard :: Game -> Target -> Bool
fellowshipCard g t = maybe False ((== Just Fellowship) . cdSphere . toCardDef) (cardCodeOf g t)

cardCodeOf :: Game -> Target -> Maybe CardCode
cardCodeOf g t = code
 where
  es = g.entities
  code = case t of
    AttachmentTarget i -> toCardCode <$> Map.lookup i es.attachments
    HeroTarget i -> toCardCode <$> Map.lookup i es.heroes
    AllyTarget i -> toCardCode <$> Map.lookup i es.allies
    CardIdTarget cid -> toCardCode <$> find ((== cid) . (.id)) [c | p <- toList g.players, c <- p.hand <> p.discard <> p.deck]
    _ -> Nothing

{- | Whether the hero cannot gain the resources the source's effect gives it: a card effect's
barred by its modifiers, as Garden of Poisons's, a player card's as No End in Sight's, and
any but a treasure card's for On the Doorstep's Bilbo Baggins. The resource phase's come
from the game, not a card.
-}
resourcesBarredFrom :: (HasGame m, Targetable a) => Source -> a -> m Bool
resourcesBarredFrom GameSource _ = pure False
resourcesBarredFrom source a = do
  mods <- getModifiers a
  g <- getGame
  let treasure = maybe False isTreasure (cardCodeOf g =<< sourceTarget source)
  pure
    $ CannotGainResourcesFromCardEffects `elem` mods
    || (isPlayerCardSource source && CannotGainResourcesFromPlayerCardEffects `elem` mods)
    || (not treasure && CannotGainResourcesFromNonTreasureCards `elem` mods)

-- | Whether only Master cards' effects touch the card, as The One Ring's do: a card effect
-- verb with no source, as 'LordOfTheRings.Message.Lifted.discard', leaves it be.
onlyMasterCardsAffect :: (HasGame m, Targetable a) => a -> m Bool
onlyMasterCardsAffect = fmap (elem ImmuneToNonMasterCardEffects) . getModifiers

-- | A printed value with the target's modifiers applied, floored at 0.
modifiedValue :: (HasGame m, Targetable a) => (ModifierType -> Maybe Int) -> a -> Int -> m Int
modifiedValue select' a base = do
  mods <- getModifiers a
  pure $ max 0 $ base + sum (mapMaybe select' mods)

willpowerOf, attackOf, defenseOf, hitPointsOf, threatOf, engagementCostOf, questPointsOf :: ModifierType -> Maybe Int
willpowerOf = \case WillpowerModifier n -> Just n; _ -> Nothing
attackOf = \case AttackModifier n -> Just n; _ -> Nothing
defenseOf = \case DefenseModifier n -> Just n; _ -> Nothing
hitPointsOf = \case HitPointModifier n -> Just n; _ -> Nothing
threatOf = \case ThreatModifier n -> Just n; _ -> Nothing
engagementCostOf = \case EngagementCostModifier n -> Just n; _ -> Nothing
questPointsOf = \case QuestPointModifier n -> Just n; _ -> Nothing

-- | A location's or quest's quest points with the target's modifiers applied: raised or
-- lowered, then reduced to a value one sets.
modifiedQuestPoints :: (HasGame m, Targetable a) => a -> Int -> m Int
modifiedQuestPoints a base = do
  points <- modifiedValue questPointsOf a base
  mods <- getModifiers a
  pure $ foldr min points [n | QuestPointsReducedTo n <- mods]

-- | Whether the card something is attached to is still in play.
hostInPlay :: Entities -> Placement -> Bool
hostInPlay es = \case
  AttachedTo (HeroTarget i) -> Map.member i es.heroes
  AttachedTo (AllyTarget i) -> Map.member i es.allies
  AttachedTo (EnemyTarget i) -> Map.member i es.enemies
  AttachedTo (LocationTarget i) -> Map.member i es.locations
  _ -> True

{- | Whether a card is immune to what the source does as an encounter card without the
trait its 'ImmuneToEncounterCardEffectsWithout' names, as Gollum is to non-Gollum card
effects. An attack's damage and the game's rules are no card's effect.
-}
immuneToEncounterSource :: Game -> [ModifierType] -> Source -> Bool
immuneToEncounterSource g mods = \case
  EnemyAttackSource _ -> False
  source -> maybe False (immuneToEncounterCard g mods) (sourceTarget source)

{- | Whether the source is an encounter card's effect: an enemy (its attack too), location,
treachery, objective or quest card, as Mount Gundabad's campaign card reads "encounter card
effects". One in play is, if its card is an encounter card and not a player card turned
enemy; one out of play, as a shadow card resolving, is, and so is a shadow card known by
its card id while it is dealt to an enemy. The game's rules, the scenario's and a player
card's effects are not.
-}
isEncounterCardSource :: Game -> Source -> Bool
isEncounterCardSource g (AbilitySource source _) = isEncounterCardSource g source
-- a shadow card known by its card id, still dealt to its enemy as its effect resolves
isEncounterCardSource g (CardIdSource cid) =
  any (any ((== cid) . (.id)) . (.shadowCards)) g.entities.enemies
isEncounterCardSource g source = case sourceTarget source of
  Just (EnemyTarget i) -> encounter $ Map.lookup i es.enemies
  Just (LocationTarget i) -> encounter $ Map.lookup i es.locations
  Just (TreacheryTarget i) -> encounter $ Map.lookup i es.treacheries
  Just (ObjectiveTarget i) -> encounter $ Map.lookup i es.objectives
  Just (QuestTarget i) -> encounter $ Map.lookup i es.quests
  _ -> False
 where
  es = g.entities
  encounter :: IsCard a => Maybe a -> Bool
  encounter = maybe True (\c -> case toCard c of EncounterCard _ -> True; _ -> False)

{- | Whether a card is immune to the encounter card, the target the source is, as
'immuneToEncounterSource'. One that has left play, as a shadow card whose effect lasts, is
known by no trait: no card with one of these traits lasts that way (Stinker, the only
Gollum card Gollum meets, leaves none).
-}
immuneToEncounterCard :: Game -> [ModifierType] -> Target -> Bool
immuneToEncounterCard g mods source = case [t | ImmuneToEncounterCardEffectsWithout t <- mods] of
  [] -> False
  traits -> case encounterDef of
    Nothing -> False
    Just def -> maybe True (\d -> not $ any (`member` cdCardTraits d) traits) def
 where
  es = g.entities
  encounterDef = case source of
    EnemyTarget i -> Just $ toCardDef <$> Map.lookup i es.enemies
    LocationTarget i -> Just $ toCardDef <$> Map.lookup i es.locations
    TreacheryTarget i -> Just $ toCardDef <$> Map.lookup i es.treacheries
    ObjectiveTarget i -> Just $ toCardDef <$> Map.lookup i es.objectives
    QuestTarget i -> Just $ toCardDef <$> Map.lookup i es.quests
    _ -> Nothing
