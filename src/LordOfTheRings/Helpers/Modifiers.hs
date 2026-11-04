module LordOfTheRings.Helpers.Modifiers (module LordOfTheRings.Helpers.Modifiers) where

import Control.Monad.Writer.Strict (execWriterT)
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (Card, CardType (AllyType))
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Ally.Types (allyCardId)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Effect
import LordOfTheRings.Enemy.Types (enemyTokens)
import LordOfTheRings.Location.Types (locationTokens)
import LordOfTheRings.Objective.Types (objectiveTokens)
import LordOfTheRings.Quest.Types (questTokens)
import LordOfTheRings.Token (countTokens)
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Matcher.Types (cardMatches)
import LordOfTheRings.Modifier
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Source (GameSource), isPlayerCardSource, isPlayerEventSource, sourceTarget)
import LordOfTheRings.Target

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
      -- the scenario's own, as a nightmare setup card's lasting effect
      getModifiersFor (gameScenario g)
  let -- what is out of play only while its source is in play, as Saruman's chosen card,
      -- comes back as the source leaves
      lasting e =
        ConsideredOutOfPlay `notElem` effectModifiers e
          || maybe True (hostInPlay es . AttachedTo) (sourceTarget $ effectSource e)
      fromEffects =
        Map.fromListWith
          (<>)
          [(effectTarget e, map (Modifier (effectSource e)) (effectModifiers e)) | e <- gameEffects g, lasting e]
      -- an effect on a card follows it into play as an ally
      followed =
        Map.fromListWith
          (<>)
          [(AllyTarget (toId a), ms) | a <- toList es.allies, Just ms <- [Map.lookup (CardIdTarget $ allyCardId $ toAttrs a) fromEffects]]
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
      blanked = blankIn fromEntities <> blankIn fromEffects <> blankIn attachedOut
      ownText m = maybe False (`member` blanked) (sourceTarget $ modifierSource m)
      -- a lasting effect on each location in the staging area reaches one that enters it
      -- after the effect began (FAQ 2.43)
      staged =
        Map.fromListWith
          (<>)
          [ (toTarget (toId l), [Modifier (effectSource e) m])
          | e <- gameEffects g
          , EachLocationInStagingArea m <- effectModifiers e
          , l <- toList es.locations
          , l.placement == StagingArea
          ]
      -- a lasting effect on each ally reaches one that enters play after it began
      allied =
        Map.fromListWith
          (<>)
          [ (toTarget (toId a), [Modifier (effectSource e) m])
          | e <- gameEffects g
          , EachAlly m <- effectModifiers e
          , a <- toList es.allies
          ]
      all0 = Map.unionsWith (<>) [Map.map (filter (not . ownText)) fromEntities, fromEffects, followed, staged, allied, attachedOut]
      -- an encounter card's lasting effect counted from tokens is recalculated as they
      -- change (FAQ 1.55): each such modifier adds its count as the plain modifier
      counted t = \case
        Modifier source (ThreatPerToken token) -> [Modifier source (ThreatModifier $ tokensOn t token)]
        Modifier source (StagingThreatPerToken on token) -> [Modifier source (StagingThreatBonus $ tokensOn on token)]
        _ -> []
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
      unaffected t ms = if immune ms then filter (\m -> sourceTarget (modifierSource m) == Just t || not (isPlayerCardSource $ modifierSource m)) ms else ms
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

-- | What forbids the player playing the card: a 'CannotPlay' on the game or on them, or
-- a one-ally-per-round rule once an ally has been played, kept whole so the board can
-- name the source.
playProhibitions :: (HasGame m) => PlayerId -> Card -> m [Modifier]
playProhibitions pid c = do
  mods <- getsGame gameModifiers
  let on t = Map.findWithDefault [] t mods
      allyPlayed = AllyPlayedThisRound `elem` map modifierType (on GameTarget)
  pure
    [ m
    | m <- on GameTarget <> on (PlayerTarget pid)
    , case m.modifierType of
        CannotPlay matcher -> cardMatches matcher c
        OneAllyPerRound -> allyPlayed && c.cardType == AllyType
        _ -> False
    ]

-- | Whether the card ignores what the source does to it, as East-gate ignores Cave In.
immuneToSource :: (HasGame m, Targetable a) => Source -> a -> m Bool
immuneToSource GameSource _ = pure False
immuneToSource source a = do
  mods <- getModifiers a
  pure
    $ ImmuneToCardEffects `elem` mods
    || (isPlayerCardSource source && ImmuneToPlayerCardEffects `elem` mods)
    || (isPlayerEventSource source && ImmuneToPlayerEvents `elem` mods)

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

-- | Whether the card something is attached to is still in play.
hostInPlay :: Entities -> Placement -> Bool
hostInPlay es = \case
  AttachedTo (HeroTarget i) -> Map.member i es.heroes
  AttachedTo (AllyTarget i) -> Map.member i es.allies
  AttachedTo (EnemyTarget i) -> Map.member i es.enemies
  AttachedTo (LocationTarget i) -> Map.member i es.locations
  _ -> True
