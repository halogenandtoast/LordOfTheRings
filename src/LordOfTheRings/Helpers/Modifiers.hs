module LordOfTheRings.Helpers.Modifiers (module LordOfTheRings.Helpers.Modifiers) where

import Control.Monad.Writer.Strict (execWriterT)
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Ally.Types (allyCardId)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Card.CardDef (CardDef (..))
import LordOfTheRings.Effect
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Modifier
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Source (isPlayerCardSource)
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
  let fromEffects =
        Map.fromListWith
          (<>)
          [(effectTarget e, map (Modifier (effectSource e)) (effectModifiers e)) | e <- gameEffects g]
      -- an effect on a card follows it into play as an ally
      followed =
        Map.fromListWith
          (<>)
          [(AllyTarget (toId a), ms) | a <- toList es.allies, Just ms <- [Map.lookup (CardIdTarget $ allyCardId $ toAttrs a) fromEffects]]
      all' = Map.unionsWith (<>) [fromEntities, fromEffects, followed]
      -- a card immune to player card effects keeps none of their modifiers
      immune ms = ImmuneToPlayerCardEffects `elem` map modifierType ms
      unaffected ms = if immune ms then filter (not . isPlayerCardSource . modifierSource) ms else ms
  pure $ g {gameModifiers = Map.map unaffected all'}

-- | Whether the character commits to the quest without exhausting: by its own modifiers,
-- or the game's for a trait it has, printed or gained.
doesNotExhaustToCommit :: (HasGame m) => Target -> CardDef -> m Bool
doesNotExhaustToCommit target def = do
  own <- getModifiers target
  game <- getModifiers GameTarget
  let traits = cdCardTraits def <> setFromList [t | GainTrait t <- own]
  pure $ DoesNotExhaustToCommit `elem` own || or [t `member` traits | TraitDoesNotExhaustToCommit t <- game]

getModifiers :: (HasGame m, Targetable a) => a -> m [ModifierType]
getModifiers a = map modifierType . Map.findWithDefault [] (toTarget a) <$> getsGame gameModifiers

-- | A printed value with the target's modifiers applied, floored at 0.
modifiedValue :: (HasGame m, Targetable a) => (ModifierType -> Maybe Int) -> a -> Int -> m Int
modifiedValue select' a base = do
  mods <- getModifiers a
  pure $ max 0 $ base + sum (mapMaybe select' mods)

willpowerOf, attackOf, defenseOf, hitPointsOf, threatOf, engagementCostOf :: ModifierType -> Maybe Int
willpowerOf = \case WillpowerModifier n -> Just n; _ -> Nothing
attackOf = \case AttackModifier n -> Just n; _ -> Nothing
defenseOf = \case DefenseModifier n -> Just n; _ -> Nothing
hitPointsOf = \case HitPointModifier n -> Just n; _ -> Nothing
threatOf = \case ThreatModifier n -> Just n; _ -> Nothing
engagementCostOf = \case EngagementCostModifier n -> Just n; _ -> Nothing

-- | Whether the card something is attached to is still in play.
hostInPlay :: Entities -> Placement -> Bool
hostInPlay es = \case
  AttachedTo (HeroTarget i) -> Map.member i es.heroes
  AttachedTo (AllyTarget i) -> Map.member i es.allies
  AttachedTo (EnemyTarget i) -> Map.member i es.enemies
  AttachedTo (LocationTarget i) -> Map.member i es.locations
  _ -> True
