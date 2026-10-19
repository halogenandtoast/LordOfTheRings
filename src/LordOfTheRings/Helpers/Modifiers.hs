module LordOfTheRings.Helpers.Modifiers (module LordOfTheRings.Helpers.Modifiers) where

import Control.Monad.Writer.Strict (execWriterT)
import Data.Map.Monoidal.Strict (MonoidalMap (..))
import Data.Map.Strict qualified as Map
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Effect
import LordOfTheRings.Entities
import LordOfTheRings.Game.Base
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | Collect every constant ability and lasting effect into the modifier cache.
preloadModifiers :: (HasGame m) => Game -> m Game
preloadModifiers g = do
  let es = g.entities
  MonoidalMap fromEntities <-
    execWriterT $ do
      traverse_ getModifiersFor es.heroes
      traverse_ getModifiersFor es.allies
      traverse_ getModifiersFor es.enemies
      traverse_ getModifiersFor es.locations
      traverse_ getModifiersFor es.treacheries
      traverse_ getModifiersFor es.quests
  let fromEffects =
        Map.fromListWith
          (<>)
          [(effectTarget e, map (Modifier (effectSource e)) (effectModifiers e)) | e <- gameEffects g]
  pure $ g {gameModifiers = Map.unionWith (<>) fromEntities fromEffects}

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
