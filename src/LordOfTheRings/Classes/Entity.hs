module LordOfTheRings.Classes.Entity (module LordOfTheRings.Classes.Entity) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Prelude

class Entity a where
  type EntityId a
  type EntityAttrs a
  toId :: a -> EntityId a
  toAttrs :: a -> EntityAttrs a
  overAttrs :: (EntityAttrs a -> EntityAttrs a) -> a -> a

type HasAttrs a b = (Entity a, EntityAttrs a ~ b)

attr :: (Entity a) => (EntityAttrs a -> b) -> a -> b
attr f = f . toAttrs

overAttrsM :: (Monad m, Entity a) => (EntityAttrs a -> m (EntityAttrs a)) -> a -> m a
overAttrsM f a = do
  attrs <- f $ toAttrs a
  pure $ overAttrs (const attrs) a

insertEntity :: (Entity v, EntityId v ~ k, Ord k) => v -> Map k v -> Map k v
insertEntity a = Map.insert (toId a) a

instance (Entity a) => Entity (a `With` b) where
  type EntityId (a `With` b) = EntityId a
  type EntityAttrs (a `With` b) = EntityAttrs a
  toId (a `With` _) = toId a
  toAttrs (a `With` _) = toAttrs a
  overAttrs f (a `With` b) = With (overAttrs f a) b
