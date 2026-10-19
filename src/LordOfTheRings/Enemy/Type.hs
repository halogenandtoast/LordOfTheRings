{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Enemy.Type (module LordOfTheRings.Enemy.Type) where

import Control.Lens
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Id
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

class
  ( Typeable a
  , Show a
  , ToJSON a
  , RunMessage a
  , Entity a
  , EntityId a ~ EnemyId
  , EntityAttrs a ~ EnemyAttrs
  )
  => IsEnemy a

instance HasField "id" Enemy EnemyId where
  getField (Enemy a) = (toAttrs a).id

instance HasPlacement Enemy where
  placementL =
    lens
      (\(Enemy a) -> (toAttrs a) ^. placementL)
      (\(Enemy a) p -> Enemy $ overAttrs (placementL .~ p) a)

instance HasPlacement EnemyAttrs where
  placementL = lens (.placement) (\x y -> x {placement = y})

data Enemy = forall a. (IsEnemy a) => Enemy a

instance Entity Enemy where
  type EntityId Enemy = EnemyId
  type EntityAttrs Enemy = EnemyAttrs
  toId = toId . toAttrs
  toAttrs (Enemy a) = toAttrs a
  overAttrs f (Enemy a) = Enemy $ overAttrs f a

instance Entity EnemyAttrs where
  type EntityId EnemyAttrs = EnemyId
  type EntityAttrs EnemyAttrs = EnemyAttrs
  toId = (.id)
  toAttrs a = a
  overAttrs f a = f a

data Side = A | B
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data EnemyAttrs = EnemyAttrs
  { id :: EnemyId
  , card :: EncounterCard 'EnemyKind
  , placement :: Placement
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

instance Show Enemy where
  show (Enemy a) = show a

instance ToJSON Enemy where
  toJSON (Enemy a) = toJSON a
