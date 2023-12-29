{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Enemy.Type (module LordOfTheRings.Enemy.Type) where

import Control.Lens
import Data.Aeson
import Data.Coerce
import GHC.Generics
import GHC.Records
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Run

class (Show a, ToJSON a, Run a) => IsEnemy a where
  enemyAttrs :: a -> EnemyAttrs
  default enemyAttrs :: (Coercible a EnemyAttrs) => a -> EnemyAttrs
  enemyAttrs = coerce
  overEnemyAttrs :: (EnemyAttrs -> EnemyAttrs) -> a -> a
  default overEnemyAttrs :: (Coercible a EnemyAttrs) => (EnemyAttrs -> EnemyAttrs) -> a -> a
  overEnemyAttrs = coerce

instance HasField "id" Enemy EnemyId where
  getField (Enemy a) = (enemyAttrs a).id

instance HasPlacement Enemy where
  placementL =
    lens
      (\(Enemy a) -> (enemyAttrs a) ^. placementL)
      (\(Enemy a) p -> Enemy $ overEnemyAttrs (placementL .~ p) a)

instance HasPlacement EnemyAttrs where
  placementL = lens (.placement) (\x y -> x {placement = y})

data Enemy where
  Enemy :: (IsEnemy a) => a -> Enemy

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
