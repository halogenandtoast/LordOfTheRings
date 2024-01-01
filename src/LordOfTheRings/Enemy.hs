{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Enemy (module LordOfTheRings.Enemy, module LordOfTheRings.Enemy.Type) where

import Control.Monad.Random
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import LordOfTheRings.Card
import LordOfTheRings.Enemy.Type
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Run

instance Run Enemy where
  run msg (Enemy a) = Enemy <$> run msg a

newtype ForestSpider = ForestSpider EnemyAttrs
  deriving newtype (Show, ToJSON)

instance IsEnemy ForestSpider
instance Run ForestSpider where
  run msg (ForestSpider a) = case msg of
    _ -> ForestSpider <$> run msg a

instance Run EnemyAttrs where
  run msg attrs = case msg of
    _ -> pure attrs

lookupEnemy :: CardCode -> EnemyAttrs -> Enemy
lookupEnemy cCode = fromMaybe (error "No such enemy") $ Map.lookup cCode allData
 where
  allData =
    Map.fromList
      [ ("01096", Enemy . ForestSpider)
      ]

newEnemy :: (MonadRandom m) => EncounterCard 'EnemyKind -> m Enemy
newEnemy card = do
  enemyId <- getRandom
  let attrs = EnemyAttrs {id = enemyId, card, placement = Unplaced}
  pure $ lookupEnemy card.def.code attrs
