{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Enemy (module LordOfTheRings.Enemy, module LordOfTheRings.Enemy.Type) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Enemy.Type
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Classes.RunMessage

instance RunMessage Enemy where
  runMessage msg (Enemy a) = Enemy <$> runMessage msg a

newtype ForestSpider = ForestSpider EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, ToJSON, Entity)
instance RunMessage ForestSpider where
  runMessage msg (ForestSpider a) = case msg of
    _ -> ForestSpider <$> runMessage msg a

instance RunMessage EnemyAttrs where
  runMessage msg attrs = case msg of
    _ -> pure attrs

lookupEnemy :: CardCode -> EnemyAttrs -> Enemy
lookupEnemy cCode = fromMaybe (error "No such enemy") $ Map.lookup cCode allData
 where
  allData =
    Map.fromList
      [ ("01096", Enemy . ForestSpider)
      ]

newEnemy :: (MonadRandom m) => EncounterCard -> m Enemy
newEnemy card = do
  enemyId <- getRandom
  let attrs = EnemyAttrs {id = enemyId, card, placement = Unplaced}
  pure $ lookupEnemy (toCardCode card) attrs
