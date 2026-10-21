{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Enemy (module LordOfTheRings.Enemy, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.Cards.BlackForestBats
import LordOfTheRings.Enemy.Cards.CavernGuardian
import LordOfTheRings.Enemy.Cards.ChieftanUfthak
import LordOfTheRings.Enemy.Cards.DolGuldurBeastmaster
import LordOfTheRings.Enemy.Cards.DolGuldurOrcs
import LordOfTheRings.Enemy.Cards.DungeonJailor
import LordOfTheRings.Enemy.Cards.EastBightPatrol
import LordOfTheRings.Enemy.Cards.EasternCrows
import LordOfTheRings.Enemy.Cards.ForestSpider
import LordOfTheRings.Enemy.Cards.GoblinSniper
import LordOfTheRings.Enemy.Cards.HillTroll
import LordOfTheRings.Enemy.Cards.Hummerhorns
import LordOfTheRings.Enemy.Cards.KingSpider
import LordOfTheRings.Enemy.Cards.MarshAdder
import LordOfTheRings.Enemy.Cards.MistyMountainGoblins
import LordOfTheRings.Enemy.Cards.NazgulOfDolGuldur
import LordOfTheRings.Enemy.Cards.UngoliantsSpawn
import LordOfTheRings.Enemy.Cards.UngoliantsSwarm
import LordOfTheRings.Enemy.Cards.Wargs
import LordOfTheRings.Enemy.Cards.WolfRider
import LordOfTheRings.Enemy.Cards.OrcGuard
import LordOfTheRings.Enemy.Cards.GoblintownScavengers
import LordOfTheRings.Enemy.Cards.HuntersFromMordor
import LordOfTheRings.Enemy.Runner ()
import LordOfTheRings.Enemy.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored enemy is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Enemy where
  parseJSON v = do
    attrs <- parseJSON @EnemyAttrs v
    pure $ overAttrs (const attrs) $ lookupEnemy (enemyCardCode attrs) (enemyCardId attrs) (enemyId attrs)

instance RunMessage Enemy where
  runMessage msg (Enemy a) = Enemy <$> runMessage msg a

createEnemy :: EncounterCard -> EnemyId -> Enemy
createEnemy card = lookupEnemy (toCardCode card) (ecId card)

lookupEnemy :: (HasCallStack) => CardCode -> CardId -> EnemyId -> Enemy
lookupEnemy cardCode cardId ident = case Map.lookup cardCode allEnemys of
  Just (SomeEnemyCard builder) -> Enemy $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown enemy: " <> show cardCode

newEnemy :: (MonadRandom m) => EncounterCard -> m Enemy
newEnemy card = createEnemy card <$> getRandom

allEnemys :: Map CardCode SomeEnemyCard
allEnemys =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeEnemyCard builder) <-
        [ SomeEnemyCard kingSpider
        , SomeEnemyCard hummerhorns
        , SomeEnemyCard ungoliantsSpawn
        , SomeEnemyCard wolfRider
        , SomeEnemyCard orcGuard
        , SomeEnemyCard hillTroll
        , SomeEnemyCard goblinSniper
        , SomeEnemyCard marshAdder
        , SomeEnemyCard wargs
        , SomeEnemyCard dolGuldurOrcs
        , SomeEnemyCard chieftanUfthak
        , SomeEnemyCard dolGuldurBeastmaster
        , SomeEnemyCard forestSpider
        , SomeEnemyCard eastBightPatrol
        , SomeEnemyCard blackForestBats
        , SomeEnemyCard dungeonJailor
        , SomeEnemyCard nazgulOfDolGuldur
        , SomeEnemyCard cavernGuardian
        , SomeEnemyCard mistyMountainGoblins
        , SomeEnemyCard easternCrows
        , SomeEnemyCard ungoliantsSwarm
        , SomeEnemyCard goblintownScavengers
        , SomeEnemyCard huntersFromMordor
        ]
    ]
