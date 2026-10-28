{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Enemy (module LordOfTheRings.Enemy, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Enemy.Cards.BlackForestBats
import LordOfTheRings.Enemy.Cards.GoblinScout
import LordOfTheRings.Enemy.Cards.GoblinSwordsman
import LordOfTheRings.Enemy.Cards.PatrolLeader
import LordOfTheRings.Enemy.Cards.GoblinFollower
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
import LordOfTheRings.Enemy.Cards.Louis
import LordOfTheRings.Enemy.Cards.Morris
import LordOfTheRings.Enemy.Cards.Stuart
import LordOfTheRings.Enemy.Cards.Rupert
import LordOfTheRings.Enemy.Cards.MuckAdder
import LordOfTheRings.Enemy.Cards.MirkwoodFlock
import LordOfTheRings.Enemy.Cards.BlackForestBatsRhosgobel
import LordOfTheRings.Enemy.Cards.OrcHorseThieves
import LordOfTheRings.Enemy.Cards.GiantMarshWorm
import LordOfTheRings.Enemy.Cards.MirkwoodBats
import LordOfTheRings.Enemy.Cards.AttercopAttercop
import LordOfTheRings.Enemy.Cards.TheNamelessFear
import LordOfTheRings.Enemy.Cards.GreatCaveTroll
import LordOfTheRings.Enemy.Cards.OrcDrummer
import LordOfTheRings.Enemy.Cards.StrayGoblin
import LordOfTheRings.Enemy.Cards.ChieftainOfThePit
import LordOfTheRings.Enemy.Cards.BlackUruks
import LordOfTheRings.Enemy.Cards.MountainWarg
import LordOfTheRings.Enemy.Cards.GoblinSpearman
import LordOfTheRings.Enemy.Cards.GoblinArcher
import LordOfTheRings.Enemy.Cards.CaveTroll
import LordOfTheRings.Enemy.Cards.OrcHornBlower
import LordOfTheRings.Enemy.Cards.CaveSpider
import LordOfTheRings.Enemy.Cards.GoblinSneak
import LordOfTheRings.Enemy.Cards.GoblinWarlord
import LordOfTheRings.Enemy.Cards.RockAdder
import LordOfTheRings.Enemy.Cards.GoblinTaskmaster
import LordOfTheRings.Enemy.Cards.OrcRaiders
import LordOfTheRings.Enemy.Cards.Crebain
import LordOfTheRings.Enemy.Cards.WildBear
import LordOfTheRings.Enemy.Cards.MountainGoblin
import LordOfTheRings.Enemy.Cards.MountainTroll
import LordOfTheRings.Enemy.Cards.SnowWarg
import LordOfTheRings.Enemy.Cards.NamelessThing
import LordOfTheRings.Enemy.Cards.ElderNamelessThing
import LordOfTheRings.Enemy.Cards.MoriaBats
import LordOfTheRings.Enemy.Cards.DurinsBane
import LordOfTheRings.Enemy.Cards.RangingGoblin
import LordOfTheRings.Enemy.Cards.TheWatcher
import LordOfTheRings.Enemy.Cards.GraspingTentacle
import LordOfTheRings.Enemy.Cards.ThrashingTentacle
import LordOfTheRings.Enemy.Cards.StrikingTentacle
import LordOfTheRings.Enemy.Cards.SouthronCompany
import LordOfTheRings.Enemy.Cards.MorgulSpider
import LordOfTheRings.Enemy.Cards.ForestBat
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
        , SomeEnemyCard goblinScout
        , SomeEnemyCard goblinSwordsman
        , SomeEnemyCard patrolLeader
        , SomeEnemyCard goblinFollower
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
        , SomeEnemyCard louis
        , SomeEnemyCard morris
        , SomeEnemyCard stuart
        , SomeEnemyCard rupert
        , SomeEnemyCard muckAdder
        , SomeEnemyCard mirkwoodFlock
        , SomeEnemyCard blackForestBatsRhosgobel
        , SomeEnemyCard orcHorseThieves
        , SomeEnemyCard giantMarshWorm
        , SomeEnemyCard mirkwoodBats
        , SomeEnemyCard attercopAttercop
        , SomeEnemyCard theNamelessFear
        , SomeEnemyCard greatCaveTroll
        , SomeEnemyCard orcDrummer
        , SomeEnemyCard strayGoblin
        , SomeEnemyCard chieftainOfThePit
        , SomeEnemyCard blackUruks
        , SomeEnemyCard mountainWarg
        , SomeEnemyCard goblinSpearman
        , SomeEnemyCard goblinArcher
        , SomeEnemyCard caveTroll
        , SomeEnemyCard orcHornBlower
        , SomeEnemyCard caveSpider
        , SomeEnemyCard goblinSneak
        , SomeEnemyCard goblinWarlord
        , SomeEnemyCard rockAdder
        , SomeEnemyCard goblinTaskmaster
        , SomeEnemyCard orcRaiders
        , SomeEnemyCard crebain
        , SomeEnemyCard wildBear
        , SomeEnemyCard mountainGoblin
        , SomeEnemyCard mountainTroll
        , SomeEnemyCard snowWarg
        , SomeEnemyCard namelessThing
        , SomeEnemyCard elderNamelessThing
        , SomeEnemyCard moriaBats
        , SomeEnemyCard durinsBane
        , SomeEnemyCard rangingGoblin
        , SomeEnemyCard theWatcher
        , SomeEnemyCard graspingTentacle
        , SomeEnemyCard thrashingTentacle
        , SomeEnemyCard strikingTentacle
        , SomeEnemyCard southronCompany
        , SomeEnemyCard morgulSpider
        , SomeEnemyCard forestBat
        ]
    ]
