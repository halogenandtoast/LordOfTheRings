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
import LordOfTheRings.Enemy.Cards.ReanimatedDead
import LordOfTheRings.Enemy.Cards.Daechanar
import LordOfTheRings.Enemy.Cards.Dwimmerlaik
import LordOfTheRings.Enemy.Cards.WitchOfAngmar
import LordOfTheRings.Enemy.Cards.WraithOfCarnDum
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
import LordOfTheRings.Enemy.Cards.OrcScramblers
import LordOfTheRings.Enemy.Cards.BatteringRam
import LordOfTheRings.Enemy.Cards.SiegeRaft
import LordOfTheRings.Enemy.Cards.LieutenantOfMordor
import LordOfTheRings.Enemy.Cards.OrcArbalesters
import LordOfTheRings.Enemy.Cards.OrcVanguard
import LordOfTheRings.Enemy.Cards.OrcRabble
import LordOfTheRings.Enemy.Cards.OrcArsonist
import LordOfTheRings.Enemy.Cards.SouthronCompany
import LordOfTheRings.Enemy.Cards.MorgulSpider
import LordOfTheRings.Enemy.Cards.ForestBat
import LordOfTheRings.Enemy.Cards.SouthronMercenaries
import LordOfTheRings.Enemy.Cards.HaradrimElite
import LordOfTheRings.Enemy.Cards.Mumak
import LordOfTheRings.Enemy.Cards.HarborThug
import LordOfTheRings.Enemy.Cards.Pickpocket
import LordOfTheRings.Enemy.Cards.ZealousTraitor
import LordOfTheRings.Enemy.Cards.LossarnachBandit
import LordOfTheRings.Enemy.Cards.UmbarAssassin
import LordOfTheRings.Enemy.Cards.Ghulat
import LordOfTheRings.Enemy.Cards.MaraudingOrc
import LordOfTheRings.Enemy.Cards.OrcRavager
import LordOfTheRings.Enemy.Cards.CravenEagle
import LordOfTheRings.Enemy.Cards.DruBuriDru
import LordOfTheRings.Enemy.Cards.DruadanDrummer
import LordOfTheRings.Enemy.Cards.DruadanElite
import LordOfTheRings.Enemy.Cards.DruadanHunter
import LordOfTheRings.Enemy.Cards.DruadanThief
import LordOfTheRings.Enemy.Cards.MorgulBodyguard
import LordOfTheRings.Enemy.Cards.MorgulSorcerer
import LordOfTheRings.Enemy.Cards.MorgulTracker
import LordOfTheRings.Enemy.Cards.Murzag
import LordOfTheRings.Enemy.Cards.LordAlcaron
import LordOfTheRings.Enemy.Cards.NazgulOfMinasMorgul
import LordOfTheRings.Enemy.Cards.UnderworldDissident
import LordOfTheRings.Enemy.Cards.DaughterOfBeruthiel
import LordOfTheRings.Enemy.Cards.TheHandOfCastamir
import LordOfTheRings.Enemy.Cards.TelemnarsBane
import LordOfTheRings.Enemy.Cards.UrukLieutenant
import LordOfTheRings.Enemy.Cards.UrukSoldier
import LordOfTheRings.Enemy.Cards.SouthronPhalanx
import LordOfTheRings.Enemy.Cards.SouthronCommander
import LordOfTheRings.Enemy.Cards.BlackNumenorean
import LordOfTheRings.Enemy.Cards.OrcAmbusher
import LordOfTheRings.Enemy.Cards.BrutalUruk
import LordOfTheRings.Enemy.Cards.EvilCrow
import LordOfTheRings.Enemy.Cards.MugashEnemy
import LordOfTheRings.Enemy.Cards.MugashsGuard
import LordOfTheRings.Enemy.Cards.MethedrasOrc
import LordOfTheRings.Enemy.Cards.DarkHeartedHuorn
import LordOfTheRings.Enemy.Cards.AngryHuorn
import LordOfTheRings.Enemy.Cards.DeadlyHuorn
import LordOfTheRings.Enemy.Cards.OrcSkirmisher
import LordOfTheRings.Enemy.Cards.OrcHunter
import LordOfTheRings.Enemy.Cards.OrcHound
import LordOfTheRings.Enemy.Cards.DunlandProwler
import LordOfTheRings.Enemy.Cards.DunlendingBandit
import LordOfTheRings.Enemy.Cards.DunlandRaider
import LordOfTheRings.Enemy.Cards.DunlandTribesman
import LordOfTheRings.Enemy.Cards.DunlandChieftain
import LordOfTheRings.Enemy.Cards.DunlandBerserker
import LordOfTheRings.Enemy.Cards.ProwlingWolf
import LordOfTheRings.Enemy.Cards.RavenChief
import LordOfTheRings.Enemy.Cards.RavenWarrior
import LordOfTheRings.Enemy.Cards.RavenSkirmisher
import LordOfTheRings.Enemy.Cards.ChiefTurchTheDunlandTrap
import LordOfTheRings.Enemy.Cards.BoarClanStalker
import LordOfTheRings.Enemy.Cards.BoarClanWarrior
import LordOfTheRings.Enemy.Cards.BellachTroubleInTharbad
import LordOfTheRings.Enemy.Cards.SpyFromMordor
import LordOfTheRings.Enemy.Cards.BellachsMarauder
import LordOfTheRings.Enemy.Cards.BellachCelebrimborsSecret
import LordOfTheRings.Enemy.Cards.BellachsScout
import LordOfTheRings.Enemy.Cards.ProwlingOrc
import LordOfTheRings.Enemy.Cards.AncientMarshDweller
import LordOfTheRings.Enemy.Cards.GiantSwampAdder
import LordOfTheRings.Enemy.Cards.Neekerbreekers
import LordOfTheRings.Enemy.Cards.BoarsGuardian
import LordOfTheRings.Enemy.Cards.WolfsGuardian
import LordOfTheRings.Enemy.Cards.RavensGuardian
import LordOfTheRings.Enemy.Cards.SpiritOfTheWild
import LordOfTheRings.Enemy.Cards.Thaurdir
import LordOfTheRings.Enemy.Cards.CarnDumGarrison
import LordOfTheRings.Enemy.Cards.OrcGrunts
import LordOfTheRings.Enemy.Cards.WerewolfOfAngmar
import LordOfTheRings.Enemy.Cards.PackLeader
import LordOfTheRings.Enemy.Cards.NorthernWarg
import LordOfTheRings.Enemy.Cards.BloodThirstyWarg
import LordOfTheRings.Enemy.Cards.HuntingPack
import LordOfTheRings.Enemy.Cards.WhiteWarg
import LordOfTheRings.Enemy.Cards.WolfOfAngmar
import LordOfTheRings.Enemy.Cards.ThaurdirDeadmensDike
import LordOfTheRings.Enemy.Cards.ThaurdirsDamned
import LordOfTheRings.Enemy.Cards.BalefulShade
import LordOfTheRings.Enemy.Cards.DeadLord
import LordOfTheRings.Enemy.Cards.CursedDead
import LordOfTheRings.Enemy.Cards.JailorGornakh
import LordOfTheRings.Enemy.Cards.CruelTorturer
import LordOfTheRings.Enemy.Cards.DungeonGuard
import LordOfTheRings.Enemy.Cards.GoblinTormentor
import LordOfTheRings.Enemy.Cards.OrcWarParty
import LordOfTheRings.Enemy.Cards.AngmarOrc
import LordOfTheRings.Enemy.Cards.AngmarMarauder
import LordOfTheRings.Enemy.Cards.AngmarCaptain
import LordOfTheRings.Enemy.Cards.CorneredOrc
import LordOfTheRings.Enemy.Cards.SavageTrollspawn
import LordOfTheRings.Enemy.Cards.ColdfellGiant
import LordOfTheRings.Enemy.Cards.CruelMountainTroll
import LordOfTheRings.Enemy.Cards.RuthlessHillTroll
import LordOfTheRings.Enemy.Cards.GoblinPursuer
import LordOfTheRings.Enemy.Cards.ThaurdirTheTreacheryOfRhudaur
import LordOfTheRings.Enemy.Cards.WightOfRhudaur
import LordOfTheRings.Enemy.Cards.TraitorousWight
import LordOfTheRings.Enemy.Cards.ScoutingShip
import LordOfTheRings.Enemy.Cards.LightCruiser
import LordOfTheRings.Enemy.Cards.CorsairWarship
import LordOfTheRings.Enemy.Cards.SouthronSailor
import LordOfTheRings.Enemy.Cards.CunningPirate
import LordOfTheRings.Enemy.Cards.ViciousMarauder
import LordOfTheRings.Enemy.Cards.UmbarCaptain
import LordOfTheRings.Enemy.Cards.UmbarRaider
import LordOfTheRings.Enemy.Cards.CorsairInfiltrator
import LordOfTheRings.Enemy.Cards.CorsairArsonist
import LordOfTheRings.Enemy.Cards.SahirsRavager
import LordOfTheRings.Enemy.Cards.RaidLeader
import LordOfTheRings.Enemy.Cards.ServantOfTheDeceiver
import LordOfTheRings.Enemy.Cards.DrownedDead
import LordOfTheRings.Enemy.Cards.SoullessCadaver
import LordOfTheRings.Enemy.Cards.ThrongsOfUnfaithful
import LordOfTheRings.Enemy.Cards.CaptainSahir
import LordOfTheRings.Enemy.Cards.Naasiyah
import LordOfTheRings.Enemy.Cards.Stormcaller
import LordOfTheRings.Enemy.Cards.SahirsEscort
import LordOfTheRings.Enemy.Cards.SwiftRaider
import LordOfTheRings.Enemy.Cards.CorsairSkiff
import LordOfTheRings.Enemy.Cards.CorsairSeafarer
import LordOfTheRings.Enemy.Cards.SerpentsBlade
import LordOfTheRings.Enemy.Cards.TheThingInTheDepths
import LordOfTheRings.Enemy.Cards.EyeOfTheBeast
import LordOfTheRings.Enemy.Cards.ThrashingArm
import LordOfTheRings.Enemy.Cards.CrushingArm
import LordOfTheRings.Enemy.Cards.GraspingArm
import LordOfTheRings.Enemy.Cards.GiantArm
import LordOfTheRings.Enemy.Cards.SeaScorpion
import LordOfTheRings.Enemy.Cards.CaveEel
import LordOfTheRings.Enemy.Cards.TempleGuardian
import LordOfTheRings.Enemy.Cards.FaithlessDesecrator
import LordOfTheRings.Enemy.Cards.IslandWatcher
import LordOfTheRings.Enemy.Cards.ProfaneDead
import LordOfTheRings.Enemy.Cards.RaiderFlagship
import LordOfTheRings.Enemy.Cards.CorsairSkirmisher
import LordOfTheRings.Enemy.Cards.SiegeShip
import LordOfTheRings.Enemy.Cards.HeavyCruiser
import LordOfTheRings.Enemy.Cards.StormcallerTheCityOfCorsairs
import LordOfTheRings.Enemy.Cards.SlaveShip
import LordOfTheRings.Enemy.Cards.UmbarSlaver
import LordOfTheRings.Enemy.Cards.CaptainSahirTheCityOfCorsairs
import LordOfTheRings.Enemy.Cards.UngoliantsSpawnNightmare
import LordOfTheRings.Enemy.Cards.UngoliantsBrood
import LordOfTheRings.Enemy.Cards.ForestFlies
import LordOfTheRings.Enemy.Cards.SpidersOfMirkwood
import LordOfTheRings.Enemy.Cards.MarshlandOutlaws
import LordOfTheRings.Enemy.Cards.AnduinTrollSpawn
import LordOfTheRings.Enemy.Cards.BrownWaterRats
import LordOfTheRings.Enemy.Cards.TortureMaster
import LordOfTheRings.Enemy.Cards.CrazedCaptive
import LordOfTheRings.Enemy.Cards.SpiderOfDolGuldur
import LordOfTheRings.Enemy.Cards.WolfpeltGoblin
import LordOfTheRings.Enemy.Cards.SilvertineMarauder
import LordOfTheRings.Enemy.Cards.SnowGiant
import LordOfTheRings.Enemy.Cards.UrukElfStalker
import LordOfTheRings.Enemy.Cards.GoblinTrapper
import LordOfTheRings.Enemy.Cards.RaidingParty
import LordOfTheRings.Enemy.Cards.ViciousTrackers
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
        , SomeEnemyCard reanimatedDead
        , SomeEnemyCard daechanar
        , SomeEnemyCard dwimmerlaik
        , SomeEnemyCard witchOfAngmar
        , SomeEnemyCard wraithOfCarnDum
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
        , SomeEnemyCard orcScramblers
        , SomeEnemyCard batteringRam
        , SomeEnemyCard siegeRaft
        , SomeEnemyCard lieutenantOfMordor
        , SomeEnemyCard orcArbalesters
        , SomeEnemyCard orcVanguard
        , SomeEnemyCard orcRabble
        , SomeEnemyCard orcArsonist
        , SomeEnemyCard southronCompany
        , SomeEnemyCard morgulSpider
        , SomeEnemyCard forestBat
        , SomeEnemyCard southronMercenaries
        , SomeEnemyCard haradrimElite
        , SomeEnemyCard mumak
        , SomeEnemyCard harborThug
        , SomeEnemyCard pickpocket
        , SomeEnemyCard zealousTraitor
        , SomeEnemyCard lossarnachBandit
        , SomeEnemyCard umbarAssassin
        , SomeEnemyCard ghulat
        , SomeEnemyCard maraudingOrc
        , SomeEnemyCard orcRavager
        , SomeEnemyCard cravenEagle
        , SomeEnemyCard druBuriDru
        , SomeEnemyCard druadanDrummer
        , SomeEnemyCard druadanElite
        , SomeEnemyCard druadanHunter
        , SomeEnemyCard druadanThief
        , SomeEnemyCard morgulBodyguard
        , SomeEnemyCard morgulSorcerer
        , SomeEnemyCard morgulTracker
        , SomeEnemyCard murzag
        , SomeEnemyCard lordAlcaron
        , SomeEnemyCard nazgulOfMinasMorgul
        , SomeEnemyCard underworldDissident
        , SomeEnemyCard daughterOfBeruthiel
        , SomeEnemyCard theHandOfCastamir
        , SomeEnemyCard telemnarsBane
        , SomeEnemyCard urukLieutenant
        , SomeEnemyCard urukSoldier
        , SomeEnemyCard southronPhalanx
        , SomeEnemyCard southronCommander
        , SomeEnemyCard blackNumenorean
        , SomeEnemyCard orcAmbusher
        , SomeEnemyCard brutalUruk
        , SomeEnemyCard evilCrow
        , SomeEnemyCard mugashEnemy
        , SomeEnemyCard mugashsGuard
        , SomeEnemyCard methedrasOrc
        , SomeEnemyCard darkHeartedHuorn
        , SomeEnemyCard angryHuorn
        , SomeEnemyCard deadlyHuorn
        , SomeEnemyCard orcSkirmisher
        , SomeEnemyCard orcHunter
        , SomeEnemyCard orcHound
        , SomeEnemyCard dunlandProwler
        , SomeEnemyCard dunlendingBandit
        , SomeEnemyCard dunlandRaider
        , SomeEnemyCard dunlandTribesman
        , SomeEnemyCard dunlandChieftain
        , SomeEnemyCard dunlandBerserker
        , SomeEnemyCard prowlingWolf
        , SomeEnemyCard ravenChief
        , SomeEnemyCard ravenWarrior
        , SomeEnemyCard ravenSkirmisher
        , SomeEnemyCard chiefTurchTheDunlandTrap
        , SomeEnemyCard boarClanStalker
        , SomeEnemyCard boarClanWarrior
        , SomeEnemyCard bellachTroubleInTharbad
        , SomeEnemyCard spyFromMordor
        , SomeEnemyCard bellachsMarauder
        , SomeEnemyCard bellachCelebrimborsSecret
        , SomeEnemyCard bellachsScout
        , SomeEnemyCard prowlingOrc
        , SomeEnemyCard ancientMarshDweller
        , SomeEnemyCard giantSwampAdder
        , SomeEnemyCard neekerbreekers
        , SomeEnemyCard boarsGuardian
        , SomeEnemyCard wolfsGuardian
        , SomeEnemyCard ravensGuardian
        , SomeEnemyCard spiritOfTheWild
        , SomeEnemyCard thaurdirCaptain
        , SomeEnemyCard thaurdirChampion
        , SomeEnemyCard carnDumGarrison
        , SomeEnemyCard orcGrunts
        , SomeEnemyCard werewolfOfAngmar
        , SomeEnemyCard packLeader
        , SomeEnemyCard northernWarg
        , SomeEnemyCard bloodThirstyWarg
        , SomeEnemyCard huntingPack
        , SomeEnemyCard whiteWarg
        , SomeEnemyCard wolfOfAngmar
        , SomeEnemyCard thaurdirDeadmensDike
        , SomeEnemyCard thaurdirsDamned
        , SomeEnemyCard balefulShade
        , SomeEnemyCard deadLord
        , SomeEnemyCard cursedDead
        , SomeEnemyCard jailorGornakh
        , SomeEnemyCard cruelTorturer
        , SomeEnemyCard dungeonGuard
        , SomeEnemyCard goblinTormentor
        , SomeEnemyCard orcWarParty
        , SomeEnemyCard angmarOrc
        , SomeEnemyCard angmarMarauder
        , SomeEnemyCard angmarCaptain
        , SomeEnemyCard corneredOrc
        , SomeEnemyCard savageTrollspawn
        , SomeEnemyCard coldfellGiant
        , SomeEnemyCard cruelMountainTroll
        , SomeEnemyCard ruthlessHillTroll
        , SomeEnemyCard goblinPursuer
        , SomeEnemyCard thaurdirTheTreacheryOfRhudaur
        , SomeEnemyCard wightOfRhudaur
        , SomeEnemyCard traitorousWight
        , SomeEnemyCard scoutingShip
        , SomeEnemyCard lightCruiser
        , SomeEnemyCard corsairWarship
        , SomeEnemyCard southronSailor
        , SomeEnemyCard cunningPirate
        , SomeEnemyCard viciousMarauder
        , SomeEnemyCard umbarCaptain
        , SomeEnemyCard umbarRaider
        , SomeEnemyCard corsairInfiltrator
        , SomeEnemyCard corsairArsonist
        , SomeEnemyCard sahirsRavager
        , SomeEnemyCard raidLeader
        , SomeEnemyCard servantOfTheDeceiver
        , SomeEnemyCard drownedDead
        , SomeEnemyCard soullessCadaver
        , SomeEnemyCard throngsOfUnfaithful
        , SomeEnemyCard captainSahir
        , SomeEnemyCard naasiyah
        , SomeEnemyCard stormcaller
        , SomeEnemyCard sahirsEscort
        , SomeEnemyCard swiftRaider
        , SomeEnemyCard corsairSkiff
        , SomeEnemyCard corsairSeafarer
        , SomeEnemyCard serpentsBlade
        , SomeEnemyCard theThingInTheDepths
        , SomeEnemyCard eyeOfTheBeast
        , SomeEnemyCard thrashingArm
        , SomeEnemyCard crushingArm
        , SomeEnemyCard graspingArm
        , SomeEnemyCard giantArm
        , SomeEnemyCard seaScorpion
        , SomeEnemyCard caveEel
        , SomeEnemyCard templeGuardian
        , SomeEnemyCard faithlessDesecrator
        , SomeEnemyCard islandWatcher
        , SomeEnemyCard profaneDead
        , SomeEnemyCard raiderFlagship
        , SomeEnemyCard corsairSkirmisher
        , SomeEnemyCard siegeShip
        , SomeEnemyCard heavyCruiser
        , SomeEnemyCard stormcallerTheCityOfCorsairs
        , SomeEnemyCard slaveShip
        , SomeEnemyCard umbarSlaver
        , SomeEnemyCard captainSahirTheCityOfCorsairs
        , SomeEnemyCard ungoliantsSpawnNightmare
        , SomeEnemyCard ungoliantsBrood
        , SomeEnemyCard forestFlies
        , SomeEnemyCard spidersOfMirkwood
        , SomeEnemyCard marshlandOutlaws
        , SomeEnemyCard anduinTrollSpawn
        , SomeEnemyCard brownWaterRats
        , SomeEnemyCard tortureMaster
        , SomeEnemyCard crazedCaptive
        , SomeEnemyCard spiderOfDolGuldur
        , SomeEnemyCard wolfpeltGoblin
        , SomeEnemyCard silvertineMarauder
        , SomeEnemyCard snowGiant
        , SomeEnemyCard urukElfStalker
        , SomeEnemyCard goblinTrapper
        , SomeEnemyCard raidingParty
        , SomeEnemyCard viciousTrackers
        ]
    ]
