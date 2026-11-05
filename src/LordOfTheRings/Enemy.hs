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
import LordOfTheRings.Enemy.Cards.Thunderstruck
import LordOfTheRings.Enemy.Cards.Crowley
import LordOfTheRings.Enemy.Cards.Revenant
import LordOfTheRings.Enemy.Cards.Graxar
import LordOfTheRings.Enemy.Cards.SeaSerpent
import LordOfTheRings.Enemy.Cards.RecurringNightmare
import LordOfTheRings.Enemy.Cards.CunningInformant
import LordOfTheRings.Enemy.Cards.HarborBrute
import LordOfTheRings.Enemy.Cards.PelargirRingleader
import LordOfTheRings.Enemy.Cards.MumakElite
import LordOfTheRings.Enemy.Cards.HaradrimMarksman
import LordOfTheRings.Enemy.Cards.HaradrimCaptain
import LordOfTheRings.Enemy.Cards.SouthronMountedArcher
import LordOfTheRings.Enemy.Cards.OrcSaboteur
import LordOfTheRings.Enemy.Cards.OrcRavagerNightmare
import LordOfTheRings.Enemy.Cards.DunlandAssassin
import LordOfTheRings.Enemy.Cards.VengefulHillman
import LordOfTheRings.Enemy.Cards.SavageHillman
import LordOfTheRings.Enemy.Cards.VigilantOrc
import LordOfTheRings.Enemy.Cards.MethedrasLookout
import LordOfTheRings.Enemy.Cards.GuardWarg
import LordOfTheRings.Enemy.Cards.HuornOfFangorn
import LordOfTheRings.Enemy.Cards.BlockingHuorn
import LordOfTheRings.Enemy.Cards.WickedHuorn
import LordOfTheRings.Enemy.Cards.BrashCorsair
import LordOfTheRings.Enemy.Cards.SahirsPursuer
import LordOfTheRings.Enemy.Cards.GuardianOfTheGoldenKing
import LordOfTheRings.Enemy.Cards.CorsairPillager
import LordOfTheRings.Enemy.Cards.CorsairPyromaniac
import LordOfTheRings.Enemy.Cards.EliteMarauder
import LordOfTheRings.Enemy.Cards.OrcAbductor
import LordOfTheRings.Enemy.Cards.SoldierOfAngmar
import LordOfTheRings.Enemy.Cards.RearguardAmbusher
import LordOfTheRings.Enemy.Cards.AngmarRaider
import LordOfTheRings.Enemy.Cards.UndeadHorde
import LordOfTheRings.Enemy.Cards.CursedDeadNightmare
import LordOfTheRings.Enemy.Cards.PatrolSentry
import LordOfTheRings.Enemy.Cards.GoblinLurker
import LordOfTheRings.Enemy.Cards.ShadowDweller
import LordOfTheRings.Enemy.Cards.OverseerMaurul
import LordOfTheRings.Enemy.Cards.OrcTaskmaster
import LordOfTheRings.Enemy.Cards.GoblinSkirmisher
import LordOfTheRings.Enemy.Cards.PitGoblin
import LordOfTheRings.Enemy.Cards.SwarmingGoblins
import LordOfTheRings.Enemy.Cards.CoalBlackOrc
import LordOfTheRings.Enemy.Cards.BoarClanAmbusher
import LordOfTheRings.Enemy.Cards.BoarClanSkirmisher
import LordOfTheRings.Enemy.Cards.WolfSpirit
import LordOfTheRings.Enemy.Cards.BoarSpirit
import LordOfTheRings.Enemy.Cards.RavenSpirit
import LordOfTheRings.Enemy.Cards.BellachsHunter
import LordOfTheRings.Enemy.Cards.OrcSeeker
import LordOfTheRings.Enemy.Cards.BloodHound
import LordOfTheRings.Enemy.Cards.DeadlyMarshAdder
import LordOfTheRings.Enemy.Cards.SwarmsOfMosquitos
import LordOfTheRings.Enemy.Cards.BellachsBodyguard
import LordOfTheRings.Enemy.Cards.LittleSnuffler
import LordOfTheRings.Enemy.Cards.DunlendingHorde
import LordOfTheRings.Enemy.Cards.RavenClanElite
import LordOfTheRings.Enemy.Cards.RavenClawDruid
import LordOfTheRings.Enemy.Cards.RaiderOfTheStorm
import LordOfTheRings.Enemy.Cards.TideRunner
import LordOfTheRings.Enemy.Cards.VenomsBite
import LordOfTheRings.Enemy.Cards.CorsairDuelist
import LordOfTheRings.Enemy.Cards.SlitheringArm
import LordOfTheRings.Enemy.Cards.CrashingArm
import LordOfTheRings.Enemy.Cards.KingsKnight
import LordOfTheRings.Enemy.Cards.CursedSoldier
import LordOfTheRings.Enemy.Cards.SubmergedDead
import LordOfTheRings.Enemy.Cards.GreatShark
import LordOfTheRings.Enemy.Cards.UnderseaCaveTroll
import LordOfTheRings.Enemy.Cards.RammingShip
import LordOfTheRings.Enemy.Cards.FangOfSahir
import LordOfTheRings.Enemy.Cards.PatrolShip
import LordOfTheRings.Enemy.Cards.CorsairLoyalist
import LordOfTheRings.Enemy.Cards.UmbarPatrolNightmare
import LordOfTheRings.Enemy.Cards.RavenousHawk
import LordOfTheRings.Enemy.Cards.HuntingWarg
import LordOfTheRings.Enemy.Cards.SavageHillTroll
import LordOfTheRings.Enemy.Cards.RobAndBob
import LordOfTheRings.Enemy.Cards.LouisNightmare
import LordOfTheRings.Enemy.Cards.MorrisNightmare
import LordOfTheRings.Enemy.Cards.RupertNightmare
import LordOfTheRings.Enemy.Cards.StuartNightmare
import LordOfTheRings.Enemy.Cards.GoblinEagleHunter
import LordOfTheRings.Enemy.Cards.TreetopBirdEater
import LordOfTheRings.Enemy.Cards.TunnelingNamelessThing
import LordOfTheRings.Enemy.Cards.OrcInterceptor
import LordOfTheRings.Enemy.Cards.ThingsInThePools
import LordOfTheRings.Enemy.Cards.MarshWight
import LordOfTheRings.Enemy.Cards.LostSoulOfLorien
import LordOfTheRings.Enemy.Cards.IllFatedGuard
import LordOfTheRings.Enemy.Cards.AmbushingOrcs
import LordOfTheRings.Enemy.Cards.PursuingWargRider
import LordOfTheRings.Enemy.Cards.WebSpinner
import LordOfTheRings.Enemy.Cards.WarriorOfNurn
import LordOfTheRings.Enemy.Cards.OrcCaptor
import LordOfTheRings.Enemy.Cards.SentryOfTheWhiteBridge
import LordOfTheRings.Enemy.Cards.BlackNumenoreanNightmare
import LordOfTheRings.Enemy.Cards.OrcTrickster
import LordOfTheRings.Enemy.Cards.UrukMarksman
import LordOfTheRings.Enemy.Cards.DruBuriDruNightmare
import LordOfTheRings.Enemy.Cards.DruadanWarrior
import LordOfTheRings.Enemy.Cards.DruadanStalker
import LordOfTheRings.Enemy.Cards.OrcRaidersNightmare
import LordOfTheRings.Enemy.Cards.PillagingOrc
import LordOfTheRings.Enemy.Cards.InvadingOrc
import LordOfTheRings.Enemy.Cards.HaradrimWarrior
import LordOfTheRings.Enemy.Cards.MurderousTurncoat
import LordOfTheRings.Enemy.Cards.FaithlessConspirator
import LordOfTheRings.Enemy.Cards.WolfpeltGoblin
import LordOfTheRings.Enemy.Cards.SilvertineMarauder
import LordOfTheRings.Enemy.Cards.SnowGiant
import LordOfTheRings.Enemy.Cards.UrukElfStalker
import LordOfTheRings.Enemy.Cards.GoblinTrapper
import LordOfTheRings.Enemy.Cards.RaidingParty
import LordOfTheRings.Enemy.Cards.ViciousTrackers
import LordOfTheRings.Enemy.Cards.TheWatcherNightmare
import LordOfTheRings.Enemy.Cards.WrithingTentacle
import LordOfTheRings.Enemy.Cards.ChokingTentacle
import LordOfTheRings.Enemy.Cards.GoblinStalker
import LordOfTheRings.Enemy.Cards.TrapdoorSpider
import LordOfTheRings.Enemy.Cards.SentinelOfTheDeep
import LordOfTheRings.Enemy.Cards.MithrilSeeker
import LordOfTheRings.Enemy.Cards.GiantCaveCentipede
import LordOfTheRings.Enemy.Cards.UnspeakableThing
import LordOfTheRings.Enemy.Cards.PrimevalThing
import LordOfTheRings.Enemy.Cards.ServantOfFlame
import LordOfTheRings.Enemy.Cards.CaptainOfMordor
import LordOfTheRings.Enemy.Cards.PackHunters
import LordOfTheRings.Enemy.Cards.JailorGornakhNightmare
import LordOfTheRings.Enemy.Cards.PrisonGuard
import LordOfTheRings.Enemy.Cards.OrcOfThaurdir
import LordOfTheRings.Enemy.Cards.HoarwellRiverSerpent
import LordOfTheRings.Enemy.Cards.ScavengingStoneTroll
import LordOfTheRings.Enemy.Cards.GiantBeetle
import LordOfTheRings.Enemy.Cards.ThaurdirsHunter
import LordOfTheRings.Enemy.Cards.UndeadThrall
import LordOfTheRings.Enemy.Cards.ApostateOfAngmar
import LordOfTheRings.Enemy.Cards.OrcWight
import LordOfTheRings.Enemy.Cards.ThaurdirsElite
import LordOfTheRings.Enemy.Cards.SilentGuardian
import LordOfTheRings.Enemy.Cards.NecromancerOfCarnDum
import LordOfTheRings.Enemy.Cards.SouthronCaptain
import LordOfTheRings.Enemy.Cards.SouthronSoldier
import LordOfTheRings.Enemy.Cards.SouthronArcher
import LordOfTheRings.Enemy.Cards.SandViper
import LordOfTheRings.Enemy.Cards.GiantScorpion
import LordOfTheRings.Enemy.Cards.UrukChieftain
import LordOfTheRings.Enemy.Cards.UrukOfMordor
import LordOfTheRings.Enemy.Cards.UrukWargRider
import LordOfTheRings.Enemy.Cards.MordorWarg
import LordOfTheRings.Enemy.Cards.SauronsEnforcers
import LordOfTheRings.Enemy.Cards.BroodMother
import LordOfTheRings.Enemy.Cards.HiveGuardian
import LordOfTheRings.Enemy.Cards.SpiderBroodling
import LordOfTheRings.Enemy.Cards.WereWorm
import LordOfTheRings.Enemy.Cards.CarrionBird
import LordOfTheRings.Enemy.Cards.UmbarSentry
import LordOfTheRings.Enemy.Cards.UrukPursuer
import LordOfTheRings.Enemy.Cards.RacingWarg
import LordOfTheRings.Enemy.Cards.TheBlackSerpent
import LordOfTheRings.Enemy.Cards.SerpentGuard
import LordOfTheRings.Enemy.Cards.JailerOfCirithGurat
import LordOfTheRings.Enemy.Cards.PrisonGuardCirithGurat
import LordOfTheRings.Enemy.Cards.DungeonWarg
import LordOfTheRings.Enemy.Cards.WildMumak
import LordOfTheRings.Enemy.Cards.HaradTiger
import LordOfTheRings.Enemy.Cards.TerritorialApe
import LordOfTheRings.Enemy.Cards.StranglingPython
import LordOfTheRings.Enemy.Cards.GiantCentipede
import LordOfTheRings.Enemy.Cards.TheBlackSerpentCrossingsOfPoros
import LordOfTheRings.Enemy.Cards.SouthronOutrider
import LordOfTheRings.Enemy.Cards.UrukProwler
import LordOfTheRings.Enemy.Cards.DesertWarg
import LordOfTheRings.Enemy.Cards.GoblinTroop
import LordOfTheRings.Enemy.Cards.StrayGoblinGreyMountains
import LordOfTheRings.Enemy.Cards.PackOfWargs
import LordOfTheRings.Enemy.Cards.StoneTroll
import LordOfTheRings.Enemy.Cards.Hobgoblin
import LordOfTheRings.Enemy.Cards.Dagnir
import LordOfTheRings.Enemy.Cards.GuardianOfGundabad
import LordOfTheRings.Enemy.Cards.DagnirsSlave
import LordOfTheRings.Enemy.Cards.DeepDweller
import LordOfTheRings.Enemy.Cards.Werewolf
import LordOfTheRings.Enemy.Cards.GiantSpider
import LordOfTheRings.Enemy.Cards.BlackBats
import LordOfTheRings.Enemy.Cards.Urdug
import LordOfTheRings.Enemy.Cards.UrdugsElite
import LordOfTheRings.Enemy.Cards.FierceVanguard
import LordOfTheRings.Enemy.Cards.WargRider
import LordOfTheRings.Enemy.Cards.DagnirMountGundabad
import LordOfTheRings.Enemy.Cards.TheDragonsThrall
import LordOfTheRings.Enemy.Cards.OrcOfTheFlame
import LordOfTheRings.Enemy.Cards.FireDrake
import LordOfTheRings.Enemy.Cards.FramsShade
import LordOfTheRings.Enemy.Cards.CryptStalker
import LordOfTheRings.Enemy.Cards.EvilSpectre
import LordOfTheRings.Enemy.Cards.ColdDrake
import LordOfTheRings.Enemy.Cards.SnowTroll
import LordOfTheRings.Enemy.Cards.DragonHatchling
import LordOfTheRings.Enemy.Cards.GreyMountainGiant
import LordOfTheRings.Enemy.Cards.HuntingEagle
import LordOfTheRings.Enemy.Cards.WilderlandBear
import LordOfTheRings.Enemy.Cards.UrdugEnemy
import LordOfTheRings.Enemy.Cards.TinyEnemy
import LordOfTheRings.Enemy.Cards.MirkwoodPatrol
import LordOfTheRings.Enemy.Cards.RavenousSpider
import LordOfTheRings.Enemy.Cards.GollumASitE
import LordOfTheRings.Enemy.Cards.KhamulTheEasterling
import LordOfTheRings.Enemy.Cards.EasterlingCaptain
import LordOfTheRings.Enemy.Cards.RiderOfRhun
import LordOfTheRings.Enemy.Cards.EasterlingRaider
import LordOfTheRings.Enemy.Cards.WarriorOfRhun
import LordOfTheRings.Enemy.Cards.MessengerFromMordor
import LordOfTheRings.Enemy.Cards.ServantOfSauron
import LordOfTheRings.Enemy.Cards.RhunBackstabber
import LordOfTheRings.Enemy.Cards.TreacherousEasterling
import LordOfTheRings.Enemy.Cards.UlchorsGuard
import LordOfTheRings.Enemy.Cards.EasterlingOutrider
import LordOfTheRings.Enemy.Cards.ThaneUlchorTheTempleOfDoom
import LordOfTheRings.Enemy.Cards.CityGuard
import LordOfTheRings.Enemy.Cards.UlfastSentry
import LordOfTheRings.Enemy.Cards.TheKingsElite
import LordOfTheRings.Enemy.Cards.UlchorsAgent
import LordOfTheRings.Enemy.Cards.NamelessBeast
import LordOfTheRings.Enemy.Cards.MordorCaveTroll
import LordOfTheRings.Enemy.Cards.AshMountainWerewolf
import LordOfTheRings.Enemy.Cards.SpawnOfShelob
import LordOfTheRings.Enemy.Cards.TorechGorgorPatrol
import LordOfTheRings.Enemy.Cards.TurnedAlly
import LordOfTheRings.Enemy.Cards.PriestOfSauron
import LordOfTheRings.Enemy.Cards.TheGovernor
import LordOfTheRings.Enemy.Cards.BlackNumenoreanASitE
import LordOfTheRings.Enemy.Cards.ThaneUlchorWrathAndRuin
import LordOfTheRings.Enemy.Cards.ChampionOfRhun
import LordOfTheRings.Enemy.Cards.EasterlingSkirmisher
import LordOfTheRings.Enemy.Cards.WainriderChampion
import LordOfTheRings.Enemy.Cards.AggressiveRacer
import LordOfTheRings.Enemy.Cards.ChariotDriver
import LordOfTheRings.Enemy.Cards.VeteranWainrider
import LordOfTheRings.Enemy.Cards.ThaneUlchorTheFortressOfNurn
import LordOfTheRings.Enemy.Cards.SoldierOfNurn
import LordOfTheRings.Enemy.Cards.EasterlingMercenary
import LordOfTheRings.Enemy.Cards.MordorOlogHai
import LordOfTheRings.Enemy.Cards.GuardDogs
import LordOfTheRings.Enemy.Cards.Ulwarth
import LordOfTheRings.Enemy.Cards.Lorgan
import LordOfTheRings.Enemy.Cards.Borlach
import LordOfTheRings.Enemy.Cards.EasterlingGuard
import LordOfTheRings.Enemy.Cards.NurnSlaver
import LordOfTheRings.Enemy.Cards.DeadManOfDunharrow (deadManOfDunharrow)
import LordOfTheRings.Enemy.Cards.FaithlessDead (faithlessDead)
import LordOfTheRings.Enemy.Cards.GhostsOfMen (ghostsOfMen)
import LordOfTheRings.Enemy.Cards.ShadowHost (shadowHost)
import LordOfTheRings.Enemy.Cards.ArmyOfTheDead (armyOfTheDead)
import LordOfTheRings.Enemy.Cards.WatcherInTheWater
import LordOfTheRings.Enemy.Cards.GreatWargChief
import LordOfTheRings.Enemy.Cards.HoundOfSauron
import LordOfTheRings.Enemy.Cards.HowlingWarg
import LordOfTheRings.Enemy.Cards.CrebainFromDunland
import LordOfTheRings.Enemy.Cards.GropingDead
import LordOfTheRings.Enemy.Cards.DeadThings
import LordOfTheRings.Enemy.Cards.CandleBearer
import LordOfTheRings.Enemy.Cards.MorgulWraith
import LordOfTheRings.Enemy.Cards.GollumTheLandOfShadow
import LordOfTheRings.Enemy.Cards.ManOfHarad
import LordOfTheRings.Enemy.Cards.CaptainOfHarad
import LordOfTheRings.Enemy.Cards.Oliphaunt
import LordOfTheRings.Enemy.Cards.HaradrimSoldier
import LordOfTheRings.Enemy.Cards.HaradrimArcher
import LordOfTheRings.Enemy.Cards.OrcOfMordor
import LordOfTheRings.Enemy.Cards.SnagaArcher
import LordOfTheRings.Enemy.Cards.UrukSoldierOrcsOfMordor
import LordOfTheRings.Enemy.Cards.Shagrat
import LordOfTheRings.Enemy.Cards.Gorbag
import LordOfTheRings.Enemy.Cards.WoundedUruk
import LordOfTheRings.Enemy.Cards.TheTwoWatchersEnemy
import LordOfTheRings.Enemy.Cards.Shelob
import LordOfTheRings.Enemy.Cards.MorgulOrc
import LordOfTheRings.Enemy.Cards.OrcOfCirithUngol
import LordOfTheRings.Enemy.Cards.OldTomnoddy
import LordOfTheRings.Enemy.Cards.CrazyCob
import LordOfTheRings.Enemy.Cards.LazyLob
import LordOfTheRings.Enemy.Cards.FatSpider
import LordOfTheRings.Enemy.Cards.WickedSpider
import LordOfTheRings.Enemy.Cards.DarkBats
import LordOfTheRings.Enemy.Cards.Bolg
import LordOfTheRings.Enemy.Cards.BodyguardOfBolg
import LordOfTheRings.Enemy.Cards.GundabadWolfRider
import LordOfTheRings.Enemy.Cards.GundabadClimber
import LordOfTheRings.Enemy.Cards.VanguardOfBolg
import LordOfTheRings.Enemy.Cards.GundabadArcher
import LordOfTheRings.Enemy.Cards.RavenousWarg
import LordOfTheRings.Enemy.Cards.William
import LordOfTheRings.Enemy.Cards.Bert
import LordOfTheRings.Enemy.Cards.Tom
import LordOfTheRings.Enemy.Cards.ASuspiciousCrow
import LordOfTheRings.Enemy.Cards.GoblinAxeman
import LordOfTheRings.Enemy.Cards.GoblinMiners
import LordOfTheRings.Enemy.Cards.GoblinRunners
import LordOfTheRings.Enemy.Cards.GoblinDriver
import LordOfTheRings.Enemy.Cards.StoneGiant
import LordOfTheRings.Enemy.Cards.GoblinBentSwords
import LordOfTheRings.Enemy.Cards.TheGreatGoblin
import LordOfTheRings.Enemy.Cards.GollumDungeonsDeep
import LordOfTheRings.Enemy.Cards.GreatGrayWolf
import LordOfTheRings.Enemy.Cards.WildWargs
import LordOfTheRings.Enemy.Cards.SmaugTheGolden (smaugTheGolden)
import LordOfTheRings.Enemy.Cards.SmaugTheMagnificent (smaugTheMagnificent)
import LordOfTheRings.Enemy.Cards.CroakingCrows (croakingCrows)
import LordOfTheRings.Enemy.Cards.BlackRider
import LordOfTheRings.Enemy.Cards.EvilCrowAShadowOfThePast
import LordOfTheRings.Enemy.Cards.TheWitchKing
import LordOfTheRings.Enemy.Cards.Ringwraith
import LordOfTheRings.Enemy.Cards.FellRider
import LordOfTheRings.Enemy.Cards.RiderOfMordor
import LordOfTheRings.Enemy.Cards.BillFerny
import LordOfTheRings.Enemy.Cards.ShadyBreeLander
import LordOfTheRings.Enemy.Cards.SquintEyedSoutherner
import LordOfTheRings.Enemy.Cards.TheBlackSerpentPelennor (theBlackSerpentPelennor)
import LordOfTheRings.Enemy.Cards.WarMumak (warMumak)
import LordOfTheRings.Enemy.Cards.SouthronChampion (southronChampion)
import LordOfTheRings.Enemy.Cards.MorgulCaptain (morgulCaptain)
import LordOfTheRings.Enemy.Cards.OrcOfTheEye (orcOfTheEye)
import LordOfTheRings.Enemy.Cards.MordorOrc (mordorOrc)
import LordOfTheRings.Enemy.Cards.WraithOnWings (wraithOnWings)
import LordOfTheRings.Enemy.Cards.TheWitchKingCaptain (theWitchKingCaptain, theWitchKingSorcerer)
import LordOfTheRings.Enemy.Cards.CorsairCaptain (corsairCaptain)
import LordOfTheRings.Enemy.Cards.CorsairOfUmbar (corsairOfUmbar)
import LordOfTheRings.Enemy.Cards.UmbarPirateCrew (umbarPirateCrew)
import LordOfTheRings.Enemy.Cards.HaradrimPirate (haradrimPirate)
import LordOfTheRings.Enemy.Cards.TheMouthOfSauron
import LordOfTheRings.Enemy.Cards.NazgulOfMordor
import LordOfTheRings.Enemy.Cards.GorgorothHillTroll
import LordOfTheRings.Enemy.Cards.EasterlingBowman
import LordOfTheRings.Enemy.Cards.WarriorOfRhunTheBlackGateOpens
import LordOfTheRings.Enemy.Cards.GollumMountDoom
import LordOfTheRings.Enemy.Cards.RingwraithOfSauron
import LordOfTheRings.Enemy.Cards.OrcCaptain
import LordOfTheRings.Enemy.Cards.BigUruk
import LordOfTheRings.Enemy.Cards.LittleSnufflerMountDoom
import LordOfTheRings.Enemy.Cards.TheBalrog
import LordOfTheRings.Enemy.Cards.OrcChieftain
import LordOfTheRings.Enemy.Cards.GreatCaveTrollJourneyInTheDark
import LordOfTheRings.Enemy.Cards.UrukFromMordor
import LordOfTheRings.Enemy.Cards.MoriaOrc
import LordOfTheRings.Enemy.Cards.MoriaArcher
import LordOfTheRings.Enemy.Cards.UrukHaiCaptain
import LordOfTheRings.Enemy.Cards.UrukHaiArcher
import LordOfTheRings.Enemy.Cards.UrukHaiTracker
import LordOfTheRings.Enemy.Cards.UrukHaiHunter
import LordOfTheRings.Enemy.Cards.SarumanTheRoadToIsengard
import LordOfTheRings.Enemy.Cards.GrimaTheRoadToIsengard
import LordOfTheRings.Enemy.Cards.LieutenantOfOrthanc
import LordOfTheRings.Enemy.Cards.IsengardOrc
import LordOfTheRings.Enemy.Cards.WolfOfIsengard
import LordOfTheRings.Enemy.Cards.Ugluk
import LordOfTheRings.Enemy.Cards.Grishnakh
import LordOfTheRings.Enemy.Cards.UgluksUrukHai
import LordOfTheRings.Enemy.Cards.IsengardUruk
import LordOfTheRings.Enemy.Cards.UrukHaiFighter
import LordOfTheRings.Enemy.Cards.Snaga
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
  -- an ally turned enemy, as Secret Cultist turns it, or a hero, as Fallen into Evil does
  Nothing | cdCardType (toCardDef cardCode) `elem` [AllyType, HeroType] -> Enemy $ cbCardBuilder (turnedAlly cardCode) cardId ident
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
        , SomeEnemyCard stormcallerCampaign
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
        , SomeEnemyCard templeGuardianCampaign
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
        , SomeEnemyCard thunderstruck
        , SomeEnemyCard crowley
        , SomeEnemyCard revenant
        , SomeEnemyCard graxar
        , SomeEnemyCard seaSerpent
        , SomeEnemyCard seaSerpentEnraged
        , SomeEnemyCard recurringNightmare
        , SomeEnemyCard cunningInformant
        , SomeEnemyCard harborBrute
        , SomeEnemyCard pelargirRingleader
        , SomeEnemyCard mumakElite
        , SomeEnemyCard haradrimMarksman
        , SomeEnemyCard haradrimCaptain
        , SomeEnemyCard southronMountedArcher
        , SomeEnemyCard orcSaboteur
        , SomeEnemyCard orcRavagerNightmare
        , SomeEnemyCard dunlandAssassin
        , SomeEnemyCard vengefulHillman
        , SomeEnemyCard savageHillman
        , SomeEnemyCard vigilantOrc
        , SomeEnemyCard methedrasLookout
        , SomeEnemyCard guardWarg
        , SomeEnemyCard huornOfFangorn
        , SomeEnemyCard blockingHuorn
        , SomeEnemyCard wickedHuorn
        , SomeEnemyCard brashCorsair
        , SomeEnemyCard sahirsPursuer
        , SomeEnemyCard guardianOfTheGoldenKing
        , SomeEnemyCard corsairPillager
        , SomeEnemyCard corsairPyromaniac
        , SomeEnemyCard eliteMarauder
        , SomeEnemyCard orcAbductor
        , SomeEnemyCard soldierOfAngmar
        , SomeEnemyCard rearguardAmbusher
        , SomeEnemyCard angmarRaider
        , SomeEnemyCard undeadHorde
        , SomeEnemyCard cursedDeadNightmare
        , SomeEnemyCard patrolSentry
        , SomeEnemyCard goblinLurker
        , SomeEnemyCard shadowDweller
        , SomeEnemyCard overseerMaurul
        , SomeEnemyCard orcTaskmaster
        , SomeEnemyCard goblinSkirmisher
        , SomeEnemyCard pitGoblin
        , SomeEnemyCard swarmingGoblins
        , SomeEnemyCard coalBlackOrc
        , SomeEnemyCard boarClanAmbusher
        , SomeEnemyCard boarClanSkirmisher
        , SomeEnemyCard wolfSpirit
        , SomeEnemyCard boarSpirit
        , SomeEnemyCard ravenSpirit
        , SomeEnemyCard bellachsHunter
        , SomeEnemyCard orcSeeker
        , SomeEnemyCard bloodHound
        , SomeEnemyCard deadlyMarshAdder
        , SomeEnemyCard swarmsOfMosquitos
        , SomeEnemyCard bellachsBodyguard
        , SomeEnemyCard littleSnuffler
        , SomeEnemyCard dunlendingHorde
        , SomeEnemyCard ravenClanElite
        , SomeEnemyCard ravenClawDruid
        , SomeEnemyCard raiderOfTheStorm
        , SomeEnemyCard tideRunner
        , SomeEnemyCard venomsBite
        , SomeEnemyCard corsairDuelist
        , SomeEnemyCard slitheringArm
        , SomeEnemyCard crashingArm
        , SomeEnemyCard kingsKnight
        , SomeEnemyCard cursedSoldier
        , SomeEnemyCard submergedDead
        , SomeEnemyCard greatShark
        , SomeEnemyCard underseaCaveTroll
        , SomeEnemyCard rammingShip
        , SomeEnemyCard fangOfSahir
        , SomeEnemyCard patrolShip
        , SomeEnemyCard corsairLoyalist
        , SomeEnemyCard umbarPatrolNightmare
        , SomeEnemyCard ravenousHawk
        , SomeEnemyCard huntingWarg
        , SomeEnemyCard savageHillTroll
        , SomeEnemyCard robAndBob
        , SomeEnemyCard louisNightmare
        , SomeEnemyCard morrisNightmare
        , SomeEnemyCard rupertNightmare
        , SomeEnemyCard stuartNightmare
        , SomeEnemyCard goblinEagleHunter
        , SomeEnemyCard treetopBirdEater
        , SomeEnemyCard tunnelingNamelessThing
        , SomeEnemyCard orcInterceptor
        , SomeEnemyCard thingsInThePools
        , SomeEnemyCard marshWight
        , SomeEnemyCard lostSoulOfLorien
        , SomeEnemyCard illFatedGuard
        , SomeEnemyCard ambushingOrcs
        , SomeEnemyCard pursuingWargRider
        , SomeEnemyCard webSpinner
        , SomeEnemyCard warriorOfNurn
        , SomeEnemyCard orcCaptor
        , SomeEnemyCard sentryOfTheWhiteBridge
        , SomeEnemyCard blackNumenoreanNightmare
        , SomeEnemyCard orcTrickster
        , SomeEnemyCard urukMarksman
        , SomeEnemyCard druBuriDruNightmare
        , SomeEnemyCard druadanWarrior
        , SomeEnemyCard druadanStalker
        , SomeEnemyCard orcRaidersNightmare
        , SomeEnemyCard pillagingOrc
        , SomeEnemyCard invadingOrc
        , SomeEnemyCard haradrimWarrior
        , SomeEnemyCard murderousTurncoat
        , SomeEnemyCard faithlessConspirator
        , SomeEnemyCard wolfpeltGoblin
        , SomeEnemyCard silvertineMarauder
        , SomeEnemyCard snowGiant
        , SomeEnemyCard urukElfStalker
        , SomeEnemyCard goblinTrapper
        , SomeEnemyCard raidingParty
        , SomeEnemyCard viciousTrackers
        , SomeEnemyCard theWatcherNightmare
        , SomeEnemyCard writhingTentacle
        , SomeEnemyCard chokingTentacle
        , SomeEnemyCard goblinStalker
        , SomeEnemyCard trapdoorSpider
        , SomeEnemyCard sentinelOfTheDeep
        , SomeEnemyCard mithrilSeeker
        , SomeEnemyCard giantCaveCentipede
        , SomeEnemyCard unspeakableThing
        , SomeEnemyCard primevalThing
        , SomeEnemyCard servantOfFlame
        , SomeEnemyCard captainOfMordor
        , SomeEnemyCard packHunters
        , SomeEnemyCard jailorGornakhNightmare
        , SomeEnemyCard prisonGuard
        , SomeEnemyCard orcOfThaurdir
        , SomeEnemyCard hoarwellRiverSerpent
        , SomeEnemyCard scavengingStoneTroll
        , SomeEnemyCard giantBeetle
        , SomeEnemyCard thaurdirsHunter
        , SomeEnemyCard undeadThrall
        , SomeEnemyCard apostateOfAngmar
        , SomeEnemyCard orcWight
        , SomeEnemyCard thaurdirsElite
        , SomeEnemyCard silentGuardian
        , SomeEnemyCard necromancerOfCarnDum
        , SomeEnemyCard southronCaptain
        , SomeEnemyCard southronSoldier
        , SomeEnemyCard southronArcher
        , SomeEnemyCard sandViper
        , SomeEnemyCard giantScorpion
        , SomeEnemyCard urukChieftain
        , SomeEnemyCard urukOfMordor
        , SomeEnemyCard urukWargRider
        , SomeEnemyCard mordorWarg
        , SomeEnemyCard sauronsEnforcers
        , SomeEnemyCard broodMother
        , SomeEnemyCard hiveGuardian
        , SomeEnemyCard spiderBroodling
        , SomeEnemyCard wereWorm
        , SomeEnemyCard carrionBird
        , SomeEnemyCard umbarSentry
        , SomeEnemyCard urukPursuer
        , SomeEnemyCard racingWarg
        , SomeEnemyCard theBlackSerpent
        , SomeEnemyCard serpentGuard
        , SomeEnemyCard jailerOfCirithGurat
        , SomeEnemyCard prisonGuardCirithGurat
        , SomeEnemyCard dungeonWarg
        , SomeEnemyCard wildMumak
        , SomeEnemyCard haradTiger
        , SomeEnemyCard territorialApe
        , SomeEnemyCard stranglingPython
        , SomeEnemyCard giantCentipede
        , SomeEnemyCard theBlackSerpentCrossingsOfPoros
        , SomeEnemyCard southronOutrider
        , SomeEnemyCard urukProwler
        , SomeEnemyCard desertWarg
        , SomeEnemyCard goblinTroop
        , SomeEnemyCard strayGoblinGreyMountains
        , SomeEnemyCard packOfWargs
        , SomeEnemyCard stoneTroll
        , SomeEnemyCard hobgoblin
        , SomeEnemyCard dagnir
        , SomeEnemyCard guardianOfGundabad
        , SomeEnemyCard dagnirsSlave
        , SomeEnemyCard deepDweller
        , SomeEnemyCard werewolf
        , SomeEnemyCard giantSpider
        , SomeEnemyCard blackBats
        , SomeEnemyCard urdug
        , SomeEnemyCard urdugsElite
        , SomeEnemyCard fierceVanguard
        , SomeEnemyCard wargRider
        , SomeEnemyCard dagnirMountGundabad
        , SomeEnemyCard theDragonsThrall
        , SomeEnemyCard orcOfTheFlame
        , SomeEnemyCard fireDrake
        , SomeEnemyCard framsShade
        , SomeEnemyCard cryptStalker
        , SomeEnemyCard evilSpectre
        , SomeEnemyCard coldDrake
        , SomeEnemyCard snowTroll
        , SomeEnemyCard dragonHatchling
        , SomeEnemyCard greyMountainGiant
        , SomeEnemyCard huntingEagle
        , SomeEnemyCard wilderlandBear
        , SomeEnemyCard urdugEnemy
        , SomeEnemyCard tinyEnemy
        , SomeEnemyCard mirkwoodPatrol
        , SomeEnemyCard ravenousSpider
        , SomeEnemyCard gollumASitE
        , SomeEnemyCard khamulTheEasterling
        , SomeEnemyCard easterlingCaptain
        , SomeEnemyCard riderOfRhun
        , SomeEnemyCard easterlingRaider
        , SomeEnemyCard warriorOfRhun
        , SomeEnemyCard messengerFromMordor
        , SomeEnemyCard servantOfSauron
        , SomeEnemyCard rhunBackstabber
        , SomeEnemyCard treacherousEasterling
        , SomeEnemyCard ulchorsGuard
        , SomeEnemyCard easterlingOutrider
        , SomeEnemyCard thaneUlchorTheTempleOfDoom
        , SomeEnemyCard cityGuard
        , SomeEnemyCard ulfastSentry
        , SomeEnemyCard theKingsElite
        , SomeEnemyCard ulchorsAgent
        , SomeEnemyCard namelessBeast
        , SomeEnemyCard mordorCaveTroll
        , SomeEnemyCard ashMountainWerewolf
        , SomeEnemyCard spawnOfShelob
        , SomeEnemyCard torechGorgorPatrol
        , SomeEnemyCard priestOfSauron
        , SomeEnemyCard theGovernor
        , SomeEnemyCard blackNumenoreanASitE
        , SomeEnemyCard thaneUlchorWrathAndRuin
        , SomeEnemyCard championOfRhun
        , SomeEnemyCard easterlingSkirmisher
        , SomeEnemyCard wainriderChampion
        , SomeEnemyCard aggressiveRacer
        , SomeEnemyCard chariotDriver
        , SomeEnemyCard veteranWainrider
        , SomeEnemyCard thaneUlchorTheFortressOfNurn
        , SomeEnemyCard soldierOfNurn
        , SomeEnemyCard easterlingMercenary
        , SomeEnemyCard mordorOlogHai
        , SomeEnemyCard guardDogs
        , SomeEnemyCard ulwarth
        , SomeEnemyCard lorgan
        , SomeEnemyCard borlach
        , SomeEnemyCard easterlingGuard
        , SomeEnemyCard nurnSlaver
        , SomeEnemyCard deadManOfDunharrow
        , SomeEnemyCard faithlessDead
        , SomeEnemyCard ghostsOfMen
        , SomeEnemyCard shadowHost
        , SomeEnemyCard armyOfTheDead
        , SomeEnemyCard watcherInTheWater
        , SomeEnemyCard greatWargChief
        , SomeEnemyCard houndOfSauron
        , SomeEnemyCard howlingWarg
        , SomeEnemyCard crebainFromDunland
        , SomeEnemyCard gropingDead
        , SomeEnemyCard deadThings
        , SomeEnemyCard candleBearer
        , SomeEnemyCard morgulWraith
        , SomeEnemyCard gollumTheLandOfShadow
        , SomeEnemyCard manOfHarad
        , SomeEnemyCard captainOfHarad
        , SomeEnemyCard oliphaunt
        , SomeEnemyCard haradrimSoldier
        , SomeEnemyCard haradrimArcher
        , SomeEnemyCard shagrat
        , SomeEnemyCard gorbag
        , SomeEnemyCard woundedUruk
        , SomeEnemyCard theTwoWatchersEnemy
        , SomeEnemyCard orcOfMordor
        , SomeEnemyCard snagaArcher
        , SomeEnemyCard urukSoldierOrcsOfMordor
        , SomeEnemyCard shelob
        , SomeEnemyCard morgulOrc
        , SomeEnemyCard orcOfCirithUngol
        , SomeEnemyCard oldTomnoddy
        , SomeEnemyCard crazyCob
        , SomeEnemyCard lazyLob
        , SomeEnemyCard fatSpider
        , SomeEnemyCard wickedSpider
        , SomeEnemyCard darkBats
        , SomeEnemyCard bolg
        , SomeEnemyCard bodyguardOfBolg
        , SomeEnemyCard gundabadWolfRider
        , SomeEnemyCard gundabadClimber
        , SomeEnemyCard vanguardOfBolg
        , SomeEnemyCard gundabadArcher
        , SomeEnemyCard ravenousWarg
        , SomeEnemyCard william
        , SomeEnemyCard bert
        , SomeEnemyCard tom
        , SomeEnemyCard aSuspiciousCrow
        , SomeEnemyCard goblinAxeman
        , SomeEnemyCard goblinMiners
        , SomeEnemyCard goblinRunners
        , SomeEnemyCard goblinDriver
        , SomeEnemyCard stoneGiant
        , SomeEnemyCard goblinBentSwords
        , SomeEnemyCard theGreatGoblin
        , SomeEnemyCard gollumDungeonsDeep
        , SomeEnemyCard greatGrayWolf
        , SomeEnemyCard wildWargs
        , SomeEnemyCard smaugTheGolden
        , SomeEnemyCard smaugTheMagnificent
        , SomeEnemyCard croakingCrows
        , SomeEnemyCard blackRider
        , SomeEnemyCard evilCrowAShadowOfThePast
        , SomeEnemyCard theWitchKing
        , SomeEnemyCard ringwraith
        , SomeEnemyCard fellRider
        , SomeEnemyCard riderOfMordor
        , SomeEnemyCard billFerny
        , SomeEnemyCard shadyBreeLander
        , SomeEnemyCard squintEyedSoutherner
        , SomeEnemyCard theBlackSerpentPelennor
        , SomeEnemyCard warMumak
        , SomeEnemyCard southronChampion
        , SomeEnemyCard morgulCaptain
        , SomeEnemyCard orcOfTheEye
        , SomeEnemyCard mordorOrc
        , SomeEnemyCard wraithOnWings
        , SomeEnemyCard theWitchKingCaptain
        , SomeEnemyCard theWitchKingSorcerer
        , SomeEnemyCard corsairCaptain
        , SomeEnemyCard corsairOfUmbar
        , SomeEnemyCard umbarPirateCrew
        , SomeEnemyCard haradrimPirate
        , SomeEnemyCard theMouthOfSauron
        , SomeEnemyCard nazgulOfMordor
        , SomeEnemyCard gorgorothHillTroll
        , SomeEnemyCard easterlingBowman
        , SomeEnemyCard warriorOfRhunTheBlackGateOpens
        , SomeEnemyCard gollumMountDoom
        , SomeEnemyCard ringwraithOfSauron
        , SomeEnemyCard orcCaptain
        , SomeEnemyCard bigUruk
        , SomeEnemyCard littleSnufflerMountDoom
        , SomeEnemyCard theBalrog
        , SomeEnemyCard orcChieftain
        , SomeEnemyCard greatCaveTrollJourneyInTheDark
        , SomeEnemyCard urukFromMordor
        , SomeEnemyCard moriaOrc
        , SomeEnemyCard moriaArcher
        , SomeEnemyCard urukHaiCaptain
        , SomeEnemyCard urukHaiArcher
        , SomeEnemyCard urukHaiTracker
        , SomeEnemyCard urukHaiHunter
        , SomeEnemyCard sarumanTheRoadToIsengard
        , SomeEnemyCard grimaTheRoadToIsengard
        , SomeEnemyCard lieutenantOfOrthanc
        , SomeEnemyCard isengardOrc
        , SomeEnemyCard wolfOfIsengard
        , SomeEnemyCard ugluk
        , SomeEnemyCard grishnakh
        , SomeEnemyCard ugluksUrukHai
        , SomeEnemyCard isengardUruk
        , SomeEnemyCard urukHaiFighter
        , SomeEnemyCard snaga
        ]
    ]
