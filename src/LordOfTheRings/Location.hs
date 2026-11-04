{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location (module LordOfTheRings.Location, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Location.Cards.BanksOfTheAnduin
import LordOfTheRings.Location.Cards.DreadfulGap
import LordOfTheRings.Location.Cards.FouledWell
import LordOfTheRings.Location.Cards.GoblinTunnels
import LordOfTheRings.Location.Cards.KneesOfTheMountain
import LordOfTheRings.Location.Cards.TurbulentWaters
import LordOfTheRings.Location.Cards.WargLair
import LordOfTheRings.Location.Cards.EastGate
import LordOfTheRings.Location.Cards.FirstHall
import LordOfTheRings.Location.Cards.BridgeOfKhazadDum
import LordOfTheRings.Location.Cards.StairsOfNain
import LordOfTheRings.Location.Cards.BranchingPaths
import LordOfTheRings.Location.Cards.LightlessPassage
import LordOfTheRings.Location.Cards.ZigilMineshaft
import LordOfTheRings.Location.Cards.EnchantedStream
import LordOfTheRings.Location.Cards.EndlessCaverns
import LordOfTheRings.Location.Cards.ForestGate
import LordOfTheRings.Location.Cards.GladdenFields
import LordOfTheRings.Location.Cards.GreatForestWeb
import LordOfTheRings.Location.Cards.MountainsOfMirkwood
import LordOfTheRings.Location.Cards.NecromancersPass
import LordOfTheRings.Location.Cards.OldForestRoad
import LordOfTheRings.Location.Cards.TheBrownLands
import LordOfTheRings.Location.Cards.TheEastBight
import LordOfTheRings.Location.Cards.TowerGate
import LordOfTheRings.Location.Cards.TheOldFord
import LordOfTheRings.Location.Cards.TheEavesOfMirkwood
import LordOfTheRings.Location.Cards.RiverNinglor
import LordOfTheRings.Location.Cards.TheEastBank
import LordOfTheRings.Location.Cards.TheWestBank
import LordOfTheRings.Location.Cards.TheCarrock
import LordOfTheRings.Location.Cards.RiverLangflood
import LordOfTheRings.Location.Cards.BeePastures
import LordOfTheRings.Location.Cards.OakWoodGrove
import LordOfTheRings.Location.Cards.Rhosgobel
import LordOfTheRings.Location.Cards.ForestGrove
import LordOfTheRings.Location.Cards.AmonHen
import LordOfTheRings.Location.Cards.AmonLhaw
import LordOfTheRings.Location.Cards.TheEastWallOfRohan
import LordOfTheRings.Location.Cards.TheNorthStair
import LordOfTheRings.Location.Cards.RaurosFalls
import LordOfTheRings.Location.Cards.TheShoresOfNenHithoel
import LordOfTheRings.Location.Cards.TheOuterRidge
import LordOfTheRings.Location.Cards.TheHighlands
import LordOfTheRings.Location.Cards.ImpassableBog
import LordOfTheRings.Location.Cards.TheHeartOfTheMarshes
import LordOfTheRings.Location.Cards.FensAndMires
import LordOfTheRings.Location.Cards.TheSpidersRing
import LordOfTheRings.Location.Cards.DryWatercourse
import LordOfTheRings.Location.Cards.WoodmansGlade
import LordOfTheRings.Location.Cards.WoodElfPath
import LordOfTheRings.Location.Cards.TheMountainsRoots
import LordOfTheRings.Location.Cards.PlunderedArmoury
import LordOfTheRings.Location.Cards.UpperHall
import LordOfTheRings.Location.Cards.AbandonedMine
import LordOfTheRings.Location.Cards.DwarvenForge
import LordOfTheRings.Location.Cards.SilentCaverns
import LordOfTheRings.Location.Cards.TwistingPassage
import LordOfTheRings.Location.Cards.RuinedRoad
import LordOfTheRings.Location.Cards.GoblinGate
import LordOfTheRings.Location.Cards.PathlessCountry
import LordOfTheRings.Location.Cards.BarrenHills
import LordOfTheRings.Location.Cards.Caradhras
import LordOfTheRings.Location.Cards.Fanuidhol
import LordOfTheRings.Location.Cards.Celebdil
import LordOfTheRings.Location.Cards.TheDimrillStair
import LordOfTheRings.Location.Cards.RockyCrags
import LordOfTheRings.Location.Cards.DrownedTreasury
import LordOfTheRings.Location.Cards.MithrilLode
import LordOfTheRings.Location.Cards.DarkPit
import LordOfTheRings.Location.Cards.SecondHall
import LordOfTheRings.Location.Cards.SecondDeep
import LordOfTheRings.Location.Cards.DoorsOfDurin
import LordOfTheRings.Location.Cards.StairFalls
import LordOfTheRings.Location.Cards.PerilousSwamp
import LordOfTheRings.Location.Cards.MakeshiftPassage
import LordOfTheRings.Location.Cards.StagnantCreek
import LordOfTheRings.Location.Cards.TheCitadel
import LordOfTheRings.Location.Cards.TheApproach
import LordOfTheRings.Location.Cards.TheBanks
import LordOfTheRings.Location.Cards.OrcWarCamp
import LordOfTheRings.Location.Cards.IthilienRoad
import LordOfTheRings.Location.Cards.OvergrownTrail
import LordOfTheRings.Location.Cards.SecludedGlade
import LordOfTheRings.Location.Cards.TheLeapingFish
import LordOfTheRings.Location.Cards.PelargirDocks
import LordOfTheRings.Location.Cards.HarborStorehouse
import LordOfTheRings.Location.Cards.CityStreet
import LordOfTheRings.Location.Cards.MarketSquare
import LordOfTheRings.Location.Cards.BurningFarmhouse
import LordOfTheRings.Location.Cards.GondorianHamlet
import LordOfTheRings.Location.Cards.SecludedFarmhouse
import LordOfTheRings.Location.Cards.AncestralClearing
import LordOfTheRings.Location.Cards.GardenOfPoisons
import LordOfTheRings.Location.Cards.GladeOfCleansing
import LordOfTheRings.Location.Cards.MorgulRoad
import LordOfTheRings.Location.Cards.MorgulVale
import LordOfTheRings.Location.Cards.TheWhiteBridge
import LordOfTheRings.Location.Cards.Sewers
import LordOfTheRings.Location.Cards.RootsOfMindolluin
import LordOfTheRings.Location.Cards.Storehouse
import LordOfTheRings.Location.Cards.HousesOfTheDead
import LordOfTheRings.Location.Cards.TheFourthStar
import LordOfTheRings.Location.Cards.WestGate
import LordOfTheRings.Location.Cards.TheKingsLibrary
import LordOfTheRings.Location.Cards.AncientHarbor
import LordOfTheRings.Location.Cards.TheOldBridge
import LordOfTheRings.Location.Cards.WestQuarter
import LordOfTheRings.Location.Cards.EastQuarter
import LordOfTheRings.Location.Cards.RuinedSquare
import LordOfTheRings.Location.Cards.RuinedTower
import LordOfTheRings.Location.Cards.TheCrossroads
import LordOfTheRings.Location.Cards.IthilienRoads
import LordOfTheRings.Location.Cards.TheDarkWoods
import LordOfTheRings.Location.Cards.Methedras
import LordOfTheRings.Location.Cards.OrcCave
import LordOfTheRings.Location.Cards.MugashsLair
import LordOfTheRings.Location.Cards.EdgeOfFangorn
import LordOfTheRings.Location.Cards.HeartOfFangorn
import LordOfTheRings.Location.Cards.TheIslet
import LordOfTheRings.Location.Cards.FordsOfIsen
import LordOfTheRings.Location.Cards.TheKingsRoad
import LordOfTheRings.Location.Cards.GapOfRohan
import LordOfTheRings.Location.Cards.AncientForestLocation
import LordOfTheRings.Location.Cards.TangledWoods
import LordOfTheRings.Location.Cards.BrokenLandsLocation
import LordOfTheRings.Location.Cards.RavenChiefsCamp
import LordOfTheRings.Location.Cards.RavenWarCamp
import LordOfTheRings.Location.Cards.RavenVillage
import LordOfTheRings.Location.Cards.DunlandBattlefield
import LordOfTheRings.Location.Cards.RavenCountry
import LordOfTheRings.Location.Cards.MunuvDuvRavine
import LordOfTheRings.Location.Cards.OldSouthRoad
import LordOfTheRings.Location.Cards.HithaeglirFoothills
import LordOfTheRings.Location.Cards.HillsOfDunland
import LordOfTheRings.Location.Cards.PlainsOfEnedwaith
import LordOfTheRings.Location.Cards.HithaeglirStream
import LordOfTheRings.Location.Cards.TheEmptyMug
import LordOfTheRings.Location.Cards.TheCrossingAtTharbad
import LordOfTheRings.Location.Cards.StreetsOfTharbad
import LordOfTheRings.Location.Cards.TharbadHideout
import LordOfTheRings.Location.Cards.RuinsOfTheSecondAge
import LordOfTheRings.Location.Cards.SeedyInn
import LordOfTheRings.Location.Cards.HiddenAlleyway
import LordOfTheRings.Location.Cards.DecrepitRooftops
import LordOfTheRings.Location.Cards.TheSecretChamber
import LordOfTheRings.Location.Cards.RuinedPlaza
import LordOfTheRings.Location.Cards.CollapsedTower
import LordOfTheRings.Location.Cards.AncientFoundation
import LordOfTheRings.Location.Cards.CityRemains
import LordOfTheRings.Location.Cards.FenOfReeds
import LordOfTheRings.Location.Cards.FingerOfGlanduin
import LordOfTheRings.Location.Cards.HiddenEyot
import LordOfTheRings.Location.Cards.SinkingBog
import LordOfTheRings.Location.Cards.HillBarrow
import LordOfTheRings.Location.Cards.CaveBarrow
import LordOfTheRings.Location.Cards.StoneBarrow
import LordOfTheRings.Location.Cards.HallowedCircle
import LordOfTheRings.Location.Cards.CursedForest
import LordOfTheRings.Location.Cards.GrimFoothills
import LordOfTheRings.Location.Cards.AccursedBattlefield
import LordOfTheRings.Location.Cards.MountainsOfAngmar
import LordOfTheRings.Location.Cards.BlightOfCarnDum
import LordOfTheRings.Location.Cards.FortressWalls
import LordOfTheRings.Location.Cards.EriadorWastes
import LordOfTheRings.Location.Cards.WargsDen
import LordOfTheRings.Location.Cards.NorthDowns
import LordOfTheRings.Location.Cards.BrokenBattlements
import LordOfTheRings.Location.Cards.NorburyTombs
import LordOfTheRings.Location.Cards.FornostSquare
import LordOfTheRings.Location.Cards.HauntedKeep
import LordOfTheRings.Location.Cards.DeadmensGate
import LordOfTheRings.Location.Cards.RuinsOfArnor
import LordOfTheRings.Location.Cards.AncientCauseway
import LordOfTheRings.Location.Cards.SouthernGate
import LordOfTheRings.Location.Cards.PrisonCell
import LordOfTheRings.Location.Cards.PatrolRoom
import LordOfTheRings.Location.Cards.TunnelsOfMountGram
import LordOfTheRings.Location.Cards.ChetwoodForest
import LordOfTheRings.Location.Cards.BordersOfBreeLand
import LordOfTheRings.Location.Cards.OutlyingHomestead
import LordOfTheRings.Location.Cards.RuggedCountry
import LordOfTheRings.Location.Cards.ShroudedHills
import LordOfTheRings.Location.Cards.AmonForn
import LordOfTheRings.Location.Cards.WeatheredHilltop
import LordOfTheRings.Location.Cards.ExposedRidge
import LordOfTheRings.Location.Cards.ShelteredValley
import LordOfTheRings.Location.Cards.ConcealedOrcCamp
import LordOfTheRings.Location.Cards.SecludedCave
import LordOfTheRings.Location.Cards.TheHoarwell
import LordOfTheRings.Location.Cards.AbandonedCamp
import LordOfTheRings.Location.Cards.PatchOfWoods
import LordOfTheRings.Location.Cards.TrollFells
import LordOfTheRings.Location.Cards.BarrenMoorland
import LordOfTheRings.Location.Cards.AltarOfMidwinter
import LordOfTheRings.Location.Cards.SinisterDungeon
import LordOfTheRings.Location.Cards.TombsOfCarnDum
import LordOfTheRings.Location.Cards.DarkHalls
import LordOfTheRings.Location.Cards.UnholyCrypt
import LordOfTheRings.Location.Cards.TheGreatHall
import LordOfTheRings.Location.Cards.ForbiddenDescent
import LordOfTheRings.Location.Cards.EerieHalls
import LordOfTheRings.Location.Cards.GhostlyRuins
import LordOfTheRings.Location.Cards.DecrepitRemains
import LordOfTheRings.Location.Cards.StarlitSea
import LordOfTheRings.Location.Cards.FogBank
import LordOfTheRings.Location.Cards.CalmWaters
import LordOfTheRings.Location.Cards.RollingSeas
import LordOfTheRings.Location.Cards.DreamChaserLocation
import LordOfTheRings.Location.Cards.MithlondHarbor
import LordOfTheRings.Location.Cards.BurningPiers
import LordOfTheRings.Location.Cards.PillagedShip
import LordOfTheRings.Location.Cards.WhiteShip
import LordOfTheRings.Location.Cards.ElvenWaveRunner
import LordOfTheRings.Location.Cards.ElvenCaravel
import LordOfTheRings.Location.Cards.RoughWaters
import LordOfTheRings.Location.Cards.Waterspout
import LordOfTheRings.Location.Cards.LostIsland
import LordOfTheRings.Location.Cards.ShrineToMorgoth
import LordOfTheRings.Location.Cards.LushJungle
import LordOfTheRings.Location.Cards.ForbiddenCoast
import LordOfTheRings.Location.Cards.SteepPlateau
import LordOfTheRings.Location.Cards.FloodedRuins
import LordOfTheRings.Location.Cards.DrownedGraves
import LordOfTheRings.Location.Cards.CursedTemple
import LordOfTheRings.Location.Cards.JaggedReef
import LordOfTheRings.Location.Cards.HiddenCove
import LordOfTheRings.Location.Cards.VastCoastland
import LordOfTheRings.Location.Cards.HelmOfTheStormcaller
import LordOfTheRings.Location.Cards.ForecastleDeck
import LordOfTheRings.Location.Cards.WeatherDeck
import LordOfTheRings.Location.Cards.CargoHold
import LordOfTheRings.Location.Cards.CrewQuarters
import LordOfTheRings.Location.Cards.QuarterDeck
import LordOfTheRings.Location.Cards.ShrineToMorgothTheDrownedRuins
import LordOfTheRings.Location.Cards.ShrineToMorgothUnderwater
import LordOfTheRings.Location.Cards.UnderseaGrotto
import LordOfTheRings.Location.Cards.CursedCaverns
import LordOfTheRings.Location.Cards.WaterLoggedHalls
import LordOfTheRings.Location.Cards.DarkAbyss
import LordOfTheRings.Location.Cards.TwistingHollow
import LordOfTheRings.Location.Cards.SunkenTemple
import LordOfTheRings.Location.Cards.DrownedCave
import LordOfTheRings.Location.Cards.TempleOfTheDeceived
import LordOfTheRings.Location.Cards.TheGrottosEntrance
import LordOfTheRings.Location.Cards.TempleHalls
import LordOfTheRings.Location.Cards.JaggedCliffs
import LordOfTheRings.Location.Cards.WindingCaverns
import LordOfTheRings.Location.Cards.JunglePath
import LordOfTheRings.Location.Cards.CoastOfAnfalas
import LordOfTheRings.Location.Cards.BelfalasIslet
import LordOfTheRings.Location.Cards.CobasHaven
import LordOfTheRings.Location.Cards.SouthernBelfalas
import LordOfTheRings.Location.Cards.CorsairWaters
import LordOfTheRings.Location.Cards.WatchTower
import LordOfTheRings.Location.Cards.TheShatteredMonument
import LordOfTheRings.Location.Cards.StreetsOfUmbar
import LordOfTheRings.Location.Cards.CityOfCorsairs
import LordOfTheRings.Location.Cards.UmbarHarbor
import LordOfTheRings.Location.Cards.AbandonedCampNightmare
import LordOfTheRings.Location.Cards.GladeOfTheSpawn
import LordOfTheRings.Location.Cards.AFloodedFord
import LordOfTheRings.Location.Cards.GladdenMarshlands
import LordOfTheRings.Location.Cards.TortureChamber
import LordOfTheRings.Location.Cards.DungeonLabyrinth
import LordOfTheRings.Location.Cards.DocksideStreet
import LordOfTheRings.Location.Cards.PelargirAlley
import LordOfTheRings.Location.Cards.IthilienOverlook
import LordOfTheRings.Location.Cards.DenseThickets
import LordOfTheRings.Location.Cards.CormallenFields
import LordOfTheRings.Location.Cards.BesiegedCourtyard
import LordOfTheRings.Location.Cards.WallsOfCairAndros
import LordOfTheRings.Location.Cards.AnduinWharf
import LordOfTheRings.Location.Cards.WesternShore
import LordOfTheRings.Location.Cards.EasternShore
import LordOfTheRings.Location.Cards.IsenRiver
import LordOfTheRings.Location.Cards.MethedrasPeak
import LordOfTheRings.Location.Cards.OrcTrail
import LordOfTheRings.Location.Cards.TangledForest
import LordOfTheRings.Location.Cards.Entwood
import LordOfTheRings.Location.Cards.DarkCornerOfFangorn
import LordOfTheRings.Location.Cards.OpenWaters
import LordOfTheRings.Location.Cards.ShatteredSea
import LordOfTheRings.Location.Cards.DesecratedGrounds
import LordOfTheRings.Location.Cards.ForsakenCity
import LordOfTheRings.Location.Cards.OvergrownSteps
import LordOfTheRings.Location.Cards.NarelenyaLocation
import LordOfTheRings.Location.Cards.DawnStarLocation
import LordOfTheRings.Location.Cards.SilverWingLocation
import LordOfTheRings.Location.Cards.BurningTower
import LordOfTheRings.Location.Cards.NorthernChetwood
import LordOfTheRings.Location.Cards.OutskirtsOfArchet
import LordOfTheRings.Location.Cards.GreenwayPath
import LordOfTheRings.Location.Cards.WeatherTornValley
import LordOfTheRings.Location.Cards.RuinedWatchTower
import LordOfTheRings.Location.Cards.PalaceRuins
import LordOfTheRings.Location.Cards.FornostStreet
import LordOfTheRings.Location.Cards.DecrepitCity
import LordOfTheRings.Location.Cards.TwistingShadows
import LordOfTheRings.Location.Cards.NarrowFissure
import LordOfTheRings.Location.Cards.ChamberOfRecords
import LordOfTheRings.Location.Cards.DeepMines
import LordOfTheRings.Location.Cards.WesternHall
import LordOfTheRings.Location.Cards.ShadowedCorridor
import LordOfTheRings.Location.Cards.PlainsOfDunland
import LordOfTheRings.Location.Cards.MunuvDuvRiver
import LordOfTheRings.Location.Cards.HithaeglirWoods
import LordOfTheRings.Location.Cards.SacredForest
import LordOfTheRings.Location.Cards.HauntedPillars
import LordOfTheRings.Location.Cards.CityOfSpies
import LordOfTheRings.Location.Cards.DarkPathway
import LordOfTheRings.Location.Cards.DesolateMeadow
import LordOfTheRings.Location.Cards.PeatBog
import LordOfTheRings.Location.Cards.BonesOfHollin
import LordOfTheRings.Location.Cards.DustyLabyrinth
import LordOfTheRings.Location.Cards.RavenStronghold
import LordOfTheRings.Location.Cards.HighlandsOfDunland
import LordOfTheRings.Location.Cards.SharpJetties
import LordOfTheRings.Location.Cards.CliffsideInlet
import LordOfTheRings.Location.Cards.CaptainsQuarters
import LordOfTheRings.Location.Cards.AftCabin
import LordOfTheRings.Location.Cards.EdgeOfTheTemple
import LordOfTheRings.Location.Cards.DesecratedRuinsNightmare
import LordOfTheRings.Location.Cards.TempleFoothills
import LordOfTheRings.Location.Cards.CursedTempleNightmare
import LordOfTheRings.Location.Cards.OvergrownPassage
import LordOfTheRings.Location.Cards.SharpPrecipice
import LordOfTheRings.Location.Cards.JaggedCavern
import LordOfTheRings.Location.Cards.SubmergedCrawlway
import LordOfTheRings.Location.Cards.FloodedCaverns
import LordOfTheRings.Location.Cards.CityUnderSiege
import LordOfTheRings.Location.Cards.SeaWall
import LordOfTheRings.Location.Cards.WarTornWaters
import LordOfTheRings.Location.Cards.WallsOfUmbar
import LordOfTheRings.Location.Cards.UmbarBridge
import LordOfTheRings.Location.Cards.WatchedPath
import LordOfTheRings.Location.Cards.HuntersLookout
import LordOfTheRings.Location.Cards.DarkPools
import LordOfTheRings.Location.Cards.TrollLair
import LordOfTheRings.Location.Cards.HivesAndHives
import LordOfTheRings.Location.Cards.DarkWoodGrove
import LordOfTheRings.Location.Cards.LabyrinthineWoods
import LordOfTheRings.Location.Cards.PitchDarkThicket
import LordOfTheRings.Location.Cards.BramblesAndThorns
import LordOfTheRings.Location.Cards.SarnGebirRapids
import LordOfTheRings.Location.Cards.TheMereOfDeadFaces
import LordOfTheRings.Location.Cards.StagnantQuagmire
import LordOfTheRings.Location.Cards.DeceptivePath
import LordOfTheRings.Location.Cards.WebWall
import LordOfTheRings.Location.Cards.BanksOfMorgulduin
import LordOfTheRings.Location.Cards.EphelDuath
import LordOfTheRings.Location.Cards.WhiteFloweredFields
import LordOfTheRings.Location.Cards.EdgeOfIthilien
import LordOfTheRings.Location.Cards.MountainsOfShadow
import LordOfTheRings.Location.Cards.DevilsMirk
import LordOfTheRings.Location.Cards.Eilenach
import LordOfTheRings.Location.Cards.StonewainValley
import LordOfTheRings.Location.Cards.BesiegedHomestead
import LordOfTheRings.Location.Cards.OverrunVillage
import LordOfTheRings.Location.Cards.PalaceRemains
import LordOfTheRings.Location.Cards.CrumbledBattlements
import LordOfTheRings.Location.Cards.EasternQuarter
import LordOfTheRings.Location.Cards.WesternQuarter
import LordOfTheRings.Location.Cards.TraitorsDen
import LordOfTheRings.Location.Cards.RathCelerdain
import LordOfTheRings.Location.Cards.ArchedTunnel
import LordOfTheRings.Location.Cards.IcyCavern
import LordOfTheRings.Location.Cards.FrozenSilverlode
import LordOfTheRings.Location.Cards.TreacherousBluffs
import LordOfTheRings.Location.Cards.ConcealedWoods
import LordOfTheRings.Location.Cards.BanksOfSirannon
import LordOfTheRings.Location.Cards.HideousDepths
import LordOfTheRings.Location.Cards.EasternMines
import LordOfTheRings.Location.Cards.BottomlessChasm
import LordOfTheRings.Location.Cards.NestOfHorrors
import LordOfTheRings.Location.Cards.ShiveringRiver
import LordOfTheRings.Location.Cards.FieryDepths
import LordOfTheRings.Location.Cards.CrumblingStairs
import LordOfTheRings.Location.Cards.ShroudedWilderness
import LordOfTheRings.Location.Cards.SnowyBadlands
import LordOfTheRings.Location.Cards.PrisonCellNightmare
import LordOfTheRings.Location.Cards.TortureChamberMountGram
import LordOfTheRings.Location.Cards.RockyFoothills
import LordOfTheRings.Location.Cards.VastTrollFells
import LordOfTheRings.Location.Cards.ForgottenCourtyard
import LordOfTheRings.Location.Cards.CharredCellar
import LordOfTheRings.Location.Cards.SecretAntechamber
import LordOfTheRings.Location.Cards.StrongholdTower
import LordOfTheRings.Location.Cards.GatesOfCarnDum
import LordOfTheRings.Location.Cards.InnerSanctum
import LordOfTheRings.Location.Cards.CursedHalls
import LordOfTheRings.Location.Cards.ToweringDunes
import LordOfTheRings.Location.Cards.DesertOasis
import LordOfTheRings.Location.Cards.JungleTrail
import LordOfTheRings.Location.Cards.ForestClearing
import LordOfTheRings.Location.Cards.DenseJungle
import LordOfTheRings.Location.Cards.SouthronBoma
import LordOfTheRings.Location.Cards.VillageHut
import LordOfTheRings.Location.Cards.SpiderBurrow
import LordOfTheRings.Location.Cards.PitchBlackTunnel
import LordOfTheRings.Location.Cards.NestOfVermin
import LordOfTheRings.Location.Cards.CobwebbedCavern
import LordOfTheRings.Location.Cards.BlockedPassage
import LordOfTheRings.Location.Cards.BurntSands
import LordOfTheRings.Location.Cards.ScorchedHamada
import LordOfTheRings.Location.Cards.ParchedWadi
import LordOfTheRings.Location.Cards.NarrowAlleyway
import LordOfTheRings.Location.Cards.UmbarStreet
import LordOfTheRings.Location.Cards.UmbarSquare
import LordOfTheRings.Location.Cards.PortOfHarad
import LordOfTheRings.Location.Cards.HaradrimEstate
import LordOfTheRings.Location.Cards.RiverHarnen
import LordOfTheRings.Location.Cards.ExpanseOfHarad
import LordOfTheRings.Location.Cards.BlisteringErg
import LordOfTheRings.Location.Cards.HaradRoad
import LordOfTheRings.Location.Cards.TheSerpentsRedoubt
import LordOfTheRings.Location.Cards.RooflessTower
import LordOfTheRings.Location.Cards.DesertBastion
import LordOfTheRings.Location.Cards.SouthronCamp
import LordOfTheRings.Location.Cards.BrokenCourtyard
import LordOfTheRings.Location.Cards.GateOfIron
import LordOfTheRings.Location.Cards.DungeonCell
import LordOfTheRings.Location.Cards.UrukGuardroom
import LordOfTheRings.Location.Cards.MinesOfCirithGurat
import LordOfTheRings.Location.Cards.TortureRoom
import LordOfTheRings.Location.Cards.MumakTrail
import LordOfTheRings.Location.Cards.DesolateLand
import LordOfTheRings.Location.Cards.HarondorWaste
import LordOfTheRings.Location.Cards.FoothillsOfMordor
import LordOfTheRings.Location.Cards.ShadowyVale
import LordOfTheRings.Location.Cards.HillsOfWilderland
import LordOfTheRings.Location.Cards.LonelyLands
import LordOfTheRings.Location.Runner ()
import LordOfTheRings.Location.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored location is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Location where
  parseJSON v = do
    attrs <- parseJSON @LocationAttrs v
    pure $ overAttrs (const attrs) $ lookupLocation (locationCardCode attrs) (locationCardId attrs) (locationId attrs)

instance RunMessage Location where
  runMessage msg (Location a) = Location <$> runMessage msg a

createLocation :: EncounterCard -> LocationId -> Location
createLocation card = lookupLocation (toCardCode card) (ecId card)

lookupLocation :: (HasCallStack) => CardCode -> CardId -> LocationId -> Location
lookupLocation cardCode cardId ident = case Map.lookup cardCode allLocations of
  Just (SomeLocationCard builder) -> Location $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown location: " <> show cardCode

-- | A location card entering play: an Uncharted one enters Lost Island side faceup.
newLocation :: (MonadRandom m) => EncounterCard -> m Location
newLocation card = enterFaceUp . createLocation card <$> getRandom

-- | The location with the side it enters play with faceup: an Uncharted one's Lost Island
-- side.
enterFaceUp :: Location -> Location
enterFaceUp l = fromMaybe l do
  other <- cdOtherSide (toCardDef l)
  guard (isUncharted other)
  turnLocationOver l

-- | The location turned over to its other side, if it has one.
turnLocationOver :: Location -> Maybe Location
turnLocationOver = flipLocation lookupLocation . toAttrs

allLocations :: Map CardCode SomeLocationCard
allLocations =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeLocationCard builder) <-
        [ SomeLocationCard greatForestWeb
        , SomeLocationCard dreadfulGap
        , SomeLocationCard fouledWell
        , SomeLocationCard goblinTunnels
        , SomeLocationCard kneesOfTheMountain
        , SomeLocationCard turbulentWaters
        , SomeLocationCard wargLair
        , SomeLocationCard eastGate
        , SomeLocationCard firstHall
        , SomeLocationCard bridgeOfKhazadDum
        , SomeLocationCard stairsOfNain
        , SomeLocationCard branchingPaths
        , SomeLocationCard lightlessPassage
        , SomeLocationCard zigilMineshaft
        , SomeLocationCard mountainsOfMirkwood
        , SomeLocationCard theBrownLands
        , SomeLocationCard theEastBight
        , SomeLocationCard necromancersPass
        , SomeLocationCard enchantedStream
        , SomeLocationCard oldForestRoad
        , SomeLocationCard forestGate
        , SomeLocationCard endlessCaverns
        , SomeLocationCard towerGate
        , SomeLocationCard banksOfTheAnduin
        , SomeLocationCard gladdenFields
        , SomeLocationCard theOldFord
        , SomeLocationCard theEavesOfMirkwood
        , SomeLocationCard riverNinglor
        , SomeLocationCard theEastBank
        , SomeLocationCard theWestBank
        , SomeLocationCard theCarrock
        , SomeLocationCard riverLangflood
        , SomeLocationCard beePastures
        , SomeLocationCard oakWoodGrove
        , SomeLocationCard rhosgobel
        , SomeLocationCard forestGrove
        , SomeLocationCard amonHen
        , SomeLocationCard amonLhaw
        , SomeLocationCard theEastWallOfRohan
        , SomeLocationCard theNorthStair
        , SomeLocationCard raurosFalls
        , SomeLocationCard theShoresOfNenHithoel
        , SomeLocationCard theOuterRidge
        , SomeLocationCard theHighlands
        , SomeLocationCard impassableBog
        , SomeLocationCard theHeartOfTheMarshes
        , SomeLocationCard fensAndMires
        , SomeLocationCard theSpidersRing
        , SomeLocationCard dryWatercourse
        , SomeLocationCard woodmansGlade
        , SomeLocationCard woodElfPath
        , SomeLocationCard theMountainsRoots
        , SomeLocationCard plunderedArmoury
        , SomeLocationCard upperHall
        , SomeLocationCard abandonedMine
        , SomeLocationCard dwarvenForge
        , SomeLocationCard silentCaverns
        , SomeLocationCard twistingPassage
        , SomeLocationCard ruinedRoad
        , SomeLocationCard goblinGate
        , SomeLocationCard pathlessCountry
        , SomeLocationCard barrenHills
        , SomeLocationCard caradhras
        , SomeLocationCard fanuidhol
        , SomeLocationCard celebdil
        , SomeLocationCard theDimrillStair
        , SomeLocationCard rockyCrags
        , SomeLocationCard drownedTreasury
        , SomeLocationCard mithrilLode
        , SomeLocationCard darkPit
        , SomeLocationCard secondHall
        , SomeLocationCard secondDeep
        , SomeLocationCard doorsOfDurin
        , SomeLocationCard stairFalls
        , SomeLocationCard perilousSwamp
        , SomeLocationCard makeshiftPassage
        , SomeLocationCard stagnantCreek
        , SomeLocationCard theCitadel
        , SomeLocationCard theApproach
        , SomeLocationCard theBanks
        , SomeLocationCard orcWarCamp
        , SomeLocationCard ithilienRoad
        , SomeLocationCard overgrownTrail
        , SomeLocationCard secludedGlade
        , SomeLocationCard theLeapingFish
        , SomeLocationCard pelargirDocks
        , SomeLocationCard harborStorehouse
        , SomeLocationCard cityStreet
        , SomeLocationCard marketSquare
        , SomeLocationCard burningFarmhouse
        , SomeLocationCard gondorianHamlet
        , SomeLocationCard secludedFarmhouse
        , SomeLocationCard ancestralClearing
        , SomeLocationCard gardenOfPoisons
        , SomeLocationCard gladeOfCleansing
        , SomeLocationCard morgulRoad
        , SomeLocationCard morgulVale
        , SomeLocationCard theWhiteBridge
        , SomeLocationCard sewers
        , SomeLocationCard rootsOfMindolluin
        , SomeLocationCard storehouse
        , SomeLocationCard housesOfTheDead
        , SomeLocationCard theFourthStar
        , SomeLocationCard westGate
        , SomeLocationCard theKingsLibrary
        , SomeLocationCard ancientHarbor
        , SomeLocationCard theOldBridge
        , SomeLocationCard westQuarter
        , SomeLocationCard eastQuarter
        , SomeLocationCard ruinedSquare
        , SomeLocationCard ruinedTower
        , SomeLocationCard theCrossroads
        , SomeLocationCard southernRoad
        , SomeLocationCard westernRoad
        , SomeLocationCard northernRoad
        , SomeLocationCard easternRoad
        , SomeLocationCard theDarkWoods
        , SomeLocationCard methedras
        , SomeLocationCard orcCave
        , SomeLocationCard mugashsLair
        , SomeLocationCard edgeOfFangorn
        , SomeLocationCard heartOfFangorn
        , SomeLocationCard theIslet
        , SomeLocationCard fordsOfIsen
        , SomeLocationCard theKingsRoad
        , SomeLocationCard gapOfRohan
        , SomeLocationCard ancientForestLocation
        , SomeLocationCard tangledWoods
        , SomeLocationCard brokenLandsLocation
        , SomeLocationCard ravenChiefsCamp
        , SomeLocationCard ravenWarCamp
        , SomeLocationCard ravenVillage
        , SomeLocationCard dunlandBattlefield
        , SomeLocationCard ravenCountry
        , SomeLocationCard munuvDuvRavine
        , SomeLocationCard oldSouthRoad
        , SomeLocationCard hithaeglirFoothills
        , SomeLocationCard hillsOfDunland
        , SomeLocationCard plainsOfEnedwaith
        , SomeLocationCard hithaeglirStream
        , SomeLocationCard theEmptyMug
        , SomeLocationCard theCrossingAtTharbad
        , SomeLocationCard streetsOfTharbad
        , SomeLocationCard tharbadHideout
        , SomeLocationCard ruinsOfTheSecondAge
        , SomeLocationCard seedyInn
        , SomeLocationCard hiddenAlleyway
        , SomeLocationCard decrepitRooftops
        , SomeLocationCard theSecretChamber
        , SomeLocationCard ruinedPlaza
        , SomeLocationCard collapsedTower
        , SomeLocationCard ancientFoundation
        , SomeLocationCard cityRemains
        , SomeLocationCard fenOfReeds
        , SomeLocationCard fingerOfGlanduin
        , SomeLocationCard hiddenEyot
        , SomeLocationCard sinkingBog
        , SomeLocationCard hillBarrow
        , SomeLocationCard caveBarrow
        , SomeLocationCard stoneBarrow
        , SomeLocationCard hallowedCircle
        , SomeLocationCard cursedForest
        , SomeLocationCard grimFoothills
        , SomeLocationCard accursedBattlefield
        , SomeLocationCard mountainsOfAngmar
        , SomeLocationCard blightOfCarnDum
        , SomeLocationCard fortressWalls
        , SomeLocationCard eriadorWastes
        , SomeLocationCard wargsDen
        , SomeLocationCard northDowns
        , SomeLocationCard brokenBattlements
        , SomeLocationCard norburyTombs
        , SomeLocationCard fornostSquare
        , SomeLocationCard hauntedKeep
        , SomeLocationCard deadmensGate
        , SomeLocationCard ruinsOfArnor
        , SomeLocationCard ancientCauseway
        , SomeLocationCard southernGate
        , SomeLocationCard prisonCell
        , SomeLocationCard patrolRoom
        , SomeLocationCard tunnelsOfMountGram
        , SomeLocationCard chetwoodForest
        , SomeLocationCard bordersOfBreeLand
        , SomeLocationCard outlyingHomestead
        , SomeLocationCard ruggedCountry
        , SomeLocationCard shroudedHills
        , SomeLocationCard amonForn
        , SomeLocationCard weatheredHilltop
        , SomeLocationCard exposedRidge
        , SomeLocationCard shelteredValley
        , SomeLocationCard concealedOrcCamp
        , SomeLocationCard secludedCave
        , SomeLocationCard theHoarwell
        , SomeLocationCard abandonedCamp
        , SomeLocationCard patchOfWoods
        , SomeLocationCard trollFells
        , SomeLocationCard barrenMoorland
        , SomeLocationCard altarOfMidwinter
        , SomeLocationCard sinisterDungeon
        , SomeLocationCard tombsOfCarnDum
        , SomeLocationCard darkHalls
        , SomeLocationCard unholyCrypt
        , SomeLocationCard theGreatHall
        , SomeLocationCard forbiddenDescent
        , SomeLocationCard eerieHalls
        , SomeLocationCard ghostlyRuins
        , SomeLocationCard decrepitRemains
        , SomeLocationCard starlitSea
        , SomeLocationCard fogBank
        , SomeLocationCard calmWaters
        , SomeLocationCard rollingSeas
        , SomeLocationCard dreamChaserLocation
        , SomeLocationCard mithlondHarbor
        , SomeLocationCard burningPiers
        , SomeLocationCard pillagedShip
        , SomeLocationCard whiteShip
        , SomeLocationCard elvenWaveRunner
        , SomeLocationCard elvenCaravel
        , SomeLocationCard roughWaters
        , SomeLocationCard waterspout
        , SomeLocationCard lostIsland
        , SomeLocationCard shrineToMorgoth
        , SomeLocationCard lushJungle
        , SomeLocationCard forbiddenCoast
        , SomeLocationCard steepPlateau
        , SomeLocationCard floodedRuins
        , SomeLocationCard drownedGraves
        , SomeLocationCard cursedTemple
        , SomeLocationCard jaggedReef
        , SomeLocationCard hiddenCove
        , SomeLocationCard vastCoastland
        , SomeLocationCard helmOfTheStormcaller
        , SomeLocationCard forecastleDeck
        , SomeLocationCard weatherDeck
        , SomeLocationCard cargoHold
        , SomeLocationCard crewQuarters
        , SomeLocationCard quarterDeck
        , SomeLocationCard shrineToMorgothTheDrownedRuins
        , SomeLocationCard shrineToMorgothUnderwater
        , SomeLocationCard underseaGrotto
        , SomeLocationCard cursedCaverns
        , SomeLocationCard waterLoggedHalls
        , SomeLocationCard darkAbyss
        , SomeLocationCard twistingHollow
        , SomeLocationCard sunkenTemple
        , SomeLocationCard drownedCave
        , SomeLocationCard lostIslandTempleOfTheDeceived
        , SomeLocationCard templeOfTheDeceived
        , SomeLocationCard theGrottosEntrance
        , SomeLocationCard templeHalls
        , SomeLocationCard jaggedCliffs
        , SomeLocationCard windingCaverns
        , SomeLocationCard junglePath
        , SomeLocationCard coastOfAnfalas
        , SomeLocationCard belfalasIslet
        , SomeLocationCard cobasHaven
        , SomeLocationCard southernBelfalas
        , SomeLocationCard corsairWaters
        , SomeLocationCard watchTower
        , SomeLocationCard theShatteredMonument
        , SomeLocationCard streetsOfUmbar
        , SomeLocationCard cityOfCorsairs
        , SomeLocationCard umbarHarbor
        , SomeLocationCard abandonedCampNightmare
        , SomeLocationCard gladeOfTheSpawn
        , SomeLocationCard aFloodedFord
        , SomeLocationCard gladdenMarshlands
        , SomeLocationCard tortureChamber
        , SomeLocationCard dungeonLabyrinth
        , SomeLocationCard docksideStreet
        , SomeLocationCard pelargirAlley
        , SomeLocationCard ithilienOverlook
        , SomeLocationCard denseThickets
        , SomeLocationCard cormallenFields
        , SomeLocationCard besiegedCourtyard
        , SomeLocationCard wallsOfCairAndros
        , SomeLocationCard anduinWharf
        , SomeLocationCard westernShore
        , SomeLocationCard easternShore
        , SomeLocationCard isenRiver
        , SomeLocationCard methedrasPeak
        , SomeLocationCard orcTrail
        , SomeLocationCard tangledForest
        , SomeLocationCard entwood
        , SomeLocationCard darkCornerOfFangorn
        , SomeLocationCard openWaters
        , SomeLocationCard shatteredSea
        , SomeLocationCard desecratedGrounds
        , SomeLocationCard forsakenCity
        , SomeLocationCard overgrownSteps
        , SomeLocationCard narelenyaLocation
        , SomeLocationCard dawnStarLocation
        , SomeLocationCard silverWingLocation
        , SomeLocationCard burningTower
        , SomeLocationCard shrineToMorgothNightmare
        , SomeLocationCard cursedTempleNightmare
        , SomeLocationCard drownedGravesNightmare
        , SomeLocationCard floodedRuinsNightmare
        , SomeLocationCard lushJungleNightmare
        , SomeLocationCard northernChetwood
        , SomeLocationCard outskirtsOfArchet
        , SomeLocationCard greenwayPath
        , SomeLocationCard weatherTornValley
        , SomeLocationCard ruinedWatchTower
        , SomeLocationCard palaceRuins
        , SomeLocationCard fornostStreet
        , SomeLocationCard decrepitCity
        , SomeLocationCard twistingShadows
        , SomeLocationCard narrowFissure
        , SomeLocationCard chamberOfRecords
        , SomeLocationCard deepMines
        , SomeLocationCard westernHall
        , SomeLocationCard shadowedCorridor
        , SomeLocationCard plainsOfDunland
        , SomeLocationCard munuvDuvRiver
        , SomeLocationCard hithaeglirWoods
        , SomeLocationCard sacredForest
        , SomeLocationCard hauntedPillars
        , SomeLocationCard cityOfSpies
        , SomeLocationCard darkPathway
        , SomeLocationCard desolateMeadow
        , SomeLocationCard peatBog
        , SomeLocationCard bonesOfHollin
        , SomeLocationCard dustyLabyrinth
        , SomeLocationCard ravenStronghold
        , SomeLocationCard highlandsOfDunland
        , SomeLocationCard sharpJetties
        , SomeLocationCard cliffsideInlet
        , SomeLocationCard captainsQuarters
        , SomeLocationCard aftCabin
        , SomeLocationCard edgeOfTheTemple
        , SomeLocationCard desecratedRuinsNightmare
        , SomeLocationCard templeFoothills
        , SomeLocationCard cursedTempleOfTheDeceived
        , SomeLocationCard overgrownPassage
        , SomeLocationCard sharpPrecipice
        , SomeLocationCard jaggedCavern
        , SomeLocationCard submergedCrawlway
        , SomeLocationCard floodedCaverns
        , SomeLocationCard cityUnderSiege
        , SomeLocationCard seaWall
        , SomeLocationCard warTornWaters
        , SomeLocationCard wallsOfUmbar
        , SomeLocationCard umbarBridge
        , SomeLocationCard watchedPath
        , SomeLocationCard huntersLookout
        , SomeLocationCard darkPools
        , SomeLocationCard trollLair
        , SomeLocationCard hivesAndHives
        , SomeLocationCard darkWoodGrove
        , SomeLocationCard labyrinthineWoods
        , SomeLocationCard pitchDarkThicket
        , SomeLocationCard bramblesAndThorns
        , SomeLocationCard sarnGebirRapids
        , SomeLocationCard theMereOfDeadFaces
        , SomeLocationCard stagnantQuagmire
        , SomeLocationCard deceptivePath
        , SomeLocationCard webWall
        , SomeLocationCard banksOfMorgulduin
        , SomeLocationCard ephelDuath
        , SomeLocationCard whiteFloweredFields
        , SomeLocationCard edgeOfIthilien
        , SomeLocationCard mountainsOfShadow
        , SomeLocationCard devilsMirk
        , SomeLocationCard eilenach
        , SomeLocationCard stonewainValley
        , SomeLocationCard besiegedHomestead
        , SomeLocationCard overrunVillage
        , SomeLocationCard palaceRemains
        , SomeLocationCard crumbledBattlements
        , SomeLocationCard easternQuarter
        , SomeLocationCard westernQuarter
        , SomeLocationCard traitorsDen
        , SomeLocationCard rathCelerdain
        , SomeLocationCard archedTunnel
        , SomeLocationCard icyCavern
        , SomeLocationCard frozenSilverlode
        , SomeLocationCard treacherousBluffs
        , SomeLocationCard concealedWoods
        , SomeLocationCard banksOfSirannon
        , SomeLocationCard hideousDepths
        , SomeLocationCard easternMines
        , SomeLocationCard bottomlessChasm
        , SomeLocationCard nestOfHorrors
        , SomeLocationCard shiveringRiver
        , SomeLocationCard fieryDepths
        , SomeLocationCard crumblingStairs
        , SomeLocationCard shroudedWilderness
        , SomeLocationCard snowyBadlands
        , SomeLocationCard prisonCellNightmare
        , SomeLocationCard tortureChamberMountGram
        , SomeLocationCard rockyFoothills
        , SomeLocationCard vastTrollFells
        , SomeLocationCard forgottenCourtyard
        , SomeLocationCard charredCellar
        , SomeLocationCard secretAntechamber
        , SomeLocationCard strongholdTower
        , SomeLocationCard gatesOfCarnDum
        , SomeLocationCard innerSanctum
        , SomeLocationCard cursedHalls
        , SomeLocationCard toweringDunes
        , SomeLocationCard desertOasis
        , SomeLocationCard jungleTrail
        , SomeLocationCard forestClearing
        , SomeLocationCard denseJungle
        , SomeLocationCard southronBoma
        , SomeLocationCard villageHut
        , SomeLocationCard spiderBurrow
        , SomeLocationCard pitchBlackTunnel
        , SomeLocationCard nestOfVermin
        , SomeLocationCard cobwebbedCavern
        , SomeLocationCard blockedPassage
        , SomeLocationCard burntSands
        , SomeLocationCard scorchedHamada
        , SomeLocationCard parchedWadi
        , SomeLocationCard narrowAlleyway
        , SomeLocationCard umbarStreet
        , SomeLocationCard umbarSquare
        , SomeLocationCard portOfHarad
        , SomeLocationCard haradrimEstate
        , SomeLocationCard riverHarnen
        , SomeLocationCard expanseOfHarad
        , SomeLocationCard blisteringErg
        , SomeLocationCard haradRoad
        , SomeLocationCard theSerpentsRedoubt
        , SomeLocationCard rooflessTower
        , SomeLocationCard desertBastion
        , SomeLocationCard southronCamp
        , SomeLocationCard brokenCourtyard
        , SomeLocationCard gateOfIron
        , SomeLocationCard dungeonCell
        , SomeLocationCard urukGuardroom
        , SomeLocationCard minesOfCirithGurat
        , SomeLocationCard tortureRoom
        , SomeLocationCard mumakTrail
        , SomeLocationCard desolateLand
        , SomeLocationCard harondorWaste
        , SomeLocationCard foothillsOfMordor
        , SomeLocationCard shadowyVale
        , SomeLocationCard hillsOfWilderland
        , SomeLocationCard lonelyLands
        ]
    ]
