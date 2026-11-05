{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Ally (module LordOfTheRings.Ally, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Ally.Cards.Beorn
import LordOfTheRings.Ally.Cards.BrokIronfist
import LordOfTheRings.Ally.Cards.DaughterOfTheNimrodel
import LordOfTheRings.Ally.Cards.EreborHammersmith
import LordOfTheRings.Ally.Cards.Faramir
import LordOfTheRings.Ally.Cards.Gandalf
import LordOfTheRings.Ally.Cards.Gleowine
import LordOfTheRings.Ally.Cards.GondorianSpearman
import LordOfTheRings.Ally.Cards.GuardOfTheCitadel
import LordOfTheRings.Ally.Cards.HenamarthRiversong
import LordOfTheRings.Ally.Cards.HorsebackArcher
import LordOfTheRings.Ally.Cards.LongbeardOrcSlayer
import LordOfTheRings.Ally.Cards.LorienGuide
import LordOfTheRings.Ally.Cards.MinerOfTheIronHills
import LordOfTheRings.Ally.Cards.NorthernTracker
import LordOfTheRings.Ally.Cards.SilverlodeArcher
import LordOfTheRings.Ally.Cards.SnowbournScout
import LordOfTheRings.Ally.Cards.SonOfArnor
import LordOfTheRings.Ally.Cards.VeteranAxehand
import LordOfTheRings.Ally.Cards.WanderingTook
import LordOfTheRings.Ally.Cards.WingedGuardian
import LordOfTheRings.Ally.Cards.RivendellMinstrel
import LordOfTheRings.Ally.Cards.BeorningBeekeeper
import LordOfTheRings.Ally.Cards.LongbeardMapMaker
import LordOfTheRings.Ally.Cards.GrimbeornTheOld
import LordOfTheRings.Ally.Cards.MistyMountainEagle
import LordOfTheRings.Ally.Cards.Landroval
import LordOfTheRings.Ally.Cards.HaldirOfLorien
import LordOfTheRings.Ally.Cards.Radagast
import LordOfTheRings.Ally.Cards.Wilyador
import LordOfTheRings.Ally.Cards.Mendor
import LordOfTheRings.Ally.Cards.Berelind
import LordOfTheRings.Ally.Cards.Coruhaer
import LordOfTheRings.Ally.Cards.Laermuin
import LordOfTheRings.Ally.Cards.ElrondsChampion
import LordOfTheRings.Ally.Cards.RivendellSkirmishers
import LordOfTheRings.Ally.Cards.ArwenUndomielObjective
import LordOfTheRings.Ally.Cards.KeenEyedTook
import LordOfTheRings.Ally.Cards.DescendantOfThorondor
import LordOfTheRings.Ally.Cards.GildorInglorion
import LordOfTheRings.Ally.Cards.DunedainWatcher
import LordOfTheRings.Ally.Cards.VassalOfTheWindlord
import LordOfTheRings.Ally.Cards.Elfhelm
import LordOfTheRings.Ally.Cards.EaglesOfTheMistyMountains
import LordOfTheRings.Ally.Cards.WestfoldHorseBreaker
import LordOfTheRings.Ally.Cards.Eomund
import LordOfTheRings.Ally.Cards.EscortFromEdoras
import LordOfTheRings.Ally.Cards.TheRiddermarksFinest
import LordOfTheRings.Ally.Cards.WestRoadTraveller
import LordOfTheRings.Ally.Cards.Guthlaf
import LordOfTheRings.Ally.Cards.WestfoldOutrider
import LordOfTheRings.Ally.Cards.WestfoldHorseBreeder
import LordOfTheRings.Ally.Cards.RiddermarkKnight
import LordOfTheRings.Ally.Cards.WestfoldLancer
import LordOfTheRings.Ally.Cards.SilvanTracker
import LordOfTheRings.Ally.Cards.MirkwoodRunner
import LordOfTheRings.Ally.Cards.SilvanRefugee
import LordOfTheRings.Ally.Cards.NaithGuide
import LordOfTheRings.Ally.Cards.Rumil
import LordOfTheRings.Ally.Cards.GaladhrimMinstrel
import LordOfTheRings.Ally.Cards.DefenderOfTheNaith
import LordOfTheRings.Ally.Cards.GaladhonArcher
import LordOfTheRings.Ally.Cards.Orophin
import LordOfTheRings.Ally.Cards.GaladrielsHandmaiden
import LordOfTheRings.Ally.Cards.GreenwoodArcher
import LordOfTheRings.Ally.Cards.ZigilMiner
import LordOfTheRings.Ally.Cards.EreborRecordKeeper
import LordOfTheRings.Ally.Cards.Bombur
import LordOfTheRings.Ally.Cards.LongbeardElder
import LordOfTheRings.Ally.Cards.EredNimraisProspector
import LordOfTheRings.Ally.Cards.EredLuinMiner
import LordOfTheRings.Ally.Cards.Fili
import LordOfTheRings.Ally.Cards.Kili
import LordOfTheRings.Ally.Cards.Dori
import LordOfTheRings.Ally.Cards.Gloin
import LordOfTheRings.Ally.Cards.EreborGuard
import LordOfTheRings.Ally.Cards.ErrandRider
import LordOfTheRings.Ally.Cards.PelargirShipCaptain
import LordOfTheRings.Ally.Cards.CitadelCustodian
import LordOfTheRings.Ally.Cards.DefenderOfRammas
import LordOfTheRings.Ally.Cards.EnvoyOfPelargir
import LordOfTheRings.Ally.Cards.Damrod
import LordOfTheRings.Ally.Cards.HunterOfLamedon
import LordOfTheRings.Ally.Cards.IthilienTracker
import LordOfTheRings.Ally.Cards.MasterOfLore
import LordOfTheRings.Ally.Cards.SquireOfTheCitadel
import LordOfTheRings.Ally.Cards.EmynArnenRanger
import LordOfTheRings.Ally.Cards.SoldierOfGondor
import LordOfTheRings.Ally.Cards.AngborTheFearless
import LordOfTheRings.Ally.Cards.VeteranOfNanduhirion
import LordOfTheRings.Ally.Cards.Bofur
import LordOfTheRings.Ally.Cards.RavenhillScout
import LordOfTheRings.Ally.Cards.DunedainWanderer
import LordOfTheRings.Ally.Cards.RiderOfTheMark
import LordOfTheRings.Ally.Cards.WatcherOfTheBruinen
import LordOfTheRings.Ally.Cards.ArwenUndomiel
import LordOfTheRings.Ally.Cards.Erestor
import LordOfTheRings.Ally.Cards.EreborBattleMaster
import LordOfTheRings.Ally.Cards.WardenOfHealing
import LordOfTheRings.Ally.Cards.TrollshawScout
import LordOfTheRings.Ally.Cards.ImladrisStargazer
import LordOfTheRings.Ally.Cards.MasterOfTheForge
import LordOfTheRings.Ally.Cards.Treebeard
import LordOfTheRings.Ally.Cards.Glorfindel
import LordOfTheRings.Ally.Cards.MarksmanOfLorien
import LordOfTheRings.Ally.Cards.Yazan
import LordOfTheRings.Ally.Cards.Jubayr
import LordOfTheRings.Ally.Cards.Firyal
import LordOfTheRings.Ally.Cards.Celador
import LordOfTheRings.Ally.Cards.IthilienGuardian
import LordOfTheRings.Ally.Cards.LordAlcaron
import LordOfTheRings.Ally.Cards.FaramirObjective
import LordOfTheRings.Ally.Cards.LordAlcaronTheBloodOfGondor
import LordOfTheRings.Ally.Cards.WarriorOfLossarnach
import LordOfTheRings.Ally.Cards.KnightsOfTheSwan
import LordOfTheRings.Ally.Cards.EthirSwordsman
import LordOfTheRings.Ally.Cards.AnfalasHerdsman
import LordOfTheRings.Ally.Cards.Forlong
import LordOfTheRings.Ally.Cards.HarborMaster
import LordOfTheRings.Ally.Cards.WhiteTowerWatchman
import LordOfTheRings.Ally.Cards.Denethor
import LordOfTheRings.Ally.Cards.MinasTirithLampwright
import LordOfTheRings.Ally.Cards.IthilienArcher
import LordOfTheRings.Ally.Cards.KnightOfMinasTirith
import LordOfTheRings.Ally.Cards.PelargirShipwright
import LordOfTheRings.Ally.Cards.Emery
import LordOfTheRings.Ally.Cards.Anborn
import LordOfTheRings.Ally.Cards.OrthancGuard
import LordOfTheRings.Ally.Cards.IsengardMessenger
import LordOfTheRings.Ally.Cards.Saruman
import LordOfTheRings.Ally.Cards.RivendellScout
import LordOfTheRings.Ally.Cards.WardenOfHelmsDeep
import LordOfTheRings.Ally.Cards.WanderingEnt
import LordOfTheRings.Ally.Cards.BoomingEnt
import LordOfTheRings.Ally.Cards.IthilienLookout
import LordOfTheRings.Ally.Cards.CelduinTraveler
import LordOfTheRings.Ally.Cards.GreyfloodWanderer
import LordOfTheRings.Ally.Cards.HeraldOfAnorien
import LordOfTheRings.Ally.Cards.HennethAnnunGuard
import LordOfTheRings.Ally.Cards.MirkwoodPioneer
import LordOfTheRings.Ally.Cards.BlueMountainTrader
import LordOfTheRings.Ally.Cards.Gwaihir
import LordOfTheRings.Ally.Cards.WeatherHillsWatchman
import LordOfTheRings.Ally.Cards.DunedainHunter
import LordOfTheRings.Ally.Cards.SarnFordSentry
import LordOfTheRings.Ally.Cards.WardenOfAnnuminas
import LordOfTheRings.Ally.Cards.RangerOfTheNorth
import LordOfTheRings.Ally.Cards.Ingold
import LordOfTheRings.Ally.Cards.HonourGuard
import LordOfTheRings.Ally.Cards.CuriousBrandybuck
import LordOfTheRings.Ally.Cards.EastRoadRanger
import LordOfTheRings.Ally.Cards.RangerOfCardolan
import LordOfTheRings.Ally.Cards.GrimaTheFordsOfIsen
import LordOfTheRings.Ally.Cards.ChiefTurchTheAntleredCrown
import LordOfTheRings.Ally.Cards.NalirTroubleInTharbad
import LordOfTheRings.Ally.Cards.NalirTheNinInEilph
import LordOfTheRings.Ally.Cards.Amarthiul
import LordOfTheRings.Ally.Cards.Iarion
import LordOfTheRings.Ally.Cards.GuardianOfArnor
import LordOfTheRings.Ally.Cards.Beechbone
import LordOfTheRings.Ally.Cards.Lindir
import LordOfTheRings.Ally.Cards.Eothain
import LordOfTheRings.Ally.Cards.FornostBowman
import LordOfTheRings.Ally.Cards.GaladhrimHealer
import LordOfTheRings.Ally.Cards.VeteranOfOsgiliath
import LordOfTheRings.Ally.Cards.DerndingleWarrior
import LordOfTheRings.Ally.Cards.ElvenJeweler
import LordOfTheRings.Ally.Cards.LongbeardSentry
import LordOfTheRings.Ally.Cards.WellinghallPreserver
import LordOfTheRings.Ally.Cards.GaladhrimWeaver
import LordOfTheRings.Ally.Cards.GaldorOfTheHavens
import LordOfTheRings.Ally.Cards.AmarthiulAcrossTheEttenmoors
import LordOfTheRings.Ally.Cards.DawnStar
import LordOfTheRings.Ally.Cards.DreamChaser
import LordOfTheRings.Ally.Cards.Narelenya
import LordOfTheRings.Ally.Cards.SilverWing
import LordOfTheRings.Ally.Cards.AmarthiulTheTreacheryOfRhudaur
import LordOfTheRings.Ally.Cards.MithlondSeaWatcher
import LordOfTheRings.Ally.Cards.WardenOfTheHavens
import LordOfTheRings.Ally.Cards.LindonNavigator
import LordOfTheRings.Ally.Cards.SailorOfLune
import LordOfTheRings.Ally.Cards.Calphon
import LordOfTheRings.Ally.Cards.CaptainSahir
import LordOfTheRings.Ally.Cards.Haldan
import LordOfTheRings.Ally.Cards.Urdug
import LordOfTheRings.Ally.Cards.Tiny
import LordOfTheRings.Ally.Cards.Naasiyah
import LordOfTheRings.Ally.Cards.AzainSilverbeard
import LordOfTheRings.Ally.Cards.ImladrisCaregiver
import LordOfTheRings.Ally.Cards.GuardianOfRivendell
import LordOfTheRings.Ally.Cards.Eldahir
import LordOfTheRings.Ally.Cards.MasterIronsmith
import LordOfTheRings.Ally.Cards.SamGamgee
import LordOfTheRings.Ally.Cards.MirkwoodExplorer
import LordOfTheRings.Ally.Cards.Ceorl
import LordOfTheRings.Ally.Cards.Deorwine
import LordOfTheRings.Ally.Cards.RhovanionOutrider
import LordOfTheRings.Ally.Cards.DwarvenSellsword
import LordOfTheRings.Ally.Cards.WoodlandCourier
import LordOfTheRings.Ally.Cards.RobinSmallburrow
import LordOfTheRings.Ally.Cards.VeteranSwordElf
import LordOfTheRings.Ally.Cards.LinhirSeaCaptain
import LordOfTheRings.Ally.Cards.Ioreth
import LordOfTheRings.Ally.Cards.KnightOfTheWhiteTower
import LordOfTheRings.Ally.Cards.SoldierOfDolAmroth
import LordOfTheRings.Ally.Cards.Sulien
import LordOfTheRings.Ally.Cards.GuardianOfIthilien
import LordOfTheRings.Ally.Cards.DolAmrothWarship
import LordOfTheRings.Ally.Cards.CorsairSkirmisherShip
import LordOfTheRings.Ally.Cards.Calaerion
import LordOfTheRings.Ally.Cards.Gavin
import LordOfTheRings.Ally.Cards.Starla
import LordOfTheRings.Ally.Cards.CaptiveVillager
import LordOfTheRings.Ally.Cards.HalflingBounder
import LordOfTheRings.Ally.Cards.VigilantDunadan
import LordOfTheRings.Ally.Cards.AndrathGuardsman
import LordOfTheRings.Ally.Cards.KahlielsTribesman
import LordOfTheRings.Ally.Cards.DunedainPathfinder
import LordOfTheRings.Ally.Cards.ErynGalenSettler
import LordOfTheRings.Ally.Cards.StewardOfOrthanc
import LordOfTheRings.Ally.Cards.TamedMumak
import LordOfTheRings.Ally.Cards.Abaan
import LordOfTheRings.Ally.Cards.RiderOfRohan
import LordOfTheRings.Ally.Cards.DunedainLookout
import LordOfTheRings.Ally.Cards.DefenderOfCairAndros
import LordOfTheRings.Ally.Cards.SouthronRefugee
import LordOfTheRings.Ally.Cards.KnightOfDale
import LordOfTheRings.Ally.Cards.EregionSurvivor
import LordOfTheRings.Ally.Cards.Leaflock
import LordOfTheRings.Ally.Cards.HalfastGamgee
import LordOfTheRings.Ally.Cards.BeorningGuardian
import LordOfTheRings.Ally.Cards.RedwaterSentry
import LordOfTheRings.Ally.Cards.NorthRealmLookout
import LordOfTheRings.Ally.Cards.WarriorOfDale
import LordOfTheRings.Ally.Cards.LongLakeTrader
import LordOfTheRings.Ally.Cards.GuardianOfEsgaroth
import LordOfTheRings.Ally.Cards.BeorningSkinChanger
import LordOfTheRings.Ally.Cards.LongLakeFisherman
import LordOfTheRings.Ally.Cards.PorosGarrison
import LordOfTheRings.Ally.Cards.GreenwoodDefender
import LordOfTheRings.Ally.Cards.GafferGamgee
import LordOfTheRings.Ally.Cards.EreborToymaker
import LordOfTheRings.Ally.Cards.MessengerRaven
import LordOfTheRings.Ally.Cards.LoyalHound
import LordOfTheRings.Ally.Cards.BywaterShirriff
import LordOfTheRings.Ally.Cards.Wiglaf
import LordOfTheRings.Ally.Cards.Meneldor
import LordOfTheRings.Ally.Cards.WildStallion
import LordOfTheRings.Ally.Cards.EagleOfTheNorth
import LordOfTheRings.Ally.Cards.ForestRoadTraveler
import LordOfTheRings.Ally.Cards.GiantBear
import LordOfTheRings.Ally.Cards.Galion
import LordOfTheRings.Ally.Cards.Thalion
import LordOfTheRings.Ally.Cards.SoldierOfErebor
import LordOfTheRings.Ally.Cards.MirkwoodHunter
import LordOfTheRings.Ally.Cards.DescendantOfGirion
import LordOfTheRings.Ally.Cards.SoldierOfIsengard
import LordOfTheRings.Ally.Cards.NoriTactics
import LordOfTheRings.Ally.Cards.RohirrimScout
import LordOfTheRings.Ally.Cards.WanderingExile
import LordOfTheRings.Ally.Cards.NorthernBowmaster
import LordOfTheRings.Ally.Cards.OdoProudfoot
import LordOfTheRings.Ally.Cards.MerryLeadership
import LordOfTheRings.Ally.Cards.PippinTactics
import LordOfTheRings.Ally.Cards.FaramirLore
import LordOfTheRings.Ally.Cards.Derufin
import LordOfTheRings.Ally.Cards.CautiousHalfling
import LordOfTheRings.Ally.Cards.TomBombadil
import LordOfTheRings.Ally.Cards.WoodlandSentry
import LordOfTheRings.Ally.Cards.KnightOfBelfalas
import LordOfTheRings.Ally.Cards.HobbitArcher
import LordOfTheRings.Ally.Cards.DaleMessenger
import LordOfTheRings.Ally.Cards.EagleEmissary
import LordOfTheRings.Ally.Cards.WilyadorTheLandOfSorrow
import LordOfTheRings.Ally.Cards.Haleth
import LordOfTheRings.Ally.Cards.MistyMountainJourneyman
import LordOfTheRings.Ally.Cards.Edrahil
import LordOfTheRings.Ally.Cards.Arador
import LordOfTheRings.Ally.Cards.EntOfFangorn
import LordOfTheRings.Ally.Cards.Farin
import LordOfTheRings.Ally.Cards.ArmyOfTheDead (armyOfTheDead)
import LordOfTheRings.Ally.Cards.GwaihirTheBlackGateOpens
import LordOfTheRings.Ally.Cards.AnbornLeadership
import LordOfTheRings.Ally.Cards.MablungLore
import LordOfTheRings.Ally.Cards.Skinbark
import LordOfTheRings.Ally.Cards.Gamling
import LordOfTheRings.Ally.Cards.Grimbold
import LordOfTheRings.Ally.Cards.PrinceImrahilSpirit
import LordOfTheRings.Ally.Cards.HalbaradLeadership
import LordOfTheRings.Ally.Cards.GhanBuriGhan
import LordOfTheRings.Ally.Cards.RosieCotton
import LordOfTheRings.Ally.Cards.ElfhelmTactics
import LordOfTheRings.Ally.Cards.ElladanLore
import LordOfTheRings.Ally.Cards.ElrohirSpirit
import LordOfTheRings.Ally.Cards.GildorInglorionObjective
import LordOfTheRings.Ally.Cards.BillThePony
import LordOfTheRings.Ally.Cards.BarlimanButterbur
import LordOfTheRings.Ally.Cards.FarmerMaggot
import LordOfTheRings.Ally.Cards.GaladrielTheRoadDarkens
import LordOfTheRings.Ally.Cards.BoromirTheRoadDarkens
import LordOfTheRings.Ally.Cards.ElrondTheRoadDarkens
import LordOfTheRings.Ally.Cards.BilboBagginsTheRoadDarkens
import LordOfTheRings.Ally.Cards.GimliTheTreasonOfSaruman
import LordOfTheRings.Ally.Cards.LegolasTheTreasonOfSaruman
import LordOfTheRings.Ally.Cards.QuickbeamTheTreasonOfSaruman
import LordOfTheRings.Ally.Cards.HamaTheTreasonOfSaruman
import LordOfTheRings.Ally.Cards.SmeagolTheLandOfShadow
import LordOfTheRings.Ally.Cards.BofurTactics
import LordOfTheRings.Ally.Cards.GandalfHobbit
import LordOfTheRings.Ally.Cards.Bifur
import LordOfTheRings.Ally.Cards.Dwalin
import LordOfTheRings.Ally.Runner ()
import LordOfTheRings.Ally.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored ally is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Ally where
  parseJSON v = do
    attrs <- parseJSON @AllyAttrs v
    pure $ overAttrs (const attrs) $ lookupAlly (allyCardCode attrs) (allyCardId attrs) (allyId attrs) (allyOwner attrs)

instance RunMessage Ally where
  runMessage msg (Ally a) = Ally <$> runMessage msg a

createAlly :: PlayerCard -> AllyId -> PlayerId -> Ally
createAlly card hid pid = lookupAlly (toCardCode card) (pcId card) hid pid

lookupAlly :: (HasCallStack) => CardCode -> CardId -> AllyId -> PlayerId -> Ally
lookupAlly cardCode cardId hid pid = case Map.lookup cardCode allAllies of
  Just (SomeAllyCard builder) -> Ally $ cbCardBuilder builder cardId (hid, pid)
  Nothing -> error $ "Unknown ally: " <> show cardCode

newAlly :: (MonadRandom m) => PlayerId -> PlayerCard -> m Ally
newAlly pid card = do
  hid <- getRandom
  pure $ createAlly card hid pid

allAllies :: Map CardCode SomeAllyCard
allAllies =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeAllyCard builder) <-
        [ SomeAllyCard guardOfTheCitadel
        , SomeAllyCard faramir
        , SomeAllyCard sonOfArnor
        , SomeAllyCard snowbournScout
        , SomeAllyCard silverlodeArcher
        , SomeAllyCard longbeardOrcSlayer
        , SomeAllyCard brokIronfist
        , SomeAllyCard veteranAxehand
        , SomeAllyCard gondorianSpearman
        , SomeAllyCard horsebackArcher
        , SomeAllyCard beorn
        , SomeAllyCard wanderingTook
        , SomeAllyCard lorienGuide
        , SomeAllyCard northernTracker
        , SomeAllyCard daughterOfTheNimrodel
        , SomeAllyCard ereborHammersmith
        , SomeAllyCard henamarthRiversong
        , SomeAllyCard minerOfTheIronHills
        , SomeAllyCard gleowine
        , SomeAllyCard gandalf
        , SomeAllyCard wingedGuardian
        , SomeAllyCard rivendellMinstrel
        , SomeAllyCard beorningBeekeeper
        , SomeAllyCard longbeardMapMaker
        , SomeAllyCard grimbeornTheOld
        , SomeAllyCard mistyMountainEagle
        , SomeAllyCard landroval
        , SomeAllyCard haldirOfLorien
        , SomeAllyCard radagast
        , SomeAllyCard wilyador
        , SomeAllyCard mendor
        , SomeAllyCard berelind
        , SomeAllyCard coruhaer
        , SomeAllyCard laermuin
        , SomeAllyCard elrondsChampion
        , SomeAllyCard rivendellSkirmishers
        , SomeAllyCard arwenUndomielTheRedhornGate
        , SomeAllyCard arwenUndomielRoadToRivendell
        , SomeAllyCard keenEyedTook
        , SomeAllyCard descendantOfThorondor
        , SomeAllyCard gildorInglorion
        , SomeAllyCard dunedainWatcher
        , SomeAllyCard vassalOfTheWindlord
        , SomeAllyCard elfhelm
        , SomeAllyCard eaglesOfTheMistyMountains
        , SomeAllyCard westfoldHorseBreaker
        , SomeAllyCard eomund
        , SomeAllyCard escortFromEdoras
        , SomeAllyCard theRiddermarksFinest
        , SomeAllyCard westRoadTraveller
        , SomeAllyCard guthlaf
        , SomeAllyCard westfoldOutrider
        , SomeAllyCard westfoldHorseBreeder
        , SomeAllyCard riddermarkKnight
        , SomeAllyCard westfoldLancer
        , SomeAllyCard silvanTracker
        , SomeAllyCard mirkwoodRunner
        , SomeAllyCard silvanRefugee
        , SomeAllyCard naithGuide
        , SomeAllyCard rumil
        , SomeAllyCard galadhrimMinstrel
        , SomeAllyCard defenderOfTheNaith
        , SomeAllyCard galadhonArcher
        , SomeAllyCard orophin
        , SomeAllyCard galadrielsHandmaiden
        , SomeAllyCard greenwoodArcher
        , SomeAllyCard zigilMiner
        , SomeAllyCard ereborRecordKeeper
        , SomeAllyCard bombur
        , SomeAllyCard longbeardElder
        , SomeAllyCard eredNimraisProspector
        , SomeAllyCard eredLuinMiner
        , SomeAllyCard fili
        , SomeAllyCard kili
        , SomeAllyCard dori
        , SomeAllyCard gloin
        , SomeAllyCard ereborGuard
        , SomeAllyCard errandRider
        , SomeAllyCard pelargirShipCaptain
        , SomeAllyCard citadelCustodian
        , SomeAllyCard defenderOfRammas
        , SomeAllyCard envoyOfPelargir
        , SomeAllyCard damrod
        , SomeAllyCard hunterOfLamedon
        , SomeAllyCard ithilienTracker
        , SomeAllyCard masterOfLore
        , SomeAllyCard squireOfTheCitadel
        , SomeAllyCard emynArnenRanger
        , SomeAllyCard soldierOfGondor
        , SomeAllyCard angborTheFearless
        , SomeAllyCard veteranOfNanduhirion
        , SomeAllyCard bofur
        , SomeAllyCard ravenhillScout
        , SomeAllyCard dunedainWanderer
        , SomeAllyCard riderOfTheMark
        , SomeAllyCard watcherOfTheBruinen
        , SomeAllyCard arwenUndomiel
        , SomeAllyCard erestor
        , SomeAllyCard ereborBattleMaster
        , SomeAllyCard wardenOfHealing
        , SomeAllyCard trollshawScout
        , SomeAllyCard imladrisStargazer
        , SomeAllyCard masterOfTheForge
        , SomeAllyCard treebeard
        , SomeAllyCard glorfindel
        , SomeAllyCard marksmanOfLorien
        , SomeAllyCard yazan
        , SomeAllyCard jubayr
        , SomeAllyCard firyal
        , SomeAllyCard celador
        , SomeAllyCard ithilienGuardian
        , SomeAllyCard lordAlcaron
        , SomeAllyCard faramirObjective
        , SomeAllyCard lordAlcaronTheBloodOfGondor
        , SomeAllyCard warriorOfLossarnach
        , SomeAllyCard knightsOfTheSwan
        , SomeAllyCard ethirSwordsman
        , SomeAllyCard anfalasHerdsman
        , SomeAllyCard forlong
        , SomeAllyCard harborMaster
        , SomeAllyCard whiteTowerWatchman
        , SomeAllyCard denethor
        , SomeAllyCard minasTirithLampwright
        , SomeAllyCard ithilienArcher
        , SomeAllyCard knightOfMinasTirith
        , SomeAllyCard pelargirShipwright
        , SomeAllyCard emery
        , SomeAllyCard anborn
        , SomeAllyCard orthancGuard
        , SomeAllyCard isengardMessenger
        , SomeAllyCard saruman
        , SomeAllyCard rivendellScout
        , SomeAllyCard wardenOfHelmsDeep
        , SomeAllyCard wanderingEnt
        , SomeAllyCard boomingEnt
        , SomeAllyCard ithilienLookout
        , SomeAllyCard celduinTraveler
        , SomeAllyCard greyfloodWanderer
        , SomeAllyCard heraldOfAnorien
        , SomeAllyCard hennethAnnunGuard
        , SomeAllyCard mirkwoodPioneer
        , SomeAllyCard blueMountainTrader
        , SomeAllyCard gwaihir
        , SomeAllyCard weatherHillsWatchman
        , SomeAllyCard dunedainHunter
        , SomeAllyCard sarnFordSentry
        , SomeAllyCard wardenOfAnnuminas
        , SomeAllyCard rangerOfTheNorth
        , SomeAllyCard ingold
        , SomeAllyCard honourGuard
        , SomeAllyCard curiousBrandybuck
        , SomeAllyCard eastRoadRanger
        , SomeAllyCard rangerOfCardolan
        , SomeAllyCard grimaTheFordsOfIsen
        , SomeAllyCard chiefTurchTheAntleredCrown
        , SomeAllyCard nalirTroubleInTharbad
        , SomeAllyCard amarthiulTheTreacheryOfRhudaur
        , SomeAllyCard nalirTheNinInEilph
        , SomeAllyCard amarthiul
        , SomeAllyCard iarion
        , SomeAllyCard guardianOfArnor
        , SomeAllyCard beechbone
        , SomeAllyCard lindir
        , SomeAllyCard eothain
        , SomeAllyCard fornostBowman
        , SomeAllyCard galadhrimHealer
        , SomeAllyCard veteranOfOsgiliath
        , SomeAllyCard derndingleWarrior
        , SomeAllyCard elvenJeweler
        , SomeAllyCard longbeardSentry
        , SomeAllyCard wellinghallPreserver
        , SomeAllyCard galadhrimWeaver
        , SomeAllyCard galdorOfTheHavens
        , SomeAllyCard amarthiulAcrossTheEttenmoors
        , SomeAllyCard mithlondSeaWatcher
        , SomeAllyCard wardenOfTheHavens
        , SomeAllyCard lindonNavigator
        , SomeAllyCard sailorOfLune
        , SomeAllyCard dreamChaser
        , SomeAllyCard dawnStar
        , SomeAllyCard narelenya
        , SomeAllyCard silverWing
        , SomeAllyCard calphon
        , SomeAllyCard captainSahir
        , SomeAllyCard haldan
        , SomeAllyCard urdug
        , SomeAllyCard tiny
        , SomeAllyCard naasiyah
        , SomeAllyCard azainSilverbeard
        , SomeAllyCard imladrisCaregiver
        , SomeAllyCard guardianOfRivendell
        , SomeAllyCard eldahir
        , SomeAllyCard masterIronsmith
        , SomeAllyCard samGamgee
        , SomeAllyCard mirkwoodExplorer
        , SomeAllyCard ceorl
        , SomeAllyCard deorwine
        , SomeAllyCard rhovanionOutrider
        , SomeAllyCard dwarvenSellsword
        , SomeAllyCard woodlandCourier
        , SomeAllyCard robinSmallburrow
        , SomeAllyCard veteranSwordElf
        , SomeAllyCard linhirSeaCaptain
        , SomeAllyCard ioreth
        , SomeAllyCard knightOfTheWhiteTower
        , SomeAllyCard soldierOfDolAmroth
        , SomeAllyCard sulien
        , SomeAllyCard guardianOfIthilien
        , SomeAllyCard dolAmrothWarship
        , SomeAllyCard corsairSkirmisherShip
        , SomeAllyCard calaerion
        , SomeAllyCard calaerionUpgraded
        , SomeAllyCard gavin
        , SomeAllyCard starla
        , SomeAllyCard dreamChaserUpgraded
        , SomeAllyCard dawnStarUpgraded
        , SomeAllyCard silverWingUpgraded
        , SomeAllyCard narelenyaUpgraded
        , SomeAllyCard captiveVillager
        , SomeAllyCard halflingBounder
        , SomeAllyCard vigilantDunadan
        , SomeAllyCard andrathGuardsman
        , SomeAllyCard kahlielsTribesman
        , SomeAllyCard dunedainPathfinder
        , SomeAllyCard erynGalenSettler
        , SomeAllyCard stewardOfOrthanc
        , SomeAllyCard tamedMumak
        , SomeAllyCard abaan
        , SomeAllyCard riderOfRohan
        , SomeAllyCard dunedainLookout
        , SomeAllyCard defenderOfCairAndros
        , SomeAllyCard southronRefugee
        , SomeAllyCard knightOfDale
        , SomeAllyCard eregionSurvivor
        , SomeAllyCard leaflock
        , SomeAllyCard halfastGamgee
        , SomeAllyCard beorningGuardian
        , SomeAllyCard redwaterSentry
        , SomeAllyCard northRealmLookout
        , SomeAllyCard warriorOfDale
        , SomeAllyCard longLakeTrader
        , SomeAllyCard guardianOfEsgaroth
        , SomeAllyCard beorningSkinChanger
        , SomeAllyCard longLakeFisherman
        , SomeAllyCard porosGarrison
        , SomeAllyCard greenwoodDefender
        , SomeAllyCard gafferGamgee
        , SomeAllyCard ereborToymaker
        , SomeAllyCard messengerRaven
        , SomeAllyCard loyalHound
        , SomeAllyCard bywaterShirriff
        , SomeAllyCard wiglaf
        , SomeAllyCard meneldor
        , SomeAllyCard wildStallion
        , SomeAllyCard eagleOfTheNorth
        , SomeAllyCard forestRoadTraveler
        , SomeAllyCard giantBear
        , SomeAllyCard galion
        , SomeAllyCard thalion
        , SomeAllyCard soldierOfErebor
        , SomeAllyCard mirkwoodHunter
        , SomeAllyCard descendantOfGirion
        , SomeAllyCard soldierOfIsengard
        , SomeAllyCard noriTactics
        , SomeAllyCard rohirrimScout
        , SomeAllyCard wanderingExile
        , SomeAllyCard northernBowmaster
        , SomeAllyCard odoProudfoot
        , SomeAllyCard merryLeadership
        , SomeAllyCard pippinTactics
        , SomeAllyCard faramirLore
        , SomeAllyCard derufin
        , SomeAllyCard cautiousHalfling
        , SomeAllyCard tomBombadil
        , SomeAllyCard woodlandSentry
        , SomeAllyCard knightOfBelfalas
        , SomeAllyCard hobbitArcher
        , SomeAllyCard daleMessenger
        , SomeAllyCard eagleEmissary
        , SomeAllyCard wilyadorTheLandOfSorrow
        , SomeAllyCard haleth
        , SomeAllyCard mistyMountainJourneyman
        , SomeAllyCard edrahil
        , SomeAllyCard entOfFangorn
        , SomeAllyCard arador
        , SomeAllyCard farin
        , SomeAllyCard armyOfTheDead
        , SomeAllyCard gwaihirTheBlackGateOpens
        , SomeAllyCard anbornLeadership
        , SomeAllyCard mablungLore
        , SomeAllyCard skinbark
        , SomeAllyCard gamling
        , SomeAllyCard grimbold
        , SomeAllyCard princeImrahilSpirit
        , SomeAllyCard halbaradLeadership
        , SomeAllyCard ghanBuriGhan
        , SomeAllyCard rosieCotton
        , SomeAllyCard elfhelmTactics
        , SomeAllyCard elladanLore
        , SomeAllyCard elrohirSpirit
        , SomeAllyCard gildorInglorionObjective
        , SomeAllyCard billThePony
        , SomeAllyCard barlimanButterbur
        , SomeAllyCard farmerMaggot
        , SomeAllyCard galadrielTheRoadDarkens
        , SomeAllyCard boromirTheRoadDarkens
        , SomeAllyCard elrondTheRoadDarkens
        , SomeAllyCard bilboBagginsTheRoadDarkens
        , SomeAllyCard gimliTheTreasonOfSaruman
        , SomeAllyCard legolasTheTreasonOfSaruman
        , SomeAllyCard quickbeamTheTreasonOfSaruman
        , SomeAllyCard hamaTheTreasonOfSaruman
        , SomeAllyCard smeagolTheLandOfShadow
        , SomeAllyCard bofurTactics
        , SomeAllyCard gandalfHobbit
        , SomeAllyCard bifur
        , SomeAllyCard dwalin
        ]
    ]
