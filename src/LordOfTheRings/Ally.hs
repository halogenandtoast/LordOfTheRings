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
import LordOfTheRings.Ally.Cards.Landroval
import LordOfTheRings.Ally.Cards.HaldirOfLorien
import LordOfTheRings.Ally.Cards.Radagast
import LordOfTheRings.Ally.Cards.Wilyador
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
import LordOfTheRings.Ally.Cards.AmarthiulTheTreacheryOfRhudaur
import LordOfTheRings.Ally.Cards.DwarvenSellsword
import LordOfTheRings.Ally.Cards.WoodlandCourier
import LordOfTheRings.Ally.Cards.RobinSmallburrow
import LordOfTheRings.Ally.Cards.VeteranSwordElf
import LordOfTheRings.Ally.Cards.LinhirSeaCaptain
import LordOfTheRings.Ally.Cards.Ioreth
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
        , SomeAllyCard landroval
        , SomeAllyCard haldirOfLorien
        , SomeAllyCard radagast
        , SomeAllyCard wilyador
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
        , SomeAllyCard dwarvenSellsword
        , SomeAllyCard woodlandCourier
        , SomeAllyCard robinSmallburrow
        , SomeAllyCard veteranSwordElf
        , SomeAllyCard linhirSeaCaptain
        , SomeAllyCard ioreth
        ]
    ]
