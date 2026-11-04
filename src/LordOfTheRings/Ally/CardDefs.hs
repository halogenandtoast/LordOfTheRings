module LordOfTheRings.Ally.CardDefs (module LordOfTheRings.Ally.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere
import LordOfTheRings.Trait

allAllyCards :: Map CardCode CardDef
allAllyCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ guardOfTheCitadel
      , berelind
      , coruhaer
      , laermuin
      , elrondsChampion
      , rivendellSkirmishers
      , faramir
      , sonOfArnor
      , snowbournScout
      , silverlodeArcher
      , longbeardOrcSlayer
      , brokIronfist
      , veteranAxehand
      , gondorianSpearman
      , horsebackArcher
      , beorn
      , wanderingTook
      , lorienGuide
      , northernTracker
      , daughterOfTheNimrodel
      , ereborHammersmith
      , henamarthRiversong
      , minerOfTheIronHills
      , gleowine
      , gandalf
      , westfoldHorseBreaker
      , eomund
      , escortFromEdoras
      , theRiddermarksFinest
      , silvanTracker
      , westRoadTraveller
      , mirkwoodRunner
      , zigilMiner
      , ereborRecordKeeper
      , bombur
      , longbeardElder
      , errandRider
      , citadelCustodian
      , defenderOfRammas
      , envoyOfPelargir
      , damrod
      , hunterOfLamedon
      , ithilienTracker
      , masterOfLore
      , silvanRefugee
      , squireOfTheCitadel
      , guthlaf
      , pelargirShipCaptain
      , eredNimraisProspector
      , westfoldOutrider
      , westfoldHorseBreeder
      , naithGuide
      , rumil
      , galadhrimMinstrel
      , defenderOfTheNaith
      , galadhonArcher
      , orophin
      , galadrielsHandmaiden
      , eredLuinMiner
      , fili
      , kili
      , dori
      , gloin
      , greenwoodArcher
      , ereborGuard
      , riddermarkKnight
      , emynArnenRanger
      , soldierOfGondor
      , angborTheFearless
      , westfoldLancer
      , wingedGuardian
      , rivendellMinstrel
      , beorningBeekeeper
      , longbeardMapMaker
      , landroval
      , haldirOfLorien
      , radagast
      , keenEyedTook
      , descendantOfThorondor
      , gildorInglorion
      , dunedainWatcher
      , vassalOfTheWindlord
      , elfhelm
      , eaglesOfTheMistyMountains
      , veteranOfNanduhirion
      , bofur
      , ravenhillScout
      , dunedainWanderer
      , riderOfTheMark
      , watcherOfTheBruinen
      , arwenUndomiel
      , erestor
      , ereborBattleMaster
      , wardenOfHealing
      , trollshawScout
      , imladrisStargazer
      , masterOfTheForge
      , treebeard
      , glorfindel
      , marksmanOfLorien
      , yazan
      , jubayr
      , firyal
      , warriorOfLossarnach
      , knightsOfTheSwan
      , ethirSwordsman
      , anfalasHerdsman
      , forlong
      , harborMaster
      , whiteTowerWatchman
      , denethor
      , minasTirithLampwright
      , ithilienArcher
      , knightOfMinasTirith
      , pelargirShipwright
      , emery
      , anborn
      , orthancGuard
      , isengardMessenger
      , saruman
      , rivendellScout
      , wardenOfHelmsDeep
      , wanderingEnt
      , boomingEnt
      , ithilienLookout
      , celduinTraveler
      , greyfloodWanderer
      , heraldOfAnorien
      , hennethAnnunGuard
      , mirkwoodPioneer
      , blueMountainTrader
      , gwaihir
      , guardianOfArnor
      , beechbone
      , lindir
      , eothain
      , fornostBowman
      , galadhrimHealer
      , veteranOfOsgiliath
      , derndingleWarrior
      , elvenJeweler
      , longbeardSentry
      , wellinghallPreserver
      , galadhrimWeaver
      , galdorOfTheHavens
      , weatherHillsWatchman
      , dunedainHunter
      , sarnFordSentry
      , wardenOfAnnuminas
      , rangerOfTheNorth
      , ingold
      , honourGuard
      , curiousBrandybuck
      , eastRoadRanger
      , rangerOfCardolan
      , mithlondSeaWatcher
      , wardenOfTheHavens
      , lindonNavigator
      , sailorOfLune
      , azainSilverbeard
      , imladrisCaregiver
      , guardianOfRivendell
      , eldahir
      , masterIronsmith
      , samGamgee
      , mirkwoodExplorer
      , ceorl
      , deorwine
      , rhovanionOutrider
      , dwarvenSellsword
      , woodlandCourier
      , robinSmallburrow
      , veteranSwordElf
      , linhirSeaCaptain
      , ioreth
      , knightOfTheWhiteTower
      , soldierOfDolAmroth
      , sulien
      , guardianOfIthilien
      , starla
      , halflingBounder
      , vigilantDunadan
      , andrathGuardsman
      , kahlielsTribesman
      , dunedainPathfinder
      , erynGalenSettler
      , stewardOfOrthanc
      , riderOfRohan
      , dunedainLookout
      , defenderOfCairAndros
      , southronRefugee
      , knightOfDale
      , eregionSurvivor
      , leaflock
      , halfastGamgee
      , beorningGuardian
      , greenwoodDefender
      , gafferGamgee
      , ereborToymaker
      , messengerRaven
      , loyalHound
      , bywaterShirriff
      , redwaterSentry
      , northRealmLookout
      , warriorOfDale
      , longLakeTrader
      , guardianOfEsgaroth
      , beorningSkinChanger
      , longLakeFisherman
      , wiglaf
      , meneldor
      , wildStallion
      , eagleOfTheNorth
      , forestRoadTraveler
      , giantBear
      , galion
      , thalion
      , soldierOfErebor
      , mirkwoodHunter
      , descendantOfGirion
      ]

guardOfTheCitadel :: CardDef
guardOfTheCitadel =
  (ally "01013" "Guard of the Citadel" Leadership (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Gondor, Warrior]
    }

faramir :: CardDef
faramir =
  (ally "01014" "Faramir" Leadership (StaticCost 4) 2 1 2 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Noble, Ranger]
    }

sonOfArnor :: CardDef
sonOfArnor =
  (ally "01015" "Son of Arnor" Leadership (StaticCost 3) 0 2 0 2)
    { cdCardTraits = setFromList [Dunedain]
    }

snowbournScout :: CardDef
snowbournScout =
  (ally "01016" "Snowbourn Scout" Leadership (StaticCost 1) 0 0 1 1)
    { cdCardTraits = setFromList [Rohan, Scout]
    }

silverlodeArcher :: CardDef
silverlodeArcher =
  (ally "01017" "Silverlode Archer" Leadership (StaticCost 3) 1 2 0 1)
    { cdCardTraits = setFromList [Archer, Silvan]
    , cdKeywords = setFromList [Ranged]
    }

longbeardOrcSlayer :: CardDef
longbeardOrcSlayer =
  (ally "01018" "Longbeard Orc Slayer" Leadership (StaticCost 4) 0 2 1 3)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    }

brokIronfist :: CardDef
brokIronfist =
  (ally "01019" "Brok Ironfist" Leadership (StaticCost 6) 2 2 1 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf, Warrior]
    }

veteranAxehand :: CardDef
veteranAxehand =
  (ally "01028" "Veteran Axehand" Tactics (StaticCost 2) 0 2 1 2)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    }

gondorianSpearman :: CardDef
gondorianSpearman =
  (ally "01029" "Gondorian Spearman" Tactics (StaticCost 2) 0 1 1 1)
    { cdCardTraits = setFromList [Gondor, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

horsebackArcher :: CardDef
horsebackArcher =
  (ally "01030" "Horseback Archer" Tactics (StaticCost 3) 0 2 1 2)
    { cdCardTraits = setFromList [Archer, Rohan]
    , cdKeywords = setFromList [Ranged]
    }

beorn :: CardDef
beorn =
  (ally "01031" "Beorn" Tactics (StaticCost 6) 1 3 3 6)
    { cdUnique = True
    , cdCardTraits = setFromList [Beorning, Warrior]
    }

wanderingTook :: CardDef
wanderingTook =
  (ally "01043" "Wandering Took" Spirit (StaticCost 2) 1 1 1 2)
    { cdCardTraits = setFromList [Hobbit]
    }

lorienGuide :: CardDef
lorienGuide =
  (ally "01044" "Lórien Guide" Spirit (StaticCost 3) 1 1 0 2)
    { cdCardTraits = setFromList [Scout, Silvan]
    }

northernTracker :: CardDef
northernTracker =
  (ally "01045" "Northern Tracker" Spirit (StaticCost 4) 1 2 2 3)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

daughterOfTheNimrodel :: CardDef
daughterOfTheNimrodel =
  (ally "01058" "Daughter of the Nimrodel" Lore (StaticCost 3) 1 0 0 1)
    { cdCardTraits = setFromList [Silvan]
    }

ereborHammersmith :: CardDef
ereborHammersmith =
  (ally "01059" "Erebor Hammersmith" Lore (StaticCost 2) 1 1 1 3)
    { cdCardTraits = setFromList [Craftsman, Dwarf]
    }

henamarthRiversong :: CardDef
henamarthRiversong =
  (ally "01060" "Henamarth Riversong" Lore (StaticCost 1) 1 1 0 1)
    { cdUnique = True
    , cdCardTraits = setFromList [Silvan]
    }

minerOfTheIronHills :: CardDef
minerOfTheIronHills =
  (ally "01061" "Miner of the Iron Hills" Lore (StaticCost 2) 0 1 1 2)
    { cdCardTraits = setFromList [Dwarf]
    }

gleowine :: CardDef
gleowine =
  (ally "01062" "Gléowine" Lore (StaticCost 2) 1 0 0 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Minstrel, Rohan]
    }

gandalf :: CardDef
gandalf =
  (ally "01073" "Gandalf" Neutral (StaticCost 5) 4 4 4 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Istari]
    }

westfoldHorseBreaker :: CardDef
westfoldHorseBreaker =
  (ally "02006" "Westfold Horse-breaker" Spirit (StaticCost 2) 1 0 1 1)
    { cdCardTraits = setFromList [Rohan]
    }

eomund :: CardDef
eomund =
  (ally "02030" "Éomund" Spirit (StaticCost 3) 2 1 1 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Rohan]
    }

escortFromEdoras :: CardDef
escortFromEdoras =
  (ally "02055" "Escort from Edoras" Spirit (StaticCost 2) 2 0 0 1)
    { cdCardTraits = setFromList [Rohan]
    }

theRiddermarksFinest :: CardDef
theRiddermarksFinest =
  (ally "02077" "The Riddermark's Finest" Spirit (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Creature, Rohan]
    }

silvanTracker :: CardDef
silvanTracker =
  (ally "02102" "Silvan Tracker" Lore (StaticCost 3) 1 1 1 3)
    { cdCardTraits = setFromList [Silvan]
    }

westRoadTraveller :: CardDef
westRoadTraveller =
  (ally "02121" "West Road Traveller" Spirit (StaticCost 2) 2 0 0 1)
    { cdCardTraits = setFromList [Rohan]
    }

mirkwoodRunner :: CardDef
mirkwoodRunner =
  (ally "02123" "Mirkwood Runner" Lore (StaticCost 3) 1 2 0 2)
    { cdCardTraits = setFromList [Scout, Silvan]
    }

zigilMiner :: CardDef
zigilMiner =
  (ally "03009" "Zigil Miner" Spirit (StaticCost 2) 1 1 1 1)
    { cdCardTraits = setFromList [Dwarf]
    }

ereborRecordKeeper :: CardDef
ereborRecordKeeper =
  (ally "03011" "Erebor Record Keeper" Lore (StaticCost 1) 1 0 0 1)
    { cdCardTraits = setFromList [Dwarf]
    }

bombur :: CardDef
bombur =
  (ally "04035" "Bombur" Lore (StaticCost 3) 0 0 1 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf]
    }

longbeardElder :: CardDef
longbeardElder =
  (ally "04102" "Longbeard Elder" Leadership (StaticCost 3) 2 1 1 2)
    { cdCardTraits = setFromList [Dwarf]
    }

errandRider :: CardDef
errandRider =
  (ally "05003" "Errand-rider" Leadership (StaticCost 1) 0 0 0 2)
    { cdCardTraits = setFromList [Gondor]
    }

citadelCustodian :: CardDef
citadelCustodian =
  (ally "05004" "Citadel Custodian" Leadership (StaticCost 5) 1 0 1 3)
    { cdCardTraits = setFromList [Gondor]
    }

defenderOfRammas :: CardDef
defenderOfRammas =
  (ally "05007" "Defender of Rammas" Tactics (StaticCost 2) 0 1 4 1)
    { cdCardTraits = setFromList [Gondor, Warrior]
    }

envoyOfPelargir :: CardDef
envoyOfPelargir =
  (ally "05018" "Envoy of Pelargir" Neutral (StaticCost 2) 1 1 0 1)
    { cdCardTraits = setFromList [Gondor]
    }

damrod :: CardDef
damrod =
  (ally "05010" "Damrod" Spirit (StaticCost 4) 1 2 2 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Ranger]
    }

hunterOfLamedon :: CardDef
hunterOfLamedon =
  (ally "05014" "Hunter of Lamedon" Lore (StaticCost 2) 1 1 1 1)
    { cdCardTraits = setFromList [Gondor, Outlands]
    }

ithilienTracker :: CardDef
ithilienTracker =
  (ally "05015" "Ithilien Tracker" Lore (StaticCost 2) 0 1 0 3)
    { cdCardTraits = setFromList [Gondor, Ranger]
    }

masterOfLore :: CardDef
masterOfLore =
  (ally "05016" "Master of Lore" Lore (StaticCost 3) 1 0 1 1)
    { cdCardTraits = setFromList [Gondor]
    }

silvanRefugee :: CardDef
silvanRefugee =
  (ally "06037" "Silvan Refugee" Spirit (StaticCost 1) 2 0 0 1)
    { cdCardTraits = setFromList [Scout, Silvan]
    }

squireOfTheCitadel :: CardDef
squireOfTheCitadel =
  (ally "06108" "Squire of the Citadel" Leadership (StaticCost 1) 0 0 1 1)
    { cdCardTraits = setFromList [Gondor]
    }

guthlaf :: CardDef
guthlaf =
  (ally "06110" "Guthlaf" Tactics (StaticCost 3) 1 1 2 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Rohan]
    }

pelargirShipCaptain :: CardDef
pelargirShipCaptain =
  (ally "06135" "Pelargir Ship Captain" Leadership (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Gondor]
    }

eredNimraisProspector :: CardDef
eredNimraisProspector =
  (ally "06141" "Ered Nimrais Prospector" Lore (StaticCost 2) 1 0 1 2)
    { cdCardTraits = setFromList [Dwarf]
    }

westfoldOutrider :: CardDef
westfoldOutrider =
  (ally "07006" "Westfold Outrider" Tactics (StaticCost 2) 0 2 1 2)
    { cdCardTraits = setFromList [Rohan, Scout]
    }

westfoldHorseBreeder :: CardDef
westfoldHorseBreeder =
  (ally "07007" "Westfold Horse-breeder" Spirit (StaticCost 1) 1 0 0 1)
    { cdCardTraits = setFromList [Rohan]
    }

naithGuide :: CardDef
naithGuide =
  (ally "08002" "Naith Guide" Leadership (StaticCost 2) 1 1 0 1)
    { cdCardTraits = setFromList [Scout, Silvan]
    }

rumil :: CardDef
rumil =
  (ally "08028" "Rúmil" Tactics (StaticCost 4) 2 2 1 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Scout, Silvan]
    , cdKeywords = setFromList [Ranged]
    }

galadhrimMinstrel :: CardDef
galadhrimMinstrel =
  (ally "08063" "Galadhrim Minstrel" Lore (StaticCost 2) 1 0 0 1)
    { cdCardTraits = setFromList [Minstrel, Silvan]
    }

defenderOfTheNaith :: CardDef
defenderOfTheNaith =
  (ally "08065" "Defender of the Naith" Neutral (StaticCost 3) 0 1 2 2)
    { cdCardTraits = setFromList [Silvan, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

galadhonArcher :: CardDef
galadhonArcher =
  (ally "08087" "Galadhon Archer" Tactics (StaticCost 2) 0 2 0 1)
    { cdCardTraits = setFromList [Silvan, Warrior]
    , cdKeywords = setFromList [Ranged]
    }

orophin :: CardDef
orophin =
  (ally "08114" "Orophin" Leadership (StaticCost 3) 2 2 0 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Scout, Silvan]
    , cdKeywords = setFromList [Ranged]
    }

galadrielsHandmaiden :: CardDef
galadrielsHandmaiden =
  (ally "08117" "Galadriel's Handmaiden" Spirit (StaticCost 2) 2 0 1 1)
    { cdCardTraits = setFromList [Silvan]
    }

eredLuinMiner :: CardDef
eredLuinMiner =
  (ally "12066" "Ered Luin Miner" Neutral (StaticCost 3) 1 1 1 2)
    { cdCardTraits = setFromList [Dwarf]
    }

fili :: CardDef
fili =
  (ally "131006" "Fili" Leadership (StaticCost 3) 1 1 1 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf]
    }

kili :: CardDef
kili =
  (ally "131007" "Kili" Spirit (StaticCost 3) 1 1 1 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf]
    }

dori :: CardDef
dori =
  (ally "131009" "Dori" Lore (StaticCost 3) 1 2 1 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf]
    }

gloin :: CardDef
gloin =
  (ally "132006" "Glóin" Leadership (StaticCost 3) 2 1 1 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf]
    }

greenwoodArcher :: CardDef
greenwoodArcher =
  (ally "16003" "Greenwood Archer" Leadership (StaticCost 2) 0 2 0 1)
    { cdCardTraits = setFromList [Silvan]
    , cdKeywords = setFromList [Ranged]
    }

ereborGuard :: CardDef
ereborGuard =
  (ally "16004" "Erebor Guard" Spirit (StaticCost 4) 0 1 2 3)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

riddermarkKnight :: CardDef
riddermarkKnight =
  (ally "17112" "Riddermark Knight" Tactics (StaticCost 2) 0 2 0 2)
    { cdCardTraits = setFromList [Rohan, Warrior]
    }

emynArnenRanger :: CardDef
emynArnenRanger =
  (ally "17141" "Emyn Arnen Ranger" Lore (StaticCost 2) 0 1 1 2)
    { cdCardTraits = setFromList [Gondor, Ranger]
    }

soldierOfGondor :: CardDef
soldierOfGondor =
  (ally "22002" "Soldier of Gondor" Leadership (StaticCost 2) 1 1 1 2)
    { cdCardTraits = setFromList [Gondor]
    }

angborTheFearless :: CardDef
angborTheFearless =
  (ally "22028" "Angbor the Fearless" Leadership (StaticCost 2) 2 1 0 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Warrior]
    }

westfoldLancer :: CardDef
westfoldLancer =
  (ally "22142" "Westfold Lancer" Tactics (StaticCost 3) 2 2 0 2)
    { cdCardTraits = setFromList [Rohan, Warrior]
    }

wingedGuardian :: CardDef
wingedGuardian =
  (ally "02004" "Winged Guardian" Tactics (StaticCost 2) 0 0 4 1)
    { cdCardTraits = setFromList [Creature, Eagle]
    , cdKeywords = setFromList [Sentinel]
    }

rivendellMinstrel :: CardDef
rivendellMinstrel =
  (ally "02008" "Rivendell Minstrel" Lore (StaticCost 3) 2 0 0 1)
    { cdCardTraits = setFromList [Noldor]
    }

beorningBeekeeper :: CardDef
beorningBeekeeper =
  (ally "02028" "Beorning Beekeeper" Tactics (StaticCost 4) 1 2 1 3)
    { cdCardTraits = setFromList [Beorning]
    }

longbeardMapMaker :: CardDef
longbeardMapMaker =
  (ally "02032" "Longbeard Map-Maker" Lore (StaticCost 3) 1 1 1 3)
    { cdCardTraits = setFromList [Dwarf]
    }

landroval :: CardDef
landroval =
  (ally "02053" "Landroval" Tactics (StaticCost 5) 1 3 1 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Creature, Eagle]
    , cdKeywords = setFromList [Sentinel]
    }

haldirOfLorien :: CardDef
haldirOfLorien =
  (ally "02057" "Haldir of Lórien" Lore (StaticCost 4) 2 2 2 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Silvan]
    , cdKeywords = setFromList [Ranged, Sentinel]
    }

radagast :: CardDef
radagast =
  (ally "02059" "Radagast" Neutral (StaticCost 5) 2 1 1 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Istari]
    }

keenEyedTook :: CardDef
keenEyedTook =
  (ally "02073" "Keen-eyed Took" Leadership (StaticCost 2) 1 0 0 2)
    { cdCardTraits = setFromList [Hobbit]
    }

descendantOfThorondor :: CardDef
descendantOfThorondor =
  (ally "02075" "Descendant of Thorondor" Tactics (StaticCost 4) 1 2 1 2)
    { cdCardTraits = setFromList [Creature, Eagle]
    }

gildorInglorion :: CardDef
gildorInglorion =
  (ally "02079" "Gildor Inglorion" Lore (StaticCost 5) 3 2 3 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Noldor]
    }

dunedainWatcher :: CardDef
dunedainWatcher =
  (ally "02096" "Dúnedain Watcher" Leadership (StaticCost 3) 1 1 1 2)
    { cdCardTraits = setFromList [Dunedain]
    }

vassalOfTheWindlord :: CardDef
vassalOfTheWindlord =
  (ally "02098" "Vassal of the Windlord" Tactics (StaticCost 1) 0 3 0 1)
    { cdCardTraits = setFromList [Creature, Eagle]
    , cdKeywords = setFromList [Ranged]
    }

elfhelm :: CardDef
elfhelm =
  (ally "02100" "Elfhelm" Spirit (StaticCost 4) 1 2 2 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Rohan, Warrior]
    }

eaglesOfTheMistyMountains :: CardDef
eaglesOfTheMistyMountains =
  (ally "02119" "Eagles of the Misty Mountains" Tactics (StaticCost 4) 2 2 2 4)
    { cdCardTraits = setFromList [Creature, Eagle]
    }

veteranOfNanduhirion :: CardDef
veteranOfNanduhirion =
  (ally "03006" "Veteran of Nanduhirion" Tactics (StaticCost 4) 0 3 2 3)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    }

bofur :: CardDef
bofur =
  unique
    (ally "04006" "Bofur" Spirit (StaticCost 3) 2 1 1 3)
      { cdCardTraits = setFromList [Dwarf]
      }

ravenhillScout :: CardDef
ravenhillScout =
  (ally "04008" "Ravenhill Scout" Lore (StaticCost 3) 0 1 1 3)
    { cdCardTraits = setFromList [Dale, Scout]
    }

dunedainWanderer :: CardDef
dunedainWanderer =
  (ally "04029" "Dúnedain Wanderer" Leadership (StaticCost 5) 1 2 2 2)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    , cdKeywords = setFromList [Ranged, Sentinel, Secrecy 3]
    }

riderOfTheMark :: CardDef
riderOfTheMark =
  (ally "04033" "Rider of the Mark" Spirit (StaticCost 3) 2 1 1 2)
    { cdCardTraits = setFromList [Rohan]
    }

watcherOfTheBruinen :: CardDef
watcherOfTheBruinen =
  (ally "04056" "Watcher of the Bruinen" Tactics (StaticCost 2) 0 1 2 2)
    { cdCardTraits = setFromList [Noldor, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

arwenUndomiel :: CardDef
arwenUndomiel =
  unique
    (ally "04058" "Arwen Undómiel" Spirit (StaticCost 2) 2 0 1 2)
      { cdCardTraits = setFromList [Noldor, Noble]
      }

erestor :: CardDef
erestor =
  unique
    (ally "04077" "Erestor" Leadership (StaticCost 4) 2 0 1 3)
      { cdCardTraits = setFromList [Noldor]
      }

ereborBattleMaster :: CardDef
ereborBattleMaster =
  (ally "04079" "Erebor Battle Master" Tactics (StaticCost 3) 0 1 1 2)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    }

wardenOfHealing :: CardDef
wardenOfHealing =
  (ally "04083" "Warden of Healing" Lore (StaticCost 2) 1 0 1 1)
    { cdCardTraits = setFromList [Gondor, Healer]
    }

trollshawScout :: CardDef
trollshawScout =
  (ally "04104" "Trollshaw Scout" Tactics (StaticCost 2) 0 2 1 2)
    { cdCardTraits = setFromList [Noldor, Scout]
    , cdKeywords = setFromList [Ranged]
    }

imladrisStargazer :: CardDef
imladrisStargazer =
  (ally "04106" "Imladris Stargazer" Spirit (StaticCost 2) 0 0 1 1)
    { cdCardTraits = setFromList [Noldor]
    }

masterOfTheForge :: CardDef
masterOfTheForge =
  (ally "04134" "Master of the Forge" Lore (StaticCost 2) 0 0 1 1)
    { cdCardTraits = setFromList [Noldor, Craftsman]
    }

treebeard :: CardDef
treebeard =
  (ally "08146" "Treebeard" Neutral (StaticCost 4) 2 4 3 5)
    { cdUnique = True
    , cdCardTraits = setFromList [Ent]
    }

glorfindel :: CardDef
glorfindel =
  (ally "12006" "Glorfindel" Spirit (StaticCost 5) 3 3 1 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Noldor, Noble, Warrior]
    , cdPlayableFrom = FromHandOrDiscard
    }

marksmanOfLorien :: CardDef
marksmanOfLorien =
  (ally "12085" "Marksman of Lórien" Tactics (StaticCost 3) 0 3 0 2)
    { cdCardTraits = setFromList [Silvan, Warrior]
    , cdKeywords = setFromList [Ranged]
    }

yazan :: CardDef
yazan =
  (ally "17004" "Yazan" Tactics (StaticCost 5) 2 3 1 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Harad, Ranger]
    , cdKeywords = setFromList [Ranged]
    }

jubayr :: CardDef
jubayr =
  (ally "17006" "Jubayr" Spirit (StaticCost 5) 1 2 3 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Harad, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

firyal :: CardDef
firyal =
  (ally "17008" "Firyal" Lore (StaticCost 5) 3 1 2 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Harad, Scout]
    }

warriorOfLossarnach :: CardDef
warriorOfLossarnach =
  (ally "06002" "Warrior of Lossarnach" Leadership (StaticCost 2) 1 1 1 1)
    { cdCardTraits = setFromList [Outlands]
    }

knightsOfTheSwan :: CardDef
knightsOfTheSwan =
  (ally "06004" "Knights of the Swan" Tactics (StaticCost 1) 0 0 0 1)
    { cdCardTraits = setFromList [Outlands]
    }

ethirSwordsman :: CardDef
ethirSwordsman =
  (ally "06006" "Ethir Swordsman" Spirit (StaticCost 2) 1 1 1 1)
    { cdCardTraits = setFromList [Outlands]
    }

anfalasHerdsman :: CardDef
anfalasHerdsman =
  (ally "06008" "Anfalas Herdsman" Lore (StaticCost 1) 0 0 0 1)
    { cdCardTraits = setFromList [Outlands]
    }

forlong :: CardDef
forlong =
  (ally "06033" "Forlong" Leadership (StaticCost 3) 1 1 1 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Outlands]
    }

harborMaster :: CardDef
harborMaster =
  (ally "06039" "Harbor Master" Lore (StaticCost 3) 1 2 0 2)
    { cdCardTraits = setFromList [Noldor]
    }

whiteTowerWatchman :: CardDef
whiteTowerWatchman =
  (ally "06041" "White Tower Watchman" Neutral (StaticCost 3) 1 0 2 3)
    { cdCardTraits = setFromList [Gondor]
    }

denethor :: CardDef
denethor =
  (ally "06057" "Denethor" Leadership (StaticCost 4) 3 1 2 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Noble]
    }

minasTirithLampwright :: CardDef
minasTirithLampwright =
  (ally "06061" "Minas Tirith Lampwright" Spirit (StaticCost 1) 0 0 1 1)
    { cdCardTraits = setFromList [Craftsman, Gondor]
    }

ithilienArcher :: CardDef
ithilienArcher =
  (ally "06063" "Ithilien Archer" Lore (StaticCost 3) 1 2 1 2)
    { cdCardTraits = setFromList [Gondor, Ranger]
    , cdKeywords = setFromList [Ranged]
    }

knightOfMinasTirith :: CardDef
knightOfMinasTirith =
  (ally "06084" "Knight of Minas Tirith" Tactics (StaticCost 3) 0 3 1 3)
    { cdCardTraits = setFromList [Gondor, Warrior]
    }

pelargirShipwright :: CardDef
pelargirShipwright =
  (ally "06086" "Pelargir Shipwright" Spirit (StaticCost 3) 0 1 1 3)
    { cdCardTraits = setFromList [Craftsman, Gondor]
    }

emery :: CardDef
emery =
  (ally "06112" "Emery" Spirit (StaticCost 3) 1 1 2 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor]
    }

anborn :: CardDef
anborn =
  (ally "06114" "Anborn" Lore (StaticCost 4) 1 3 1 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Ranger]
    }

orthancGuard :: CardDef
orthancGuard =
  (ally "07004" "Orthanc Guard" Leadership (StaticCost 2) 1 0 2 2)
    { cdCardTraits = setFromList [Isengard]
    }

isengardMessenger :: CardDef
isengardMessenger =
  (ally "07005" "Isengard Messenger" Lore (StaticCost 2) 1 0 1 2)
    { cdCardTraits = setFromList [Isengard]
    }

saruman :: CardDef
saruman =
  (ally "07003" "Saruman" Neutral (StaticCost 3) 3 5 4 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Istari, Isengard]
    , cdKeywords = setFromList [Doomed 3]
    }

rivendellScout :: CardDef
rivendellScout =
  (ally "08026" "Rivendell Scout" Leadership (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Noldor, Scout]
    , cdKeywords = setFromList [Secrecy 2]
    }

wardenOfHelmsDeep :: CardDef
wardenOfHelmsDeep =
  (ally "08138" "Warden of Helm's Deep" Leadership (StaticCost 3) 0 1 3 2)
    { cdCardTraits = setFromList [Rohan, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

wanderingEnt :: CardDef
wanderingEnt =
  (ally "08119" "Wandering Ent" Lore (StaticCost 2) 2 2 2 3)
    { cdCardTraits = setFromList [Ent]
    }

boomingEnt :: CardDef
boomingEnt =
  (ally "08141" "Booming Ent" Tactics (StaticCost 2) 0 2 2 3)
    { cdCardTraits = setFromList [Ent]
    }

ithilienLookout :: CardDef
ithilienLookout =
  (ally "08008" "Ithilien Lookout" Lore (StaticCost 3) 1 2 0 2)
    { cdCardTraits = setFromList [Gondor, Ranger]
    , cdKeywords = setFromList [Secrecy 2]
    }

celduinTraveler :: CardDef
celduinTraveler =
  (ally "08089" "Celduin Traveler" Spirit (StaticCost 3) 2 0 1 2)
    { cdCardTraits = setFromList [Dale, Scout]
    , cdKeywords = setFromList [Secrecy 2]
    }

greyfloodWanderer :: CardDef
greyfloodWanderer =
  (ally "08030" "Greyflood Wanderer" Spirit (StaticCost 3) 2 1 0 2)
    { cdCardTraits = setFromList [Dunedain, Scout]
    }

heraldOfAnorien :: CardDef
heraldOfAnorien =
  (ally "08057" "Herald of Anórien" Leadership (StaticCost 2) 1 0 1 1)
    { cdCardTraits = setFromList [Gondor]
    }

hennethAnnunGuard :: CardDef
hennethAnnunGuard =
  (ally "08115" "Henneth Annûn Guard" Tactics (StaticCost 3) 0 2 2 2)
    { cdCardTraits = setFromList [Gondor, Warrior]
    }

mirkwoodPioneer :: CardDef
mirkwoodPioneer =
  (ally "08091" "Mirkwood Pioneer" Lore (StaticCost 2) 1 0 0 2)
    { cdCardTraits = setFromList [Woodman]
    }

blueMountainTrader :: CardDef
blueMountainTrader =
  (ally "08006" "Blue Mountain Trader" Spirit (StaticCost 2) 1 0 1 2)
    { cdCardTraits = setFromList [Dwarf]
    }

gwaihir :: CardDef
gwaihir =
  (ally "08059" "Gwaihir" Tactics (StaticCost 5) 2 3 1 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Creature, Eagle]
    }

guardianOfArnor :: CardDef
guardianOfArnor =
  (ally "10116" "Guardian of Arnor" Leadership (StaticCost 3) 1 1 1 3)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    , cdKeywords = setFromList [Sentinel]
    }

beechbone :: CardDef
beechbone =
  (ally "10118" "Beechbone" Tactics (StaticCost 3) 2 2 1 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Ent]
    }

lindir :: CardDef
lindir =
  (ally "10120" "Lindir" Spirit (StaticCost 3) 2 1 2 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Noldor]
    }

eothain :: CardDef
eothain =
  (ally "10141" "Éothain" Leadership (StaticCost 4) 2 2 1 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Rohan, Warrior]
    }

fornostBowman :: CardDef
fornostBowman =
  (ally "10143" "Fornost Bowman" Tactics (StaticCost 3) 1 1 1 3)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    , cdKeywords = setFromList [Ranged]
    }

galadhrimHealer :: CardDef
galadhrimHealer =
  (ally "10147" "Galadhrim Healer" Lore (StaticCost 2) 1 0 1 1)
    { cdCardTraits = setFromList [Silvan, Healer]
    }

veteranOfOsgiliath :: CardDef
veteranOfOsgiliath =
  (ally "10029" "Veteran of Osgiliath" Leadership (StaticCost 3) 1 2 1 2)
    { cdCardTraits = setFromList [Gondor, Warrior]
    }

derndingleWarrior :: CardDef
derndingleWarrior =
  (ally "10031" "Derndingle Warrior" Tactics (StaticCost 2) 1 1 2 3)
    { cdCardTraits = setFromList [Ent, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

elvenJeweler :: CardDef
elvenJeweler =
  (ally "10033" "Elven Jeweler" Spirit (StaticCost 2) 1 1 1 2)
    { cdCardTraits = setFromList [Noldor, Craftsman]
    }

longbeardSentry :: CardDef
longbeardSentry =
  (ally "10057" "Longbeard Sentry" Tactics (StaticCost 3) 0 1 2 3)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    }

wellinghallPreserver :: CardDef
wellinghallPreserver =
  (ally "10061" "Wellinghall Preserver" Lore (StaticCost 3) 3 2 2 3)
    { cdCardTraits = setFromList [Ent]
    }

galadhrimWeaver :: CardDef
galadhrimWeaver =
  (ally "10089" "Galadhrim Weaver" Spirit (StaticCost 1) 1 0 0 1)
    { cdCardTraits = setFromList [Silvan, Craftsman]
    }

galdorOfTheHavens :: CardDef
galdorOfTheHavens =
  (ally "10091" "Galdor of the Havens" Lore (StaticCost 4) 2 2 1 4)
    { cdCardTraits = setFromList [Noldor]
    , cdUnique = True
    }

weatherHillsWatchman :: CardDef
weatherHillsWatchman =
  (ally "09003" "Weather Hills Watchman" Leadership (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Dunedain]
    }

dunedainHunter :: CardDef
dunedainHunter =
  (ally "09004" "Dúnedain Hunter" Tactics (StaticCost 0) 1 3 1 3)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

sarnFordSentry :: CardDef
sarnFordSentry =
  (ally "09005" "Sarn Ford Sentry" Lore (StaticCost 3) 2 1 0 2)
    { cdCardTraits = setFromList [Dunedain, Scout]
    }

wardenOfAnnuminas :: CardDef
wardenOfAnnuminas =
  (ally "09006" "Warden of Annúminas" Spirit (StaticCost 4) 0 2 2 3)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

-- | Encounter: set aside from its owner's deck, it enters play only as Ranger Summons
-- shuffles it into the encounter deck and it is revealed. It has no cost.
rangerOfTheNorth :: CardDef
rangerOfTheNorth =
  (ally "09015" "Ranger of the North" Neutral (StaticCost 0) 2 2 2 3)
    { cdCost = Nothing
    , cdCardTraits = setFromList [Dunedain, Ranger]
    , cdKeywords = setFromList [Encounter, Surge, Ranged, Sentinel]
    , cdWhenRevealed = True
    , cdEncounterToOwnersDiscard = True
    }

ingold :: CardDef
ingold =
  unique
    (ally "10002" "Ingold" Leadership (StaticCost 3) 0 1 1 3)
      { cdCardTraits = setFromList [Gondor]
      }

honourGuard :: CardDef
honourGuard =
  (ally "10004" "Honour Guard" Tactics (StaticCost 2) 0 0 1 3)
    { cdCardTraits = setFromList [Gondor, Warrior]
    }

curiousBrandybuck :: CardDef
curiousBrandybuck =
  (ally "10006" "Curious Brandybuck" Spirit (StaticCost 2) 2 0 0 2)
    { cdCardTraits = setFromList [Hobbit]
    }

eastRoadRanger :: CardDef
eastRoadRanger =
  (ally "10008" "East Road Ranger" Lore (StaticCost 3) 1 2 1 3)
    { cdCardTraits = setFromList [Dunedain, Scout]
    }

rangerOfCardolan :: CardDef
rangerOfCardolan =
  (ally "10010" "Ranger of Cardolan" Neutral (StaticCost 4) 2 2 2 3)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

mithlondSeaWatcher :: CardDef
mithlondSeaWatcher =
  (ally "11003" "Mithlond Sea-watcher" Tactics (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Noldor, Warrior]
    }

wardenOfTheHavens :: CardDef
wardenOfTheHavens =
  (ally "11006" "Warden of the Havens" Leadership (StaticCost 2) 0 1 1 3)
    { cdCardTraits = setFromList [Noldor, Warrior]
    }

lindonNavigator :: CardDef
lindonNavigator =
  (ally "11009" "Lindon Navigator" Lore (StaticCost 2) 2 1 1 2)
    { cdCardTraits = setFromList [Noldor, Scout]
    }

sailorOfLune :: CardDef
sailorOfLune =
  (ally "11012" "Sailor of Lune" Spirit (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Noldor, Scout]
    }

azainSilverbeard :: CardDef
azainSilverbeard =
  (ally "12004" "Azain Silverbeard" Tactics (StaticCost 3) 0 3 2 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf, Warrior]
    }

imladrisCaregiver :: CardDef
imladrisCaregiver =
  (ally "12008" "Imladris Caregiver" Lore (StaticCost 2) 1 0 1 2)
    { cdCardTraits = setFromList [Noldor, Healer]
    }

guardianOfRivendell :: CardDef
guardianOfRivendell =
  (ally "12010" "Guardian of Rivendell" Neutral (StaticCost 3) 1 2 3 3)
    { cdCardTraits = setFromList [Noldor, Warrior]
    , cdAdditionalCost = Just (DiscardCardsFromHand 2)
    }

eldahir :: CardDef
eldahir =
  (ally "12031" "Eldahir" Leadership (StaticCost 4) 1 2 2 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Dunedain, Noble]
    , cdKeywords = setFromList [Sentinel]
    }

masterIronsmith :: CardDef
masterIronsmith =
  (ally "12033" "Master Ironsmith" Tactics (StaticCost 3) 1 1 1 2)
    { cdCardTraits = setFromList [Gondor, Craftsman]
    }

samGamgee :: CardDef
samGamgee =
  (ally "12035" "Sam Gamgee" Spirit (StaticCost 3) 2 1 1 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Hobbit]
    }

mirkwoodExplorer :: CardDef
mirkwoodExplorer =
  (ally "12037" "Mirkwood Explorer" Lore (StaticCost 3) 2 1 0 2)
    { cdCardTraits = setFromList [Woodman, Scout]
    }

ceorl :: CardDef
ceorl =
  (ally "12058" "Ceorl" Leadership (StaticCost 2) 1 2 0 2)
    { cdUnique = True
    , cdCardTraits = setFromList [Rohan, Scout]
    }

deorwine :: CardDef
deorwine =
  (ally "12060" "Déorwine" Tactics (StaticCost 4) 0 1 3 3)
    { cdUnique = True
    , cdCardTraits = setFromList [Rohan, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

rhovanionOutrider :: CardDef
rhovanionOutrider =
  (ally "12062" "Rhovanion Outrider" Spirit (StaticCost 3) 1 2 1 2)
    { cdCardTraits = setFromList [Dale, Scout]
    }

dwarvenSellsword :: CardDef
dwarvenSellsword =
  (ally "12083" "Dwarven Sellsword" Leadership (StaticCost 1) 2 2 2 3)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    }

woodlandCourier :: CardDef
woodlandCourier =
  (ally "12087" "Woodland Courier" Spirit (StaticCost 2) 1 1 0 1)
    { cdCardTraits = setFromList [Silvan, Scout]
    }

robinSmallburrow :: CardDef
robinSmallburrow =
  unique
    (ally "12089" "Robin Smallburrow" Lore (StaticCost 2) 2 0 1 2)
      { cdCardTraits = setFromList [Hobbit, Shirriff]
      }

veteranSwordElf :: CardDef
veteranSwordElf =
  (ally "12112" "Veteran Sword-elf" Tactics (StaticCost 3) 1 1 1 3)
    { cdCardTraits = setFromList [Noldor, Warrior]
    }

linhirSeaCaptain :: CardDef
linhirSeaCaptain =
  (ally "12115" "Linhir Sea-captain" Spirit (StaticCost 3) 2 0 1 2)
    { cdCardTraits = setFromList [Gondor]
    }

ioreth :: CardDef
ioreth =
  unique
    (ally "12117" "Ioreth" Lore (StaticCost 0) 0 0 0 1)
      { cdCardTraits = setFromList [Gondor, Healer]
      }

knightOfTheWhiteTower :: CardDef
knightOfTheWhiteTower =
  (ally "12139" "Knight of the White Tower" Leadership (StaticCost 4) 2 2 3 3)
    { cdCardTraits = setFromList [Gondor, Warrior]
    , cdSinglePayer = True
    }

soldierOfDolAmroth :: CardDef
soldierOfDolAmroth =
  (ally "12141" "Soldier of Dol Amroth" Tactics (StaticCost 2) 0 1 1 2)
    { cdCardTraits = setFromList [Gondor, Warrior]
    }

sulien :: CardDef
sulien =
  unique
    (ally "12142" "Súlien" Spirit (StaticCost 4) 3 0 2 2)
      { cdCardTraits = setFromList [Dunedain, Scout]
      }

guardianOfIthilien :: CardDef
guardianOfIthilien =
  (ally "12144" "Guardian of Ithilien" Lore (StaticCost 1) 0 1 0 1)
    { cdCardTraits = setFromList [Gondor, Ranger]
    }

-- | A boon of the Dream-chaser campaign: X is half her level, rounded up.
starla :: CardDef
starla = unique (ally "DCC198" "Starla" Neutral (StaticCost 0) 0 0 0 4) {cdCardTraits = setFromList [Noldor, Scout, Boon]}
-- The Angmar Awakened campaign's boons

berelind :: CardDef
berelind = (unique $ ally "AAC176" "Berelind" Neutral (StaticCost 1) 1 3 0 2) {cdCardTraits = setFromList [Dunedain, Ranger, Boon]}

coruhaer :: CardDef
coruhaer = (unique $ ally "AAC178" "Coruhaer" Neutral (StaticCost 0) 0 1 1 2) {cdCardTraits = setFromList [Dunedain, Ranger, Boon]}

laermuin :: CardDef
laermuin = (unique $ ally "AAC179" "Laermuin" Neutral (StaticCost 2) 2 2 0 4) {cdCardTraits = setFromList [Dunedain, Ranger, Boon]}

elrondsChampion :: CardDef
elrondsChampion = (ally "AAC189" "Elrond's Champion" Neutral (StaticCost 3) 2 3 1 5) {cdCardTraits = setFromList [Noldor, Warrior, Boon]}

rivendellSkirmishers :: CardDef
rivendellSkirmishers = (ally "AAC192" "Rivendell Skirmishers" Neutral (StaticCost 3) 2 2 3 3) {cdCardTraits = setFromList [Noldor, Warrior, Boon]}

halflingBounder :: CardDef
halflingBounder =
  (ally "16005" "Halfling Bounder" Lore (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Hobbit]
    }

vigilantDunadan :: CardDef
vigilantDunadan =
  (ally "16006" "Vigilant Dúnadan" Tactics (StaticCost 4) 1 3 2 3)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

andrathGuardsman :: CardDef
andrathGuardsman =
  (ally "17002" "Andrath Guardsman" Leadership (StaticCost 2) 1 0 1 2)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

kahlielsTribesman :: CardDef
kahlielsTribesman =
  (ally "17029" "Kahliel's Tribesman" Leadership (StaticCost 2) 1 1 1 2)
    { cdCardTraits = setFromList [Harad]
    }

dunedainPathfinder :: CardDef
dunedainPathfinder =
  (ally "17033" "Dúnedain Pathfinder" Spirit (StaticCost 0) 2 1 0 2)
    { cdCardTraits = setFromList [Dunedain, Scout]
    }

erynGalenSettler :: CardDef
erynGalenSettler =
  (ally "17035" "Eryn Galen Settler" Lore (StaticCost 2) 1 2 0 2)
    { cdCardTraits = setFromList [Woodman]
    }

stewardOfOrthanc :: CardDef
stewardOfOrthanc =
  (ally "17037" "Steward of Orthanc" Neutral (StaticCost 3) 2 0 0 2)
    { cdCardTraits = setFromList [Isengard]
    }

riderOfRohan :: CardDef
riderOfRohan =
  (ally "17060" "Rider of Rohan" Spirit (StaticCost 3) 2 2 0 2)
    { cdCardTraits = setFromList [Rohan, Scout]
    }

dunedainLookout :: CardDef
dunedainLookout =
  (ally "17062" "Dúnedain Lookout" Lore (StaticCost 2) 1 1 1 2)
    { cdCardTraits = setFromList [Dunedain]
    }

defenderOfCairAndros :: CardDef
defenderOfCairAndros =
  (ally "17084" "Defender of Cair Andros" Tactics (StaticCost 4) 1 1 0 3)
    { cdCardTraits = setFromList [Gondor, Warrior]
    }

southronRefugee :: CardDef
southronRefugee =
  (ally "17090" "Southron Refugee" Neutral (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Harad]
    }

knightOfDale :: CardDef
knightOfDale =
  (ally "17110" "Knight of Dale" Leadership (StaticCost 4) 2 2 1 3)
    { cdCardTraits = setFromList [Dale, Warrior]
    }

eregionSurvivor :: CardDef
eregionSurvivor =
  (ally "17114" "Eregion Survivor" Spirit (StaticCost 2) 1 1 1 3)
    { cdCardTraits = setFromList [Noldor]
    }

leaflock :: CardDef
leaflock =
  unique
    (ally "17116" "Leaflock" Lore (StaticCost 3) 0 2 2 3)
      { cdCardTraits = setFromList [Ent]
      }

halfastGamgee :: CardDef
halfastGamgee =
  unique
    (ally "17136" "Halfast Gamgee" Leadership (StaticCost 3) 2 1 0 2)
      { cdCardTraits = setFromList [Hobbit]
      }

beorningGuardian :: CardDef
beorningGuardian =
  (ally "17138" "Beorning Guardian" Tactics (StaticCost 3) 0 3 1 3)
    { cdCardTraits = setFromList [Beorning]
    }

greenwoodDefender :: CardDef
greenwoodDefender =
  (ally "19113" "Greenwood Defender" Leadership (StaticCost 3) 0 0 2 3)
    { cdCardTraits = setFromList [Silvan, Warrior]
    }

gafferGamgee :: CardDef
gafferGamgee =
  unique
    (ally "19115" "Gaffer Gamgee" Lore (StaticCost 2) 1 1 0 1)
      { cdCardTraits = setFromList [Hobbit]
      }

ereborToymaker :: CardDef
ereborToymaker =
  (ally "19119" "Erebor Toymaker" Spirit (StaticCost 3) 2 0 0 2)
    { cdCardTraits = setFromList [Dwarf, Craftsman]
    }

messengerRaven :: CardDef
messengerRaven =
  (ally "19146" "Messenger Raven" Leadership (StaticCost 1) 1 0 0 1)
    { cdCardTraits = setFromList [Creature]
    }

loyalHound :: CardDef
loyalHound =
  (ally "19148" "Loyal Hound" Lore (StaticCost 2) 1 2 0 2)
    { cdCardTraits = setFromList [Creature]
    }

bywaterShirriff :: CardDef
bywaterShirriff =
  (ally "19150" "Bywater Shirriff" Tactics (StaticCost 2) 1 1 1 2)
    { cdCardTraits = setFromList [Hobbit, Shirriff]
    }

redwaterSentry :: CardDef
redwaterSentry =
  (ally "18003" "Redwater Sentry" Leadership (StaticCost 3) 0 1 2 3)
    { cdCardTraits = setFromList [Dale, Warrior]
    }

northRealmLookout :: CardDef
northRealmLookout =
  (ally "18004" "North Realm Lookout" Spirit (StaticCost 2) 1 1 1 2)
    { cdCardTraits = setFromList [Dale, Scout]
    }

warriorOfDale :: CardDef
warriorOfDale =
  (ally "18005" "Warrior of Dale" Tactics (StaticCost 3) 0 2 1 3)
    { cdCardTraits = setFromList [Dale, Warrior]
    }

longLakeTrader :: CardDef
longLakeTrader =
  (ally "18006" "Long Lake Trader" Lore (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Dale]
    }

guardianOfEsgaroth :: CardDef
guardianOfEsgaroth =
  (ally "19002" "Guardian of Esgaroth" Leadership (StaticCost 2) 1 1 1 1)
    { cdCardTraits = setFromList [Dale]
    }

beorningSkinChanger :: CardDef
beorningSkinChanger =
  (ally "19004" "Beorning Skin-changer" Tactics (StaticCost 2) 1 1 1 3)
    { cdCardTraits = setFromList [Beorning]
    }

longLakeFisherman :: CardDef
longLakeFisherman =
  (ally "19008" "Long Lake Fisherman" Spirit (StaticCost 2) 1 1 0 2)
    { cdCardTraits = setFromList [Dale]
    }

wiglaf :: CardDef
wiglaf =
  unique
    (ally "19027" "Wiglaf" Leadership (StaticCost 5) 2 2 1 3)
      { cdCardTraits = setFromList [Dale, Scout, Warrior]
      }

meneldor :: CardDef
meneldor =
  unique
    (ally "19031" "Meneldor" Tactics (StaticCost 3) 2 2 1 2)
      { cdCardTraits = setFromList [Creature, Eagle]
      }

-- | It attaches itself as a Mount: the Wild Stallion attachment builds it from this card.
wildStallion :: CardDef
wildStallion =
  (ally "19033" "Wild Stallion" Spirit (StaticCost 2) 1 1 1 1)
    { cdCardTraits = setFromList [Creature]
    }

-- | Encounter: set aside from its owner's deck, it enters play only as Flight of the
-- Eagles shuffles it into the encounter deck and it is revealed. It has no cost. In the
-- encounter discard pile it is treated as an encounter card (MEC67 rules).
eagleOfTheNorth :: CardDef
eagleOfTheNorth =
  (ally "19035" "Eagle of the North" Neutral (StaticCost 0) 2 3 1 3)
    { cdCost = Nothing
    , cdCardTraits = setFromList [Creature, Eagle]
    , cdKeywords = setFromList [Encounter, Surge]
    , cdWhenRevealed = True
    }

forestRoadTraveler :: CardDef
forestRoadTraveler =
  (ally "19059" "Forest Road Traveler" Lore (StaticCost 2) 1 1 1 2)
    { cdCardTraits = setFromList [Woodman, Scout]
    }

giantBear :: CardDef
giantBear =
  (ally "19061" "Giant Bear" Tactics (StaticCost 5) 0 4 3 4)
    { cdCardTraits = setFromList [Creature, Beorning]
    }

galion :: CardDef
galion =
  unique
    (ally "19063" "Galion" Spirit (StaticCost 0) 0 0 0 1)
      { cdCardTraits = setFromList [Silvan]
      }

thalion :: CardDef
thalion =
  unique
    (ally "19065" "Thalion" Neutral (StaticCost 4) 2 2 2 3)
      { cdCardTraits = setFromList [Dunedain, Ranger]
      }

soldierOfErebor :: CardDef
soldierOfErebor =
  (ally "19085" "Soldier of Erebor" Leadership (StaticCost 5) 0 3 3 3)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    }

mirkwoodHunter :: CardDef
mirkwoodHunter =
  (ally "19087" "Mirkwood Hunter" Lore (StaticCost 4) 2 2 2 3)
    { cdCardTraits = setFromList [Woodman, Ranger]
    , cdKeywords = setFromList [Ranged]
    }

descendantOfGirion :: CardDef
descendantOfGirion =
  (ally "19091" "Descendant of Girion" Spirit (StaticCost 4) 2 2 2 2)
    { cdCardTraits = setFromList [Dale]
    }
