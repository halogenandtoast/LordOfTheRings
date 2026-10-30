module LordOfTheRings.Location.CardDefs (module LordOfTheRings.Location.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Keyword
import LordOfTheRings.Name (Name)
import LordOfTheRings.Prelude
import LordOfTheRings.Trait

allLocationCards :: Map CardCode CardDef
allLocationCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ greatForestWeb
      , dreadfulGap
      , fouledWell
      , goblinTunnels
      , kneesOfTheMountain
      , turbulentWaters
      , wargLair
      , eastGate
      , firstHall
      , bridgeOfKhazadDum
      , stairsOfNain
      , branchingPaths
      , lightlessPassage
      , zigilMineshaft
      , mountainsOfMirkwood
      , theBrownLands
      , theEastBight
      , necromancersPass
      , enchantedStream
      , oldForestRoad
      , forestGate
      , endlessCaverns
      , towerGate
      , banksOfTheAnduin
      , gladdenFields
      , theOldFord
      , theEavesOfMirkwood
      , riverNinglor
      , theEastBank
      , theWestBank
      , theCarrock
      , riverLangflood
      , beePastures
      , oakWoodGrove
      , rhosgobel
      , forestGrove
      , amonHen
      , amonLhaw
      , theEastWallOfRohan
      , theNorthStair
      , raurosFalls
      , theShoresOfNenHithoel
      , theOuterRidge
      , theHighlands
      , impassableBog
      , theHeartOfTheMarshes
      , fensAndMires
      , theSpidersRing
      , dryWatercourse
      , woodmansGlade
      , woodElfPath
      , theMountainsRoots
      , plunderedArmoury
      , upperHall
      , abandonedMine
      , dwarvenForge
      , silentCaverns
      , twistingPassage
      , ruinedRoad
      , goblinGate
      , pathlessCountry
      , barrenHills
      , caradhras
      , fanuidhol
      , celebdil
      , theDimrillStair
      , rockyCrags
      , drownedTreasury
      , mithrilLode
      , darkPit
      , secondHall
      , secondDeep
      , doorsOfDurin
      , stairFalls
      , perilousSwamp
      , makeshiftPassage
      , stagnantCreek
      , theCitadel
      , theApproach
      , theBanks
      , orcWarCamp
      , ithilienRoad
      , overgrownTrail
      , secludedGlade
      , theLeapingFish
      , pelargirDocks
      , harborStorehouse
      , cityStreet
      , marketSquare
      , burningFarmhouse
      , gondorianHamlet
      , secludedFarmhouse
      , ancestralClearing
      , gardenOfPoisons
      , gladeOfCleansing
      , morgulRoad
      , morgulVale
      , theWhiteBridge
      , sewers
      , rootsOfMindolluin
      , storehouse
      , housesOfTheDead
      , theFourthStar
      , westGate
      , theKingsLibrary
      , ancientHarbor
      , theOldBridge
      , westQuarter
      , eastQuarter
      , ruinedSquare
      , ruinedTower
      , theCrossroads
      , southernRoad
      , westernRoad
      , northernRoad
      , easternRoad
      , theDarkWoods
      , methedras
      , orcCave
      , mugashsLair
      , edgeOfFangorn
      , heartOfFangorn
      , theIslet
      , fordsOfIsen
      , theKingsRoad
      , gapOfRohan
      , ancientForestLocation
      , tangledWoods
      , brokenLandsLocation
      , ravenChiefsCamp
      , altarOfMidwinter
      , sinisterDungeon
      , tombsOfCarnDum
      , darkHalls
      , unholyCrypt
      , ravenWarCamp
      , ravenVillage
      , dunlandBattlefield
      , ravenCountry
      , munuvDuvRavine
      , oldSouthRoad
      , hithaeglirFoothills
      , hillsOfDunland
      , plainsOfEnedwaith
      , hithaeglirStream
      , theEmptyMug
      , theCrossingAtTharbad
      , streetsOfTharbad
      , tharbadHideout
      , ruinsOfTheSecondAge
      , seedyInn
      , hiddenAlleyway
      , decrepitRooftops
      , theSecretChamber
      , ruinedPlaza
      , collapsedTower
      , ancientFoundation
      , cityRemains
      , fenOfReeds
      , fingerOfGlanduin
      , hiddenEyot
      , sinkingBog
      , hillBarrow
      , caveBarrow
      , stoneBarrow
      , hallowedCircle
      , cursedForest
      , grimFoothills
      , accursedBattlefield
      , mountainsOfAngmar
      , blightOfCarnDum
      , fortressWalls
      , eriadorWastes
      , wargsDen
      , northDowns
      , southernGate
      , prisonCell
      , patrolRoom
      , tunnelsOfMountGram
      , chetwoodForest
      , bordersOfBreeLand
      , outlyingHomestead
      , ruggedCountry
      , shroudedHills
      , amonForn
      , weatheredHilltop
      , exposedRidge
      , shelteredValley
      , concealedOrcCamp
      , secludedCave
      , theHoarwell
      , abandonedCamp
      , patchOfWoods
      , trollFells
      , barrenMoorland
      , brokenBattlements
      , norburyTombs
      , fornostSquare
      , hauntedKeep
      , deadmensGate
      , ruinsOfArnor
      , ancientCauseway
      , theGreatHall
      , forbiddenDescent
      , eerieHalls
      , ghostlyRuins
      , decrepitRemains
      , starlitSea
      , fogBank
      , calmWaters
      , rollingSeas
      , roughWaters
      , waterspout
      , lostIsland
      , shrineToMorgoth
      , lushJungle
      , forbiddenCoast
      , steepPlateau
      , floodedRuins
      , drownedGraves
      , cursedTemple
      , helmOfTheStormcaller
      , forecastleDeck
      , weatherDeck
      , cargoHold
      , crewQuarters
      , quarterDeck
      ]

drownedTreasury :: CardDef
drownedTreasury =
  (location "04121" "Drowned Treasury" FoundationsOfStone 3 2 3)
    { cdCardTraits = setFromList [Underground, Underwater]
    }

mithrilLode :: CardDef
mithrilLode =
  (location "04122" "Mithril Lode" FoundationsOfStone 1 2 5)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Doomed 1]
    }

eastGate :: CardDef
eastGate =
  (location "03016" "East-gate" IntoThePit 1 7 7)
    { cdCardTraits = setFromList [Gate]
    , cdVictoryPoints = Just 1
    }

firstHall :: CardDef
firstHall =
  (location "03017" "First Hall" IntoThePit 1 2 2)
    { cdCardTraits = setFromList [Underground]
    , cdVictoryPoints = Just 1
    }

bridgeOfKhazadDum :: CardDef
bridgeOfKhazadDum =
  (location "03018" "Bridge of Khazad-dûm" IntoThePit 1 3 3)
    { cdCardTraits = setFromList [Underground, Bridge]
    , cdVictoryPoints = Just 2
    }

stairsOfNain :: CardDef
stairsOfNain =
  (location "03019" "Stairs of Náin" IntoThePit 2 2 4)
    { cdCardTraits = setFromList [Underground]
    , cdShadowEffect = True
    }

branchingPaths :: CardDef
branchingPaths =
  (location "03036" "Branching Paths" TwistsAndTurns 3 1 3)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdEasyModeQuantity = Just 1
    }

lightlessPassage :: CardDef
lightlessPassage =
  (location "03037" "Lightless Passage" TwistsAndTurns 2 4 4)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

zigilMineshaft :: CardDef
zigilMineshaft =
  (location "03038" "Zigil Mineshaft" TwistsAndTurns 3 5 5)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdEasyModeQuantity = Just 1
    }

-- | X quest points: the number of characters in play (see the card).
dreadfulGap :: CardDef
dreadfulGap =
  (location "03052" "Dreadful Gap" HazardsOfThePit 1 2 0)
    { cdCardTraits = setFromList [Underground, Hazard]
    , cdVictoryPoints = Just 3
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 0
    }

fouledWell :: CardDef
fouledWell =
  (location "03053" "Fouled Well" HazardsOfThePit 3 3 5)
    { cdCardTraits = setFromList [Underground, Dark, Hazard]
    , cdWhenRevealed = True
    }

kneesOfTheMountain :: CardDef
kneesOfTheMountain =
  (location "03057" "Knees of the Mountain" MistyMountains 1 2 3)
    { cdCardTraits = setFromList [Mountain]
    }

turbulentWaters :: CardDef
turbulentWaters =
  (location "03058" "Turbulent Waters" MistyMountains 2 3 2)
    { cdCardTraits = setFromList [Mountain]
    , cdEasyModeQuantity = Just 1
    }

wargLair :: CardDef
wargLair =
  (location "03059" "Warg Lair" MistyMountains 2 1 3)
    { cdCardTraits = setFromList [Mountain]
    , cdWhenRevealed = True
    }

goblinTunnels :: CardDef
goblinTunnels =
  (location "03063" "Goblin Tunnels" GoblinsOfTheDeep 2 2 7)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdShadowEffect = True
    }

greatForestWeb :: CardDef
greatForestWeb =
  (location "01077" "Great Forest Web" SpidersOfMirkwood 2 2 2)
    { cdCardTraits = setFromList [Forest]
    }

mountainsOfMirkwood :: CardDef
mountainsOfMirkwood =
  (location "01078" "Mountains of Mirkwood" SpidersOfMirkwood 3 2 3)
    { cdCardTraits = setFromList [Forest, Mountain]
    }

theBrownLands :: CardDef
theBrownLands =
  (location "01087" "The Brown Lands" Wilderlands 2 5 1)
    { cdCardTraits = setFromList [Wasteland]
    }

theEastBight :: CardDef
theEastBight =
  (location "01088" "The East Bight" Wilderlands 2 1 6)
    { cdCardTraits = setFromList [Wasteland]
    }

necromancersPass :: CardDef
necromancersPass =
  (location "01094" "Necromancer's Pass" DolGuldurOrcs 2 3 2)
    { cdCardTraits = setFromList [DolGuldur, Stronghold]
    , cdEasyModeQuantity = Just 1
    }

enchantedStream :: CardDef
enchantedStream =
  (location "01095" "Enchanted Stream" DolGuldurOrcs 2 2 2)
    { cdCardTraits = setFromList [Forest]
    }

oldForestRoad :: CardDef
oldForestRoad =
  (location "01099" "Old Forest Road" PassageThroughMirkwood 2 1 3)
    { cdCardTraits = setFromList [Forest]
    }

forestGate :: CardDef
forestGate =
  (location "01100" "Forest Gate" PassageThroughMirkwood 2 2 4)
    { cdCardTraits = setFromList [Forest]
    }

endlessCaverns :: CardDef
endlessCaverns =
  (location "01106" "Endless Caverns" EscapeFromDolGuldur 2 1 3)
    { cdCardTraits = setFromList [Dungeon]
    , cdKeywords = setFromList [Doomed 1, Surge]
    }

towerGate :: CardDef
towerGate =
  (location "01107" "Tower Gate" EscapeFromDolGuldur 2 2 1)
    { cdCardTraits = setFromList [Dungeon]
    }

banksOfTheAnduin :: CardDef
banksOfTheAnduin =
  (location "01113" "Banks of the Anduin" JourneyAlongTheAnduin 2 1 3)
    { cdCardTraits = setFromList [Riverland]
    }

gladdenFields :: CardDef
gladdenFields =
  (location "01114" "Gladden Fields" JourneyAlongTheAnduin 3 3 3)
    { cdCardTraits = setFromList [Marshland]
    , cdVictoryPoints = Just 3
    , cdEasyModeQuantity = Just 1
    }

theOldFord :: CardDef
theOldFord =
  (location "02015" "The Old Ford" TheHuntForGollum 2 0 2)
    { cdCardTraits = setFromList [Riverland]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

theEavesOfMirkwood :: CardDef
theEavesOfMirkwood =
  (location "02016" "The Eaves of Mirkwood" TheHuntForGollum 3 2 2)
    { cdCardTraits = setFromList [Forest]
    }

riverNinglor :: CardDef
riverNinglor =
  (location "02017" "River Ninglor" TheHuntForGollum 2 2 4)
    { cdCardTraits = setFromList [Riverland]
    , cdShadowEffect = True
    }

theEastBank :: CardDef
theEastBank =
  (location "02018" "The East Bank" TheHuntForGollum 2 3 3)
    { cdCardTraits = setFromList [Riverland]
    , cdShadowEffect = True
    }

theWestBank :: CardDef
theWestBank =
  (location "02019" "The West Bank" TheHuntForGollum 2 3 3)
    { cdCardTraits = setFromList [Riverland]
    , cdShadowEffect = True
    }

theCarrock :: CardDef
theCarrock =
  (location "02043" "The Carrock" ConflictAtTheCarrock 1 2 6)
    { cdUnique = True
    , cdCardTraits = setFromList [Riverland]
    }

riverLangflood :: CardDef
riverLangflood =
  (location "02044" "River Langflood" ConflictAtTheCarrock 4 2 3)
    { cdCardTraits = setFromList [Riverland]
    , cdEasyModeQuantity = Just 2
    }

beePastures :: CardDef
beePastures = location "02045" "Bee Pastures" ConflictAtTheCarrock 3 1 2

oakWoodGrove :: CardDef
oakWoodGrove =
  (location "02046" "Oak-wood Grove" ConflictAtTheCarrock 3 2 1)
    { cdCardTraits = setFromList [Forest]
    }

rhosgobel :: CardDef
rhosgobel =
  (location "02065" "Rhosgobel" AJourneyToRhosgobel 1 0 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Forest]
    , cdVictoryPoints = Just 4
    }

forestGrove :: CardDef
forestGrove =
  (location "02066" "Forest Grove" AJourneyToRhosgobel 4 2 3)
    { cdCardTraits = setFromList [Forest]
    }

amonHen :: CardDef
amonHen =
  (location "02083" "Amon Hen" TheHillsOfEmynMuil 1 0 5)
    { cdUnique = True
    , cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 5
    }

amonLhaw :: CardDef
amonLhaw =
  (location "02084" "Amon Lhaw" TheHillsOfEmynMuil 1 0 5)
    { cdUnique = True
    , cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 5
    }

theEastWallOfRohan :: CardDef
theEastWallOfRohan =
  (location "02085" "The East Wall of Rohan" TheHillsOfEmynMuil 2 4 2)
    { cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 3
    , cdEasyModeQuantity = Just 0
    }

theNorthStair :: CardDef
theNorthStair =
  (location "02086" "The North Stair" TheHillsOfEmynMuil 2 3 3)
    { cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 3
    }

raurosFalls :: CardDef
raurosFalls =
  (location "02087" "Rauros Falls" TheHillsOfEmynMuil 2 2 4)
    { cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 3
    , cdShadowEffect = True
    }

theShoresOfNenHithoel :: CardDef
theShoresOfNenHithoel =
  (location "02088" "The Shores of Nen Hithoel" TheHillsOfEmynMuil 3 2 2)
    { cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 2
    , cdShadowEffect = True
    }

theOuterRidge :: CardDef
theOuterRidge =
  (location "02089" "The Outer Ridge" TheHillsOfEmynMuil 3 2 2)
    { cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 2
    , cdShadowEffect = True
    }

theHighlands :: CardDef
theHighlands =
  (location "02090" "The Highlands" TheHillsOfEmynMuil 4 1 1)
    { cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 1
    }

impassableBog :: CardDef
impassableBog =
  (location "02113" "Impassable Bog" TheDeadMarshes 4 1 12)
    { cdCardTraits = setFromList [DeadMarshes]
    , cdVictoryPoints = Just 7
    , cdEscape = Just 2
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 2
    }

theHeartOfTheMarshes :: CardDef
theHeartOfTheMarshes =
  (location "02114" "The Heart of the Marshes" TheDeadMarshes 4 3 4)
    { cdCardTraits = setFromList [DeadMarshes]
    , cdEscape = Just 1
    , cdEasyModeQuantity = Just 2
    }

fensAndMires :: CardDef
fensAndMires =
  (location "02115" "Fens and Mires" TheDeadMarshes 4 2 2)
    { cdCardTraits = setFromList [DeadMarshes]
    , cdEscape = Just 2
    }

theSpidersRing :: CardDef
theSpidersRing =
  (location "02131" "The Spider's Ring" ReturnToMirkwood 4 3 2)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

dryWatercourse :: CardDef
dryWatercourse =
  (location "02132" "Dry Watercourse" ReturnToMirkwood 3 2 2)
    { cdCardTraits = setFromList [Forest]
    }

woodmansGlade :: CardDef
woodmansGlade =
  (location "02133" "Woodman's Glade" ReturnToMirkwood 3 2 2)
    { cdCardTraits = setFromList [Forest]
    }

woodElfPath :: CardDef
woodElfPath =
  (location "02134" "Wood Elf Path" ReturnToMirkwood 3 1 3)
    { cdCardTraits = setFromList [Forest]
    }

plunderedArmoury :: CardDef
plunderedArmoury =
  (location "03034" "Plundered Armoury" PlunderingGoblins 2 3 2)
    { cdCardTraits = setFromList [Underground]
    }

upperHall :: CardDef
upperHall =
  (location "03023" "Upper Hall" TheSeventhLevel 3 3 4)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Doomed 2]
    }

-- | X is the number of players in the game.
theMountainsRoots :: CardDef
theMountainsRoots =
  (location "03047" "The Mountains' Roots" DeepsOfMoria 3 0 0)
    { cdCardTraits = setFromList [Underground]
    , cdShadowEffect = True
    }

abandonedMine :: CardDef
abandonedMine =
  (location "04089" "Abandoned Mine" TheLongDark 3 3 3)
    { cdCardTraits = setFromList [Underground, Dark]
    }

dwarvenForge :: CardDef
dwarvenForge =
  (location "04090" "Dwarven Forge" TheLongDark 2 2 4)
    { cdCardTraits = setFromList [Underground, Dark]
    }

silentCaverns :: CardDef
silentCaverns =
  (location "04091" "Silent Caverns" TheLongDark 2 1 3)
    { cdCardTraits = setFromList [Underground]
    }

twistingPassage :: CardDef
twistingPassage =
  (location "04092" "Twisting Passage" TheLongDark 2 3 5)
    { cdCardTraits = setFromList [Underground, Dark]
    }


ruinedRoad :: CardDef
ruinedRoad =
  (location "04042" "Ruined Road" RoadToRivendell 2 1 5)
    { cdCardTraits = setFromList [Road]
    , cdShadowEffect = True
    }

goblinGate :: CardDef
goblinGate =
  (location "04043" "Goblin Gate" RoadToRivendell 1 5 4)
    { cdCardTraits = setFromList [Gate]
    }

pathlessCountry :: CardDef
pathlessCountry =
  (location "04044" "Pathless Country" RoadToRivendell 3 3 5)
    { cdCardTraits = setFromList [Hills]
    , cdShadowEffect = True
    }

barrenHills :: CardDef
barrenHills =
  (location "04045" "Barren Hills" RoadToRivendell 2 2 4)
    { cdCardTraits = setFromList [Hills]
    , cdShadowEffect = True
    }


caradhras :: CardDef
caradhras =
  (location "04015" "Caradhras" TheRedhornGate 1 3 9)
    { cdCardTraits = setFromList [Mountain, Snow]
    , cdUnique = True
    , cdVictoryPoints = Just 3
    }

fanuidhol :: CardDef
fanuidhol =
  (location "04016" "Fanuidhol" TheRedhornGate 1 3 7)
    { cdCardTraits = setFromList [Mountain, Snow]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

celebdil :: CardDef
celebdil =
  (location "04017" "Celebdil" TheRedhornGate 1 3 7)
    { cdCardTraits = setFromList [Mountain, Snow]
    , cdUnique = True
    , cdVictoryPoints = Just 2
    }

theDimrillStair :: CardDef
theDimrillStair =
  (location "04018" "The Dimrill Stair" TheRedhornGate 1 2 3)
    { cdCardTraits = setFromList [Stair]
    , cdUnique = True
    , cdVictoryPoints = Just 1
    }

rockyCrags :: CardDef
rockyCrags =
  (location "04019" "Rocky Crags" TheRedhornGate 3 4 2)
    { cdCardTraits = setFromList [Mountain]
    , cdShadowEffect = True
    }


darkPit :: CardDef
darkPit = (location "04141" "Dark Pit" ShadowAndFlame 1 0 11) {cdCardTraits = setFromList [Underground, Dark]}

secondHall :: CardDef
secondHall =
  (location "04142" "Second Hall" ShadowAndFlame 1 4 2)
    { cdCardTraits = setFromList [Underground]
    , cdUnique = True
    }

secondDeep :: CardDef
secondDeep = (location "04143" "Second Deep" ShadowAndFlame 3 3 3) {cdCardTraits = setFromList [Underground]}

doorsOfDurin :: CardDef
doorsOfDurin =
  (location "04065" "Doors of Durin" TheWatcherInTheWater 1 2 0)
    { cdUnique = True
    , cdCardTraits = setFromList [Gate]
    , cdVictoryPoints = Just 3
    }

stairFalls :: CardDef
stairFalls =
  (location "04066" "Stair Falls" TheWatcherInTheWater 1 2 4)
    { cdUnique = True
    , cdCardTraits = setFromList [Stair]
    , cdShadowEffect = True
    }

perilousSwamp :: CardDef
perilousSwamp =
  (location "04067" "Perilous Swamp" TheWatcherInTheWater 2 4 2)
    { cdCardTraits = setFromList [Swamp]
    , cdShadowEffect = True
    }

makeshiftPassage :: CardDef
makeshiftPassage =
  (location "04068" "Makeshift Passage" TheWatcherInTheWater 2 1 5)
    { cdCardTraits = setFromList [Swamp]
    }

stagnantCreek :: CardDef
stagnantCreek =
  (location "04069" "Stagnant Creek" TheWatcherInTheWater 3 3 3)
    { cdCardTraits = setFromList [Swamp]
    , cdWhenRevealed = True
    }

theCitadel :: CardDef
theCitadel =
  (location "05033" "The Citadel" TheSiegeOfCairAndros 1 3 11)
    { cdCardTraits = setFromList [CairAndros, Battleground]
    , cdVictoryPoints = Just 3
    }

theApproach :: CardDef
theApproach =
  (location "05034" "The Approach" TheSiegeOfCairAndros 1 2 7)
    { cdCardTraits = setFromList [CairAndros, Battleground]
    , cdVictoryPoints = Just 2
    }

theBanks :: CardDef
theBanks =
  (location "05035" "The Banks" TheSiegeOfCairAndros 1 1 3)
    { cdCardTraits = setFromList [CairAndros, Battleground]
    , cdVictoryPoints = Just 1
    }

orcWarCamp :: CardDef
orcWarCamp =
  (location "05059" "Orc War Camp" MordorElite 2 5 2)
    { cdCardTraits = setFromList [Mordor]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

ithilienRoad :: CardDef
ithilienRoad =
  (location "05028" "Ithilien Road" IntoIthilien 2 4 4)
    { cdCardTraits = setFromList [Forest, Road]
    , cdShadowEffect = True
    }

overgrownTrail :: CardDef
overgrownTrail =
  (location "05046" "Overgrown Trail" BroodingForest 3 4 6)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

secludedGlade :: CardDef
secludedGlade =
  (location "05047" "Secluded Glade" BroodingForest 3 3 3)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

theLeapingFish :: CardDef
theLeapingFish =
  (location "05021" "The Leaping Fish" PerilInPelargir 1 2 6)
    { cdUnique = True
    , cdCardTraits = setFromList [City]
    , cdVictoryPoints = Just 3
    }

pelargirDocks :: CardDef
pelargirDocks =
  (location "05022" "Pelargir Docks" PerilInPelargir 2 4 3)
    { cdCardTraits = setFromList [City, River]
    , cdEasyModeQuantity = Just 0
    }

harborStorehouse :: CardDef
harborStorehouse =
  (location "05023" "Harbor Storehouse" PerilInPelargir 2 1 4)
    { cdCardTraits = setFromList [City]
    , cdShadowEffect = True
    }

cityStreet :: CardDef
cityStreet =
  (location "05038" "City Street" StreetsOfGondor 3 2 2)
    { cdCardTraits = setFromList [City]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

marketSquare :: CardDef
marketSquare =
  (location "05039" "Market Square" StreetsOfGondor 2 3 1)
    { cdCardTraits = setFromList [City]
    , cdShadowEffect = True
    }

-- | Villagers 4: its villager tokens are placed as it is built.
burningFarmhouse :: CardDef
burningFarmhouse =
  (location "06075" "Burning Farmhouse" EncounterAtAmonDin 4 1 5)
    { cdCardTraits = setFromList [Gondor]
    , cdVictoryPoints = Just 1
    }

-- | Villagers 3.
gondorianHamlet :: CardDef
gondorianHamlet =
  (location "06076" "Gondorian Hamlet" EncounterAtAmonDin 4 2 4)
    { cdCardTraits = setFromList [Gondor]
    , cdVictoryPoints = Just 1
    }

-- | Villagers 2.
secludedFarmhouse :: CardDef
secludedFarmhouse =
  (location "06077" "Secluded Farmhouse" EncounterAtAmonDin 3 3 3)
    { cdCardTraits = setFromList [Gondor]
    , cdVictoryPoints = Just 1
    }

ancestralClearing :: CardDef
ancestralClearing =
  (location "06050" "Ancestral Clearing" TheDruadanForest 1 4 3)
    { cdCardTraits = setFromList [Forest]
    , cdEasyModeQuantity = Just 0
    }

gardenOfPoisons :: CardDef
gardenOfPoisons =
  (location "06051" "Garden of Poisons" TheDruadanForest 3 2 3)
    { cdCardTraits = setFromList [Forest]
    }

-- | X threat, X the archery of the highest archery Wose enemy: from its own modifiers.
gladeOfCleansing :: CardDef
gladeOfCleansing =
  (location "06052" "Glade of Cleansing" TheDruadanForest 3 0 5)
    { cdCardTraits = setFromList [Forest]
    , cdEasyModeQuantity = Just 1
    }

morgulRoad :: CardDef
morgulRoad =
  (location "06154" "Morgul Road" TheMorgulVale 3 2 5)
    { cdCardTraits = setFromList [Road]
    , cdShadowEffect = True
    }

morgulVale :: CardDef
morgulVale =
  (location "06155" "Morgul Vale" TheMorgulVale 3 3 3)
    { cdCardTraits = setFromList [Valley]
    , cdShadowEffect = True
    }

-- | X threat and X quest points, X the progress on To the Tower.
theWhiteBridge :: CardDef
theWhiteBridge =
  unique
    (location "06156" "The White Bridge" TheMorgulVale 1 0 0)
      { cdCardTraits = setFromList [Bridge]
      }

sewers :: CardDef
sewers =
  (location "06018" "Sewers" TheStewardsFear 3 3 3)
    { cdCardTraits = setFromList [Underworld, City]
    , cdWhenRevealed = True
    }

rootsOfMindolluin :: CardDef
rootsOfMindolluin =
  unique
    (location "06019" "Roots of Mindolluin" TheStewardsFear 1 2 5)
      { cdCardTraits = setFromList [Underworld]
      }

storehouse :: CardDef
storehouse =
  (location "06020" "Storehouse" TheStewardsFear 2 2 2)
    { cdCardTraits = setFromList [Underworld, City]
    , cdShadowEffect = True
    }

housesOfTheDead :: CardDef
housesOfTheDead =
  unique
    (location "06021" "Houses of the Dead" TheStewardsFear 1 4 4)
      { cdCardTraits = setFromList [Underworld, City]
      , cdEasyModeQuantity = Just 0
      }

theFourthStar :: CardDef
theFourthStar =
  unique
    (location "06022" "The Fourth Star" TheStewardsFear 1 3 5)
      { cdCardTraits = setFromList [City]
      }

westGate :: CardDef
westGate =
  (location "06096" "West Gate" AssaultOnOsgiliath 1 2 5)
    { cdCardTraits = setFromList [Osgiliath]
    , cdUnique = True
    }

theKingsLibrary :: CardDef
theKingsLibrary =
  (location "06097" "The King's Library" AssaultOnOsgiliath 1 2 3)
    { cdCardTraits = setFromList [Osgiliath]
    , cdUnique = True
    }

ancientHarbor :: CardDef
ancientHarbor =
  (location "06098" "Ancient Harbor" AssaultOnOsgiliath 1 1 5)
    { cdCardTraits = setFromList [Osgiliath]
    , cdUnique = True
    }

theOldBridge :: CardDef
theOldBridge =
  (location "06099" "The Old Bridge" AssaultOnOsgiliath 1 1 6)
    { cdCardTraits = setFromList [Osgiliath]
    , cdUnique = True
    }

westQuarter :: CardDef
westQuarter =
  (location "06100" "West Quarter" AssaultOnOsgiliath 2 2 3)
    { cdCardTraits = setFromList [Osgiliath]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

eastQuarter :: CardDef
eastQuarter =
  (location "06101" "East Quarter" AssaultOnOsgiliath 2 2 3)
    { cdCardTraits = setFromList [Osgiliath]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

ruinedSquare :: CardDef
ruinedSquare =
  (location "06102" "Ruined Square" AssaultOnOsgiliath 3 1 2)
    { cdCardTraits = setFromList [Osgiliath]
    , cdKeywords = setFromList [Surge]
    }

ruinedTower :: CardDef
ruinedTower =
  (location "06103" "Ruined Tower" AssaultOnOsgiliath 3 2 2)
    { cdCardTraits = setFromList [Osgiliath]
    }

-- | X threat, X the number of players: given by its own modifiers.
theCrossroads :: CardDef
theCrossroads =
  unique
    (location "06123" "The Cross-roads" TheBloodOfGondor 1 0 2)
      { cdCardTraits = setFromList [Ithilien]
      , cdVictoryPoints = Just 3
      }

southernRoad :: CardDef
southernRoad = (location "06124" "Southern Road" TheBloodOfGondor 1 2 4) {cdCardTraits = setFromList [Ithilien, Road]}

westernRoad :: CardDef
westernRoad = (location "06125" "Western Road" TheBloodOfGondor 1 2 4) {cdCardTraits = setFromList [Ithilien, Road]}

northernRoad :: CardDef
northernRoad = (location "06126" "Northern Road" TheBloodOfGondor 1 4 3) {cdCardTraits = setFromList [Ithilien, Road]}

easternRoad :: CardDef
easternRoad = (location "06127" "Eastern Road" TheBloodOfGondor 1 4 2) {cdCardTraits = setFromList [Ithilien, Road]}

-- | Archery X, X the number of players: gained from its own modifiers.
theDarkWoods :: CardDef
theDarkWoods = (location "06128" "The Dark Woods" TheBloodOfGondor 4 2 2) {cdCardTraits = setFromList [Forest]}

methedras :: CardDef
methedras =
  (location "07027" "Methedras" ToCatchAnOrc 4 2 3)
    { cdCardTraits = setFromList [Mountain]
    }

orcCave :: CardDef
orcCave =
  (location "07028" "Orc Cave" ToCatchAnOrc 3 3 4)
    { cdCardTraits = setFromList [Mountain, Cave]
    }

mugashsLair :: CardDef
mugashsLair =
  (location "07029" "Mugash's Lair" ToCatchAnOrc 2 4 2)
    { cdCardTraits = setFromList [Mountain, Cave]
    , cdShadowEffect = True
    }

edgeOfFangorn :: CardDef
edgeOfFangorn =
  (location "07032" "Edge of Fangorn" IntoFangorn 1 2 2)
    { cdCardTraits = setFromList [Forest]
    , cdVictoryPoints = Just 1
    }

heartOfFangorn :: CardDef
heartOfFangorn =
  (location "07036" "Heart of Fangorn" IntoFangorn 3 3 3)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

theIslet :: CardDef
theIslet = unique (location "07017" "The Islet" TheFordsOfIsen 1 1 1) {cdCardTraits = setFromList [River]}

fordsOfIsen :: CardDef
fordsOfIsen = (location "07018" "Fords of Isen" TheFordsOfIsen 3 3 4) {cdCardTraits = setFromList [River]}

theKingsRoad :: CardDef
theKingsRoad = (location "07019" "The King's Road" TheFordsOfIsen 2 2 2) {cdCardTraits = setFromList [Road]}

gapOfRohan :: CardDef
gapOfRohan =
  (location "07020" "Gap of Rohan" TheFordsOfIsen 4 2 3)
    { cdCardTraits = setFromList [Valley]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

ancientForestLocation :: CardDef
ancientForestLocation =
  (location "07050" "Ancient Forest" AncientForest 3 2 5)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

tangledWoods :: CardDef
tangledWoods = (location "07051" "Tangled Woods" AncientForest 3 2 4) {cdCardTraits = setFromList [Forest]}

brokenLandsLocation :: CardDef
brokenLandsLocation =
  (location "07054" "Broken Lands" BrokenLands 3 2 6)
    { cdCardTraits = setFromList [Highlands]
    , cdShadowEffect = True
    }

ravenChiefsCamp :: CardDef
ravenChiefsCamp =
  unique
    (location "08152" "Raven Chief's Camp" TheAntleredCrown 1 4 9)
      { cdCardTraits = setFromList [Dunland]
      , cdKeywords = setFromList [Time 3]
      , cdVictoryPoints = Just 3
      }

ravenWarCamp :: CardDef
ravenWarCamp =
  (location "08155" "Raven War-camp" TheAntleredCrown 4 2 4)
    { cdCardTraits = setFromList [Dunland]
    , cdKeywords = setFromList [Time 3]
    , cdEasyModeQuantity = Just 2
    }

ravenVillage :: CardDef
ravenVillage =
  (location "08156" "Raven Village" TheAntleredCrown 3 2 3)
    { cdCardTraits = setFromList [Dunland]
    , cdKeywords = setFromList [Time 2]
    , cdShadowEffect = True
    }

dunlandBattlefield :: CardDef
dunlandBattlefield =
  (location "08157" "Dunland Battlefield" TheAntleredCrown 3 3 5)
    { cdCardTraits = setFromList [Dunland]
    , cdKeywords = setFromList [Time 3]
    , cdShadowEffect = True
    }

ravenCountry :: CardDef
ravenCountry =
  (location "08158" "Raven Country" TheAntleredCrown 2 3 6)
    { cdCardTraits = setFromList [Dunland]
    , cdKeywords = setFromList [Time 4]
    , cdShadowEffect = True
    }

munuvDuvRavine :: CardDef
munuvDuvRavine = unique (location "08015" "Munuv Dûv Ravine" TheDunlandTrap 1 4 6) {cdCardTraits = setFromList [Enedwaith, Hills, River]}

oldSouthRoad :: CardDef
oldSouthRoad = unique (location "08016" "Old South Road" TheDunlandTrap 1 1 4) {cdCardTraits = setFromList [Road]}

-- | X threat, X the resource tokens on it: given by its own modifiers.
hithaeglirFoothills :: CardDef
hithaeglirFoothills =
  (location "08019" "Hithaeglir Foothills" TheDunlandTrap 2 0 4)
    { cdCardTraits = setFromList [Enedwaith, Hills]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

hillsOfDunland :: CardDef
hillsOfDunland =
  (location "08020" "Hills of Dunland" TheDunlandTrap 4 2 3)
    { cdCardTraits = setFromList [Enedwaith, Hills]
    , cdEasyModeQuantity = Just 2
    }

plainsOfEnedwaith :: CardDef
plainsOfEnedwaith =
  (location "08021" "Plains of Enedwaith" TheDunlandTrap 3 2 2)
    { cdCardTraits = setFromList [Enedwaith, Plains]
    , cdShadowEffect = True
    }

hithaeglirStream :: CardDef
hithaeglirStream =
  (location "08022" "Hithaeglir Stream" TheDunlandTrap 4 3 4)
    { cdCardTraits = setFromList [Enedwaith, River]
    , cdShadowEffect = True
    }

theEmptyMug :: CardDef
theEmptyMug =
  unique
    (location "08069" "The Empty Mug" TroubleInTharbad 1 1 4)
      { cdCardTraits = setFromList [City]
      , cdVictoryPoints = Just 4
      }

-- | It gets +2 quest points for each player.
theCrossingAtTharbad :: CardDef
theCrossingAtTharbad =
  unique
    (location "08071" "The Crossing at Tharbad" TroubleInTharbad 1 2 10)
      { cdCardTraits = setFromList [City, Ruins, River]
      }

streetsOfTharbad :: CardDef
streetsOfTharbad =
  (location "08074" "Streets of Tharbad" TroubleInTharbad 2 3 1)
    { cdCardTraits = setFromList [City, Road]
    }

tharbadHideout :: CardDef
tharbadHideout =
  (location "08075" "Tharbad Hideout" TroubleInTharbad 2 1 1)
    { cdCardTraits = setFromList [City]
    , cdKeywords = setFromList [Surge]
    }

ruinsOfTheSecondAge :: CardDef
ruinsOfTheSecondAge =
  (location "08076" "Ruins of the Second Age" TroubleInTharbad 2 1 5)
    { cdCardTraits = setFromList [City, Ruins]
    , cdEasyModeQuantity = Just 1
    }

seedyInn :: CardDef
seedyInn =
  (location "08077" "Seedy Inn" TroubleInTharbad 2 3 3)
    { cdCardTraits = setFromList [City]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

hiddenAlleyway :: CardDef
hiddenAlleyway =
  (location "08078" "Hidden Alleyway" TroubleInTharbad 2 3 5)
    { cdCardTraits = setFromList [City]
    }

decrepitRooftops :: CardDef
decrepitRooftops =
  (location "08079" "Decrepit Rooftops" TroubleInTharbad 2 2 3)
    { cdCardTraits = setFromList [City]
    }

-- | X threat, X the number of players: given by its own modifiers.
theSecretChamber :: CardDef
theSecretChamber =
  unique
    (location "08126" "The Secret Chamber" CelebrimborsSecret 1 0 6)
      { cdCardTraits = setFromList [OstInEdhil, Ruins]
      , cdVictoryPoints = Just 3
      }

ruinedPlaza :: CardDef
ruinedPlaza =
  (location "08130" "Ruined Plaza" CelebrimborsSecret 3 2 2)
    { cdCardTraits = setFromList [OstInEdhil, Ruins]
    , cdShadowEffect = True
    }

collapsedTower :: CardDef
collapsedTower =
  (location "08131" "Collapsed Tower" CelebrimborsSecret 2 1 4)
    { cdCardTraits = setFromList [OstInEdhil, Ruins]
    , cdKeywords = setFromList [Surge, Scour]
    , cdShadowEffect = True
    }

ancientFoundation :: CardDef
ancientFoundation =
  (location "08132" "Ancient Foundation" CelebrimborsSecret 4 2 4)
    { cdCardTraits = setFromList [OstInEdhil, Ruins]
    , cdKeywords = setFromList [Scour]
    }

cityRemains :: CardDef
cityRemains =
  (location "08133" "City Remains" CelebrimborsSecret 4 3 3)
    { cdCardTraits = setFromList [OstInEdhil, Ruins]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

fenOfReeds :: CardDef
fenOfReeds =
  (location "08106" "Fen of Reeds" TheNinInEilph 3 2 3)
    { cdCardTraits = setFromList [Marsh]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

fingerOfGlanduin :: CardDef
fingerOfGlanduin =
  (location "08107" "Finger of Glanduin" TheNinInEilph 3 3 3)
    { cdCardTraits = setFromList [River, Marsh]
    , cdShadowEffect = True
    }

hiddenEyot :: CardDef
hiddenEyot = (location "08108" "Hidden Eyot" TheNinInEilph 2 3 4) {cdCardTraits = setFromList [Marsh]}

sinkingBog :: CardDef
sinkingBog =
  (location "08109" "Sinking Bog" TheNinInEilph 3 1 5)
    { cdCardTraits = setFromList [Marsh]
    , cdShadowEffect = True
    }

hillBarrow :: CardDef
hillBarrow = (location "08046" "Hill Barrow" TheThreeTrials 1 4 6) {cdCardTraits = setFromList [Barrow, Forest, Hills]}

caveBarrow :: CardDef
caveBarrow = (location "08047" "Cave Barrow" TheThreeTrials 1 3 8) {cdCardTraits = setFromList [Barrow, Forest, Hills]}

stoneBarrow :: CardDef
stoneBarrow = (location "08048" "Stone Barrow" TheThreeTrials 1 2 10) {cdCardTraits = setFromList [Barrow, Forest, Hills]}

-- | X threat, X the number of players: given by its own modifiers.
hallowedCircle :: CardDef
hallowedCircle =
  (location "08049" "Hallowed Circle" TheThreeTrials 1 0 12)
    { cdCardTraits = setFromList [Forest, Hills]
    , cdVictoryPoints = Just 5
    }

cursedForest :: CardDef
cursedForest =
  (location "08051" "Cursed Forest" TheThreeTrials 2 3 3)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

-- | X threat, X the number of players: given by its own modifiers.
grimFoothills :: CardDef
grimFoothills =
  (location "08052" "Grim Foothills" TheThreeTrials 3 0 3)
    { cdCardTraits = setFromList [Hills]
    , cdKeywords = setFromList [Doomed 2]
    , cdEasyModeQuantity = Just 2
    }

accursedBattlefield :: CardDef
accursedBattlefield =
  (location "10132" "Accursed Battlefield" TheBattleOfCarnDum 3 2 3)
    { cdCardTraits = setFromList [CarnDum]
    , cdShadowEffect = True
    }

mountainsOfAngmar :: CardDef
mountainsOfAngmar =
  (location "10133" "Mountains of Angmar" TheBattleOfCarnDum 2 3 5)
    { cdCardTraits = setFromList [Mountain]
    , cdShadowEffect = True
    }

blightOfCarnDum :: CardDef
blightOfCarnDum =
  (location "10134" "Blight of Carn Dûm" TheBattleOfCarnDum 3 3 3)
    { cdCardTraits = setFromList [Blight, Plains]
    , cdShadowEffect = True
    }

fortressWalls :: CardDef
fortressWalls =
  (location "10135" "Fortress Walls" TheBattleOfCarnDum 3 4 3)
    { cdCardTraits = setFromList [CarnDum]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

eriadorWastes :: CardDef
eriadorWastes = (location "10023" "Eriador Wastes" TheWastesOfEriador 2 3 5) {cdCardTraits = setFromList [Arnor, Plains], cdEasyModeQuantity = Just 1}

wargsDen :: CardDef
wargsDen = (location "10024" "Warg's Den" TheWastesOfEriador 2 4 1) {cdCardTraits = setFromList [Arnor], cdEasyModeQuantity = Just 1}

northDowns :: CardDef
northDowns =
  (location "10025" "North Downs" TheWastesOfEriador 3 1 4)
    { cdCardTraits = setFromList [Arnor, Hills]
    , cdShadowEffect = True
    }
southernGate :: CardDef
southernGate =
  unique
    (location "10042" "Southern Gate" EscapeFromMountGram 1 3 8)
      { cdCardTraits = setFromList [MountGram]
      , cdVictoryPoints = Just 2
      }

prisonCell :: CardDef
prisonCell =
  (location "10046" "Prison Cell" EscapeFromMountGram 4 1 3)
    { cdCardTraits = setFromList [MountGram, Underground]
    , cdKeywords = setFromList [Capture 1]
    }

patrolRoom :: CardDef
patrolRoom =
  (location "10047" "Patrol Room" EscapeFromMountGram 2 3 2)
    { cdCardTraits = setFromList [MountGram, Underground]
    , cdKeywords = setFromList [Capture 2]
    , cdShadowEffect = True
    }

-- | X threat, X the number of cards captured underneath it.
tunnelsOfMountGram :: CardDef
tunnelsOfMountGram =
  (location "10048" "Tunnels of Mount Gram" EscapeFromMountGram 2 0 4)
    { cdCardTraits = setFromList [MountGram, Underground]
    , cdKeywords = setFromList [Capture 1]
    , cdShadowEffect = True
    }
chetwoodForest :: CardDef
chetwoodForest =
  (location "09018" "Chetwood Forest" IntrudersInChetwood 3 3 3)
    { cdCardTraits = setFromList [Arnor, Forest]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

bordersOfBreeLand :: CardDef
bordersOfBreeLand =
  (location "09019" "Borders of Bree-land" IntrudersInChetwood 2 2 4)
    { cdCardTraits = setFromList [Arnor]
    , cdShadowEffect = True
    }

outlyingHomestead :: CardDef
outlyingHomestead =
  (location "09020" "Outlying Homestead" IntrudersInChetwood 3 2 8)
    { cdCardTraits = setFromList [Arnor]
    , cdKeywords = setFromList [Doomed 1]
    , cdEasyModeQuantity = Just 2
    }

ruggedCountry :: CardDef
ruggedCountry =
  (location "09047" "Rugged Country" EriadorWilds 3 2 3)
    { cdCardTraits = setFromList [Arnor]
    , cdShadowEffect = True
    }

-- | X threat, X the number of quest cards in play: gained from its own modifiers.
shroudedHills :: CardDef
shroudedHills =
  (location "09048" "Shrouded Hills" EriadorWilds 2 0 5)
    { cdCardTraits = setFromList [Arnor, Hills]
    , cdEasyModeQuantity = Just 1
    }

amonForn :: CardDef
amonForn =
  (unique $ location "09026" "Amon Forn" TheWeatherHills 1 4 6)
    { cdCardTraits = setFromList [Arnor, Ruins]
    , cdVictoryPoints = Just 3
    }

-- | +1 threat for each resource token on it: given by its own modifiers.
weatheredHilltop :: CardDef
weatheredHilltop = (location "09027" "Weathered Hilltop" TheWeatherHills 3 1 5) {cdCardTraits = setFromList [Arnor, Hills]}

exposedRidge :: CardDef
exposedRidge =
  (location "09028" "Exposed Ridge" TheWeatherHills 3 3 3)
    { cdCardTraits = setFromList [Arnor, Hills]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

shelteredValley :: CardDef
shelteredValley = (location "09029" "Sheltered Valley" TheWeatherHills 2 2 4) {cdCardTraits = setFromList [Arnor, Hills]}

concealedOrcCamp :: CardDef
concealedOrcCamp = (location "09030" "Concealed Orc-camp" TheWeatherHills 3 2 2) {cdCardTraits = setFromList [Arnor, Hills]}

-- | An objective-location of Across the Ettenmoors: guarded, safe, and immune to player
-- card effects, worth 5 victory points.
safeLocation :: CardCode -> Name -> Int -> [Trait] -> CardDef
safeLocation code name questPoints traits =
  (location code name AcrossTheEttenmoors 1 0 questPoints)
    { cdCardTraits = setFromList (Ettenmoors : traits)
    , cdKeywords = setFromList [Guarded, Safe]
    , cdVictoryPoints = Just 5
    }

secludedCave :: CardDef
secludedCave = safeLocation "10068" "Secluded Cave" 7 [Cave]

theHoarwell :: CardDef
theHoarwell = safeLocation "10069" "The Hoarwell" 3 [River]

abandonedCamp :: CardDef
abandonedCamp = safeLocation "10070" "Abandoned Camp" 1 []

patchOfWoods :: CardDef
patchOfWoods = safeLocation "10071" "Patch of Woods" 5 [Forest]

trollFells :: CardDef
trollFells =
  (location "10077" "Troll-fells" AcrossTheEttenmoors 2 1 6)
    { cdCardTraits = setFromList [Ettenmoors]
    , cdWhenRevealed = True
    }

barrenMoorland :: CardDef
barrenMoorland =
  (location "10078" "Barren Moorland" AcrossTheEttenmoors 2 2 4)
    { cdCardTraits = setFromList [Ettenmoors]
    , cdEasyModeQuantity = Just 0
    }

brokenBattlements :: CardDef
brokenBattlements =
  (location "09036" "Broken Battlements" DeadmensDike 3 2 6)
    { cdCardTraits = setFromList [Fornost, Ruins]
    , cdKeywords = setFromList [Time 3]
    , cdEasyModeQuantity = Just 2
    }

norburyTombs :: CardDef
norburyTombs = (location "09037" "Norbury Tombs" DeadmensDike 2 2 5) {cdCardTraits = setFromList [Fornost, Ruins]}

-- | It gets +1 threat for each resource token on it: from its own modifiers.
fornostSquare :: CardDef
fornostSquare = (location "09038" "Fornost Square" DeadmensDike 3 1 4) {cdCardTraits = setFromList [Fornost, Ruins]}

hauntedKeep :: CardDef
hauntedKeep =
  (location "09039" "Haunted Keep" DeadmensDike 3 3 3)
    { cdCardTraits = setFromList [Fornost, Ruins]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

deadmensGate :: CardDef
deadmensGate =
  (location "09040" "Deadmen's Gate" DeadmensDike 2 4 4)
    { cdCardTraits = setFromList [Fornost, Ruins]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

ruinsOfArnor :: CardDef
ruinsOfArnor =
  (location "09056" "Ruins of Arnor" RuinsOfArnor 3 2 3)
    { cdCardTraits = setFromList [Arnor, Ruins]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

ancientCauseway :: CardDef
ancientCauseway = (location "09057" "Ancient Causeway" RuinsOfArnor 2 3 2) {cdCardTraits = setFromList [Arnor, Ruins]}

altarOfMidwinter :: CardDef
altarOfMidwinter =
  (location "10154" "Altar of Midwinter" TheDreadRealm 1 5 9)
    { cdUnique = True
    , cdCardTraits = setFromList [CarnDum, Underground]
    , cdVictoryPoints = Just 5
    }

sinisterDungeon :: CardDef
sinisterDungeon =
  (location "10158" "Sinister Dungeon" TheDreadRealm 3 4 1)
    { cdCardTraits = setFromList [CarnDum, Underground]
    , cdShadowEffect = True
    }

tombsOfCarnDum :: CardDef
tombsOfCarnDum =
  (location "10159" "Tombs of Carn Dûm" TheDreadRealm 3 3 3)
    { cdCardTraits = setFromList [CarnDum, Underground]
    , cdEasyModeQuantity = Just 0
    }

darkHalls :: CardDef
darkHalls =
  (location "10160" "Dark Halls" TheDreadRealm 2 2 4)
    { cdCardTraits = setFromList [CarnDum, Underground]
    , cdShadowEffect = True
    }

unholyCrypt :: CardDef
unholyCrypt =
  (location "10161" "Unholy Crypt" TheDreadRealm 3 1 7)
    { cdCardTraits = setFromList [CarnDum, Underground]
    , cdShadowEffect = True
    }

theGreatHall :: CardDef
theGreatHall =
  unique
    (location "10097" "The Great Hall" TheTreacheryOfRhudaur 1 4 8)
      { cdCardTraits = setFromList [Ruins]
      , cdVictoryPoints = Just 2
      }

forbiddenDescent :: CardDef
forbiddenDescent = (location "10101" "Forbidden Descent" TheTreacheryOfRhudaur 2 2 6) {cdCardTraits = setFromList [Ruins]}

eerieHalls :: CardDef
eerieHalls =
  (location "10102" "Eerie Halls" TheTreacheryOfRhudaur 2 5 4)
    { cdCardTraits = setFromList [Ruins]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

ghostlyRuins :: CardDef
ghostlyRuins =
  (location "10103" "Ghostly Ruins" TheTreacheryOfRhudaur 2 4 4)
    { cdCardTraits = setFromList [Ruins]
    , cdEasyModeQuantity = Just 0
    }

decrepitRemains :: CardDef
decrepitRemains =
  (location "10104" "Decrepit Remains" TheTreacheryOfRhudaur 2 1 3)
    { cdCardTraits = setFromList [Ruins]
    , cdShadowEffect = True
    }

-- * The Dream-chaser cycle: Voyage Across Belegaer, Vast Oceans, Stormy Weather

starlitSea :: CardDef
starlitSea =
  (location "11023" "Starlit Sea" VoyageAcrossBelegaer 2 3 2)
    { cdCardTraits = setFromList [Ocean]
    , cdSailingIcons = 1
    , cdShadowEffect = True
    }

fogBank :: CardDef
fogBank =
  (location "11024" "Fog Bank" VoyageAcrossBelegaer 2 2 5)
    { cdCardTraits = setFromList [Ocean]
    , cdShadowEffect = True
    }

calmWaters :: CardDef
calmWaters =
  (location "11050" "Calm Waters" VastOceans 2 1 4)
    { cdCardTraits = setFromList [Ocean]
    , cdSailingIcons = 2
    , cdShadowEffect = True
    }

rollingSeas :: CardDef
rollingSeas =
  (location "11051" "Rolling Seas" VastOceans 3 4 3)
    { cdCardTraits = setFromList [Ocean]
    , cdSailingIcons = 1
    , cdShadowEffect = True
    }

roughWaters :: CardDef
roughWaters =
  (location "11053" "Rough Waters" StormyWeather 2 2 3)
    { cdCardTraits = setFromList [Ocean]
    , cdSailingIcons = 1
    , cdShadowEffect = True
    }

waterspout :: CardDef
waterspout =
  (location "11054" "Waterspout" StormyWeather 2 2 4)
    { cdCardTraits = setFromList [Ocean]
    }
-- | The Lost Island side of The Fate of Númenor's and Ruins of Numenor's Uncharted
-- locations: no copy is gathered, each of them enters play with it faceup.
lostIsland :: CardDef
lostIsland =
  (location "the-grey-havens-lost-island" "Lost Island" TheFateOfNumenor 0 2 0)
    { cdQuestPoints = Nothing
    , cdKeywords = setFromList [Uncharted]
    }

-- | An Uncharted location: under a Lost Island side.
uncharted :: CardDef -> CardDef
uncharted def = def {cdOtherSide = Just (cdCardCode lostIsland)}

shrineToMorgoth :: CardDef
shrineToMorgoth =
  uncharted
    $ unique
      (location "11027" "Shrine to Morgoth" TheFateOfNumenor 1 5 8)
        { cdCardTraits = setFromList [Ruins]
        , cdVictoryPoints = Just 5
        }

lushJungle :: CardDef
lushJungle = uncharted (location "11030" "Lush Jungle" TheFateOfNumenor 2 4 4) {cdCardTraits = setFromList [Forest]}

forbiddenCoast :: CardDef
forbiddenCoast = uncharted (location "11031" "Forbidden Coast" TheFateOfNumenor 2 1 6) {cdCardTraits = setFromList [Coastland]}

steepPlateau :: CardDef
steepPlateau =
  uncharted
    (location "11032" "Steep Plateau" TheFateOfNumenor 3 1 3)
      { cdCardTraits = setFromList [Coastland]
      , cdShadowEffect = True
      }

floodedRuins :: CardDef
floodedRuins = uncharted (location "11072" "Flooded Ruins" RuinsOfNumenor 2 3 5) {cdCardTraits = setFromList [Ruins]}

drownedGraves :: CardDef
drownedGraves = uncharted (location "11073" "Drowned Graves" RuinsOfNumenor 2 2 2) {cdCardTraits = setFromList [Ruins]}

cursedTemple :: CardDef
cursedTemple = uncharted (location "11074" "Cursed Temple" RuinsOfNumenor 2 1 3) {cdCardTraits = setFromList [Ruins]}

-- * The Stormcaller: the decks of the ship the players fight over (The Thing in the Depths)

-- | Immune to player card effects: its module says so.
helmOfTheStormcaller :: CardDef
helmOfTheStormcaller =
  unique
    (location "12042" "Helm of the Stormcaller" TheStormcaller 1 2 1)
      { cdCardTraits = setFromList [Ship, Deck]
      , cdVictoryPoints = Just 5
      }

forecastleDeck :: CardDef
forecastleDeck = (location "12043" "Forecastle Deck" TheStormcaller 2 3 5) {cdCardTraits = setFromList [Ship, Deck]}

weatherDeck :: CardDef
weatherDeck =
  (location "12044" "Weather Deck" TheStormcaller 2 2 3)
    { cdCardTraits = setFromList [Ship, Deck]
    , cdShadowEffect = True
    }

cargoHold :: CardDef
cargoHold =
  (location "12045" "Cargo Hold" TheStormcaller 2 2 2)
    { cdCardTraits = setFromList [Ship]
    , cdShadowEffect = True
    }

crewQuarters :: CardDef
crewQuarters =
  (location "12046" "Crew Quarters" TheStormcaller 2 1 4)
    { cdCardTraits = setFromList [Ship]
    , cdShadowEffect = True
    }

quarterDeck :: CardDef
quarterDeck =
  (location "12047" "Quarter Deck" TheStormcaller 2 4 3)
    { cdCardTraits = setFromList [Ship, Deck]
    , cdShadowEffect = True
    }
