module LordOfTheRings.Location.CardDefs (module LordOfTheRings.Location.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType (CardType (EnemyType, LocationType))
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
      , docksideStreet
      , pelargirAlley
      , ithilienOverlook
      , denseThickets
      , cormallenFields
      , besiegedCourtyard
      , wallsOfCairAndros
      , anduinWharf
      , abandonedCampNightmare
      , gladeOfTheSpawn
      , aFloodedFord
      , gladdenMarshlands
      , tortureChamber
      , dungeonLabyrinth
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
      , plainsOfDunland
      , munuvDuvRiver
      , hithaeglirWoods
      , sacredForest
      , hauntedPillars
      , cityOfSpies
      , darkPathway
      , desolateMeadow
      , peatBog
      , bonesOfHollin
      , dustyLabyrinth
      , ravenStronghold
      , highlandsOfDunland
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
      , northernChetwood
      , outskirtsOfArchet
      , greenwayPath
      , weatherTornValley
      , ruinedWatchTower
      , palaceRuins
      , fornostStreet
      , decrepitCity
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
      , dreamChaserLocation
      , mithlondHarbor
      , burningPiers
      , pillagedShip
      , whiteShip
      , elvenWaveRunner
      , elvenCaravel
      , jaggedReef
      , hiddenCove
      , vastCoastland
      , helmOfTheStormcaller
      , forecastleDeck
      , weatherDeck
      , cargoHold
      , crewQuarters
      , quarterDeck
      , shrineToMorgothTheDrownedRuins
      , shrineToMorgothUnderwater
      , underseaGrotto
      , cursedCaverns
      , waterLoggedHalls
      , darkAbyss
      , twistingHollow
      , sunkenTemple
      , drownedCave
      , lostIslandTempleOfTheDeceived
      , templeOfTheDeceived
      , theGrottosEntrance
      , templeHalls
      , jaggedCliffs
      , windingCaverns
      , junglePath
      , coastOfAnfalas
      , belfalasIslet
      , cobasHaven
      , southernBelfalas
      , corsairWaters
      , watchTower
      , theShatteredMonument
      , streetsOfUmbar
      , cityOfCorsairs
      , umbarHarbor
      , westernShore
      , easternShore
      , isenRiver
      , methedrasPeak
      , orcTrail
      , tangledForest
      , entwood
      , darkCornerOfFangorn
      , openWaters
      , shatteredSea
      , shrineToMorgothNightmare
      , cursedTempleNightmare
      , drownedGravesNightmare
      , floodedRuinsNightmare
      , lushJungleNightmare
      , desecratedGrounds
      , forsakenCity
      , overgrownSteps
      , narelenyaLocation
      , dawnStarLocation
      , silverWingLocation
      , burningTower
      , twistingShadows
      , narrowFissure
      , chamberOfRecords
      , deepMines
      , westernHall
      , shadowedCorridor
      , sharpJetties
      , cliffsideInlet
      , captainsQuarters
      , aftCabin
      , edgeOfTheTemple
      , desecratedRuinsNightmare
      , templeFoothills
      , cursedTempleOfTheDeceived
      , overgrownPassage
      , sharpPrecipice
      , jaggedCavern
      , submergedCrawlway
      , floodedCaverns
      , cityUnderSiege
      , seaWall
      , warTornWaters
      , wallsOfUmbar
      , umbarBridge
      , watchedPath
      , huntersLookout
      , darkPools
      , trollLair
      , hivesAndHives
      , darkWoodGrove
      , labyrinthineWoods
      , pitchDarkThicket
      , bramblesAndThorns
      , sarnGebirRapids
      , theMereOfDeadFaces
      , stagnantQuagmire
      , deceptivePath
      , webWall
      , banksOfMorgulduin
      , ephelDuath
      , whiteFloweredFields
      , edgeOfIthilien
      , mountainsOfShadow
      , devilsMirk
      , eilenach
      , stonewainValley
      , besiegedHomestead
      , overrunVillage
      , palaceRemains
      , crumbledBattlements
      , easternQuarter
      , westernQuarter
      , traitorsDen
      , rathCelerdain
      , archedTunnel
      , icyCavern
      , frozenSilverlode
      , treacherousBluffs
      , concealedWoods
      , banksOfSirannon
      , hideousDepths
      , easternMines
      , bottomlessChasm
      , nestOfHorrors
      , shiveringRiver
      , fieryDepths
      , crumblingStairs
      , shroudedWilderness
      , snowyBadlands
      , prisonCellNightmare
      , tortureChamberMountGram
      , rockyFoothills
      , vastTrollFells
      , forgottenCourtyard
      , charredCellar
      , secretAntechamber
      , strongholdTower
      , gatesOfCarnDum
      , innerSanctum
      , cursedHalls
      , toweringDunes
      , desertOasis
      , jungleTrail
      , forestClearing
      , denseJungle
      , southronBoma
      , villageHut
      , spiderBurrow
      , pitchBlackTunnel
      , nestOfVermin
      , cobwebbedCavern
      , blockedPassage
      , burntSands
      , scorchedHamada
      , parchedWadi
      , narrowAlleyway
      , umbarStreet
      , umbarSquare
      , portOfHarad
      , haradrimEstate
      , riverHarnen
      , expanseOfHarad
      , blisteringErg
      , haradRoad
      , theSerpentsRedoubt
      , rooflessTower
      , desertBastion
      , southronCamp
      , brokenCourtyard
      , gateOfIron
      , dungeonCell
      , urukGuardroom
      , minesOfCirithGurat
      , tortureRoom
      , mumakTrail
      , desolateLand
      , harondorWaste
      , foothillsOfMordor
      , shadowyVale
      , hillsOfWilderland
      , lonelyLands
      , accursedForest
      , darkBlackWoods
      , darkTunnel
      , forkedPassage
      , lostArmory
      , ancientTreasury
      , frightfulDen
      , lightlessGrotto
      , crumblingCavern
      , undergroundLake
      , woodmenVillage
      , gateOfGundabad
      , hilltopBattlements
      , dwarvenWatchtower
      , slopesOfGundabad
      , theOldFordJourneyUpTheAnduin
      , woodedRiverbank
      , townGate
      , woodenPalisade
      , burningWatchtower
      , dragonHoard
      , ironHillsMine
      , contestedDepths
      , deepChasm
      , theCursedTomb
      , forgottenCrypt
      , hauntedHall
      , hiddenStaircase
      , moonlitPassage
      , cursedTower
      , theFirstForge
      , dagnirsHoard
      , throatOfTheMountain
      , minesOfGundabad
      , narrowStair
      , desecratedRuins
      , mountainPass
      , dwarvenDoor
      , caveEntrance
      , creatureDen
      , highFalls
      , greyMoorland
      , deepRavine
      , slopesOfEredMithrin
      , overgrownPath
      , abandonedVillage
      , bareHilltop
      , forestClearingLostInMirkwood
      , twilightHall
      , theTowerOfBaradDur
      , rollingPlains
      , rockyOutcrop
      , cityForum
      , seedyTavern
      , sideStreet
      , catacombs
      , twistedTunnel
      , narrowOpening
      , crumblingPassage
      , riverRunning
      , exposedRiverbank
      , templeOfSauron
      , hallOfHorrors
      , theKingsLawn
      , guardedArchway
      , crowdedMarket
      , streetsOfUlfast
      , fieryChasm
      , hideousDeeps
      , orcPassage
      , hillsideManor
      , gateOfDorwinion
      , contestedSquare
      , ransackedHouse
      , makeshiftFortification
      , capturedTower
      , narrowPass
      , crowdedTrack
      , ravineLedge
      , fortressWallsTheFortressOfNurn
      , guardTower
      , plazaOfNurn
      , theBarracks
      , slaveQuarters
      , theStorehouse
      , workingFields
      , watchTowerTheLandOfSorrow
      , theStoneOfErech
      , darkDoor
      , trystingPlace
      , deadlyRoad
      , hauntedPath
      , redhornPass
      , doorsOfDurinTheRingGoesSouth
      , treeCrownedHill
      , hillsOfHollin
      , redhornFoothills
      , eregion
      , evilSmellingFens
      , shiftingQuagmire
      , hauntedMere
      , stagnantPools
      , softMires
      , theSpidersGlade
      , giantWeb
      , forestStream
      , hiddenPath
      , northernSlopes
      , ruinsOfDale
      , ravenhill
      , theFrontGate
      , trollCamp
      , trollCave
      , hobbitLands
      , drearyHills
      , loneLands
      , caveEntranceWesternLands
      , theGoblinsCaves
      , overhangingRock
      , theHighPass
      , frontPorch
      , greatCavernRoom
      , theWargsGlade
      , lakeInTheCavern
      , theLonelyMountain
      , greatHall
      , secretEntrance
      , desolationOfSmaug
      , westernSlopes
      , bagEnd
      , woodyEnd
      , stockBrook
      , bamfurlong
      , buckleburyFerry
      , greenHillCountry
      , stockRoad
      , theMarish
      , thePrancingPony
      , weathertop
      , chetwood
      , midgewater
      , weatherHills
      , pathlessCountryHunted
      , theLastBridge
      , fordOfBruinen
      , theTrollsCamp
      , ettenmoors
      , theOldRoad
      , greatCorsairShip
      , umbarWarGalley
      , corsairDromon
      , portOfPelargir
      , pelargirQuays
      , grond
      , gateOfGondor
      , cityWall
      , pelennorField
      , fireFilledTrench
      , siegeTower
      , topmostChamber
      , echoingPassage
      , bloodiedCourtyard
      , windingStairs
      , torchLitHall
      , theTwoWatchers
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
    , cdEasyModeQuantity = Just 1
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
    , cdEasyModeQuantity = Just 1
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

-- | The Fate of Númenor's Shrine to Morgoth: the standard card, or the Nightmare Deck's
-- reprint in its place.
fateOfNumenorShrines :: [CardDef]
fateOfNumenorShrines = [shrineToMorgoth, shrineToMorgothNightmare]

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

-- | Raid on the Grey Havens' Dream-chaser, a location: not the fleet's Ship objective.
dreamChaserLocation :: CardDef
dreamChaserLocation =
  unique
    (location "11039" "Dream-chaser" RaidOnTheGreyHavens 1 2 0)
      { cdQuestPoints = Nothing
      , cdCardTraits = setFromList [GreyHavens, Ship]
      , cdKeywords = setFromList [Aflame 12]
      }

mithlondHarbor :: CardDef
mithlondHarbor =
  (location "11042" "Mithlond Harbor" RaidOnTheGreyHavens 2 1 2)
    { cdCardTraits = setFromList [GreyHavens]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

burningPiers :: CardDef
burningPiers =
  (location "11043" "Burning Piers" RaidOnTheGreyHavens 2 1 3)
    { cdCardTraits = setFromList [GreyHavens]
    , cdKeywords = setFromList [Aflame 5]
    , cdShadowEffect = True
    }

pillagedShip :: CardDef
pillagedShip =
  (location "11044" "Pillaged Ship" RaidOnTheGreyHavens 2 2 4)
    { cdCardTraits = setFromList [GreyHavens, Ship]
    , cdKeywords = setFromList [Aflame 5]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

whiteShip :: CardDef
whiteShip =
  (location "11045" "White Ship" RaidOnTheGreyHavens 2 4 6)
    { cdCardTraits = setFromList [GreyHavens, Ship]
    , cdKeywords = setFromList [Aflame 4]
    , cdEasyModeQuantity = Just 1
    }

elvenWaveRunner :: CardDef
elvenWaveRunner =
  (location "11046" "Elven Wave-runner" RaidOnTheGreyHavens 2 3 3)
    { cdCardTraits = setFromList [GreyHavens, Ship]
    , cdKeywords = setFromList [Aflame 3]
    , cdVictoryPoints = Just 3
    , cdShadowEffect = True
    }

elvenCaravel :: CardDef
elvenCaravel =
  (location "11047" "Elven Caravel" RaidOnTheGreyHavens 2 3 5)
    { cdCardTraits = setFromList [GreyHavens, Ship]
    , cdKeywords = setFromList [Aflame 4]
    , cdEasyModeQuantity = Just 1
    }

-- * Flight of the Stormcaller

jaggedReef :: CardDef
jaggedReef =
  (location "12025" "Jagged Reef" FlightOfTheStormcaller 2 1 4)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdKeywords = setFromList [Surge]
    }

hiddenCove :: CardDef
hiddenCove = (location "12026" "Hidden Cove" FlightOfTheStormcaller 2 3 3) {cdCardTraits = setFromList [Coastland, Ocean]}

vastCoastland :: CardDef
vastCoastland =
  (location "12027" "Vast Coastland" FlightOfTheStormcaller 2 6 4)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdSailingIcons = 1
    , cdShadowEffect = True
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 1
    }

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
forecastleDeck = (location "12043" "Forecastle Deck" TheStormcaller 2 3 5) {cdCardTraits = setFromList [Ship, Deck], cdEasyModeQuantity = Just 1}

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
    , cdEasyModeQuantity = Just 1
    }

-- | The Drowned Ruins' Shrine to Morgoth, Grotto side: set aside until Sahír's Betrayal.
shrineToMorgothTheDrownedRuins :: CardDef
shrineToMorgothTheDrownedRuins =
  unique
    (location "12094" "Shrine to Morgoth" TheDrownedRuins 1 5 18)
      { cdCardTraits = setFromList [Ruins, Underground, Grotto]
      , cdVictoryPoints = Just 5
      , cdOtherSide = Just "12095"
      }

-- | Its Underwater side: no copy is gathered, the Grotto side turns into it.
shrineToMorgothUnderwater :: CardDef
shrineToMorgothUnderwater =
  unique
    (location "12095" "Shrine to Morgoth" TheDrownedRuins 0 5 18)
      { cdCardTraits = setFromList [Ruins, Underground, Underwater]
      , cdVictoryPoints = Just 5
      , cdOtherSide = Just "12094"
      }

{- | The Grotto deck's cards, Grotto side up: each has one Underwater side, gathered with
none of its own. The scans pair Water-logged Halls with Dark Abyss and Sunken Temple with
Cursed Caverns, against the quantities Hall of Beorn gives (3 Undersea Grotto / Dark Abyss,
3 Cursed Caverns / Twisting Hollow, 2 Water-logged Halls / Sunken Temple), which these
follow, the two Ruins sides together.
-}

underseaGrotto :: CardDef
underseaGrotto =
  (location "12098" "Undersea Grotto" TheDrownedRuins 3 2 3)
    { cdCardTraits = setFromList [Underground, Grotto]
    , cdOtherSide = Just "12101"
    }

cursedCaverns :: CardDef
cursedCaverns =
  (location "12099" "Cursed Caverns" TheDrownedRuins 3 3 5)
    { cdCardTraits = setFromList [Underground, Grotto]
    , cdOtherSide = Just "12102"
    }

waterLoggedHalls :: CardDef
waterLoggedHalls =
  (location "12100" "Water-logged Halls" TheDrownedRuins 2 2 4)
    { cdCardTraits = setFromList [Ruins, Underground, Grotto]
    , cdOtherSide = Just "12103"
    }

darkAbyss :: CardDef
darkAbyss =
  (location "12101" "Dark Abyss" TheDrownedRuins 0 5 12)
    { cdCardTraits = setFromList [Underground, Underwater]
    , cdVictoryPoints = Just 2
    , cdOtherSide = Just "12098"
    }

twistingHollow :: CardDef
twistingHollow =
  (location "12102" "Twisting Hollow" TheDrownedRuins 0 4 14)
    { cdCardTraits = setFromList [Underground, Underwater]
    , cdVictoryPoints = Just 2
    , cdOtherSide = Just "12099"
    }

sunkenTemple :: CardDef
sunkenTemple =
  (location "12103" "Sunken Temple" TheDrownedRuins 0 3 10)
    { cdCardTraits = setFromList [Ruins, Underground, Underwater]
    , cdVictoryPoints = Just 2
    , cdOtherSide = Just "12100"
    }

-- | Single-sided, in the encounter deck.
drownedCave :: CardDef
drownedCave = (location "12104" "Drowned Cave" TheDrownedRuins 2 3 4) {cdCardTraits = setFromList [Underground, Underwater]}

-- | The Lost Island side of Temple of the Deceived's own Uncharted locations: no copy is
-- gathered, each of them enters play with it faceup.
lostIslandTempleOfTheDeceived :: CardDef
lostIslandTempleOfTheDeceived =
  (location "temple-of-the-deceived-lost-island" "Lost Island" TempleOfTheDeceived 0 2 0)
    { cdQuestPoints = Nothing
    , cdKeywords = setFromList [Uncharted]
    }

-- | The Uncharted side of the three Temple locations, the Island Map's rightmost column.
templeOfTheDeceived :: CardDef
templeOfTheDeceived =
  (location "temple-of-the-deceived-temple-of-the-deceived" "Temple of the Deceived" TempleOfTheDeceived 0 2 0)
    { cdQuestPoints = Nothing
    , cdKeywords = setFromList [Uncharted]
    }

-- | A Temple location: under a Temple of the Deceived side.
inTheTemple :: CardDef -> CardDef
inTheTemple def = def {cdOtherSide = Just (cdCardCode templeOfTheDeceived), cdCardTraits = setFromList [Ruins]}

-- | An Uncharted location of Temple of the Deceived: under its own Lost Island side.
onTheIsland :: CardDef -> CardDef
onTheIsland def = def {cdOtherSide = Just (cdCardCode lostIslandTempleOfTheDeceived)}

theGrottosEntrance :: CardDef
theGrottosEntrance = inTheTemple $ unique $ location "12068" "The Grotto's Entrance" TempleOfTheDeceived 1 2 8

templeHalls :: CardDef
templeHalls = inTheTemple $ location "12069" "Temple Halls" TempleOfTheDeceived 2 5 5

jaggedCliffs :: CardDef
jaggedCliffs = onTheIsland (location "12072" "Jagged Cliffs" TempleOfTheDeceived 2 4 4) {cdCardTraits = setFromList [Mountain]}

windingCaverns :: CardDef
windingCaverns = onTheIsland (location "12073" "Winding Caverns" TempleOfTheDeceived 2 2 6) {cdCardTraits = setFromList [Underground]}

junglePath :: CardDef
junglePath = onTheIsland (location "12074" "Jungle Path" TempleOfTheDeceived 2 3 3) {cdCardTraits = setFromList [Forest]}

-- * A Storm on Cobas Haven

coastOfAnfalas :: CardDef
coastOfAnfalas =
  (location "12128" "Coast of Anfalas" AStormOnCobasHaven 3 3 3)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdEasyModeQuantity = Just 1
    }

-- * The City of Corsairs: Coast of Umbar and The City of Corsairs

southernBelfalas :: CardDef
southernBelfalas =
  (location "12154" "Southern Belfalas" CoastOfUmbar 2 3 5)
    { cdCardTraits = setFromList [Ocean, Coastland]
    , cdSailingIcons = 1
    , cdShadowEffect = True
    }

belfalasIslet :: CardDef
belfalasIslet =
  (location "12129" "Belfalas Islet" AStormOnCobasHaven 2 1 5)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdVictoryPoints = Just 1
    }

cobasHaven :: CardDef
cobasHaven =
  (location "12130" "Cobas Haven" AStormOnCobasHaven 2 2 4)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdShadowEffect = True
    }

corsairWaters :: CardDef
corsairWaters =
  (location "12155" "Corsair Waters" CoastOfUmbar 2 4 3)
    { cdCardTraits = setFromList [Ocean, Coastland]
    , cdSailingIcons = 1
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

watchTower :: CardDef
watchTower =
  (location "12156" "Watch Tower" CoastOfUmbar 2 2 3)
    { cdCardTraits = setFromList [Umbar, Coastland]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

theShatteredMonument :: CardDef
theShatteredMonument =
  unique
    (location "12159" "The Shattered Monument" TheCityOfCorsairs 1 4 8)
      { cdCardTraits = setFromList [Umbar]
      , cdVictoryPoints = Just 5
      }

streetsOfUmbar :: CardDef
streetsOfUmbar =
  (location "12160" "Streets of Umbar" TheCityOfCorsairs 3 1 5)
    { cdCardTraits = setFromList [Umbar, City]
    , cdShadowEffect = True
    }

-- | Its threat is X, printed as 0: see its card.
cityOfCorsairs :: CardDef
cityOfCorsairs = (location "12161" "City of Corsairs" TheCityOfCorsairs 2 0 4) {cdCardTraits = setFromList [Umbar, City], cdEasyModeQuantity = Just 1}

umbarHarbor :: CardDef
umbarHarbor =
  (location "12162" "Umbar Harbor" TheCityOfCorsairs 3 4 4)
    { cdCardTraits = setFromList [Umbar, City]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

-- Dream-chaser Nightmare Decks

sharpJetties :: CardDef
sharpJetties =
  (location "flight-of-the-stormcaller-nightmare-flight-of-the-stormcaller-005" "Sharp Jetties" FlightOfTheStormcallerNightmare 3 2 6)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdWhenRevealed = True
    }

cliffsideInlet :: CardDef
cliffsideInlet =
  (location "flight-of-the-stormcaller-nightmare-flight-of-the-stormcaller-006" "Cliffside Inlet" FlightOfTheStormcallerNightmare 3 4 3)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdSailingIcons = 1
    }

captainsQuarters :: CardDef
captainsQuarters =
  unique
    (location "the-thing-in-the-depths-nightmare-the-thing-in-the-depths-002" "Captain's Quarters" TheThingInTheDepthsNightmare 1 5 5)
      { cdCardTraits = setFromList [Ship]
      }

aftCabin :: CardDef
aftCabin =
  (location "the-thing-in-the-depths-nightmare-the-thing-in-the-depths-003" "Aft Cabin" TheThingInTheDepthsNightmare 3 3 3)
    { cdCardTraits = setFromList [Ship, Deck]
    }

edgeOfTheTemple :: CardDef
edgeOfTheTemple =
  (location "temple-of-the-deceived-nightmare-temple-of-the-deceived-edge-of-the-temple" "Edge of the Temple" TempleOfTheDeceivedNightmare 0 3 0)
    { cdQuestPoints = Nothing
    , cdKeywords = setFromList [Uncharted]
    }

desecratedRuinsNightmare :: CardDef
desecratedRuinsNightmare =
  (location "temple-of-the-deceived-nightmare-temple-of-the-deceived-002" "Desecrated Ruins" TempleOfTheDeceivedNightmare 2 4 6)
    { cdCardTraits = setFromList [Ruins]
    , cdOtherSide = Just "temple-of-the-deceived-nightmare-temple-of-the-deceived-edge-of-the-temple"
    }

templeFoothills :: CardDef
templeFoothills =
  (location "temple-of-the-deceived-nightmare-temple-of-the-deceived-003" "Temple Foothills" TempleOfTheDeceivedNightmare 2 2 5)
    { cdCardTraits = setFromList [Ruins]
    , cdOtherSide = Just "temple-of-the-deceived-nightmare-temple-of-the-deceived-edge-of-the-temple"
    }

cursedTempleOfTheDeceived :: CardDef
cursedTempleOfTheDeceived =
  (location "temple-of-the-deceived-nightmare-temple-of-the-deceived-004" "Cursed Temple" TempleOfTheDeceivedNightmare 2 3 4)
    { cdCardTraits = setFromList [Ruins]
    , cdOtherSide = Just "temple-of-the-deceived-nightmare-temple-of-the-deceived-edge-of-the-temple"
    }

overgrownPassage :: CardDef
overgrownPassage =
  (location "the-drowned-ruins-nightmare-the-drowned-ruins-002" "Overgrown Passage" TheDrownedRuinsNightmare 2 5 5)
    { cdCardTraits = setFromList [Underground, Grotto]
    , cdOtherSide = Just "the-drowned-ruins-nightmare-the-drowned-ruins-004"
    }

sharpPrecipice :: CardDef
sharpPrecipice =
  (location "the-drowned-ruins-nightmare-the-drowned-ruins-003" "Sharp Precipice" TheDrownedRuinsNightmare 2 2 3)
    { cdCardTraits = setFromList [Underground, Grotto]
    , cdOtherSide = Just "the-drowned-ruins-nightmare-the-drowned-ruins-005"
    }

jaggedCavern :: CardDef
jaggedCavern =
  (location "the-drowned-ruins-nightmare-the-drowned-ruins-004" "Jagged Cavern" TheDrownedRuinsNightmare 0 6 8)
    { cdCardTraits = setFromList [Underground, Underwater]
    , cdVictoryPoints = Just 2
    , cdOtherSide = Just "the-drowned-ruins-nightmare-the-drowned-ruins-002"
    }

submergedCrawlway :: CardDef
submergedCrawlway =
  (location "the-drowned-ruins-nightmare-the-drowned-ruins-005" "Submerged Crawlway" TheDrownedRuinsNightmare 0 2 16)
    { cdCardTraits = setFromList [Underground, Underwater]
    , cdVictoryPoints = Just 2
    , cdOtherSide = Just "the-drowned-ruins-nightmare-the-drowned-ruins-003"
    }

floodedCaverns :: CardDef
floodedCaverns =
  (location "the-drowned-ruins-nightmare-the-drowned-ruins-009" "Flooded Caverns" TheDrownedRuinsNightmare 2 2 6)
    { cdCardTraits = setFromList [Underground, Underwater]
    }

cityUnderSiege :: CardDef
cityUnderSiege =
  (location "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-006" "City Under Siege" AStormOnCobasHavenNightmare 3 2 6)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdShadowEffect = True
    }

seaWall :: CardDef
seaWall =
  (location "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-007" "Sea-wall" AStormOnCobasHavenNightmare 3 2 2)
    { cdCardTraits = setFromList [Coastland, Ocean]
    , cdShadowEffect = True
    }

warTornWaters :: CardDef
warTornWaters =
  (location "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-008" "War-torn Waters" AStormOnCobasHavenNightmare 2 3 5)
    { cdCardTraits = setFromList [Coastland, Ocean]
    }

wallsOfUmbar :: CardDef
wallsOfUmbar =
  (location "the-city-of-corsairs-nightmare-the-city-of-corsairs-004" "Walls of Umbar" CoastOfUmbarNightmare 2 5 4)
    { cdCardTraits = setFromList [Ocean, Coastland]
    , cdSailingIcons = 1
    }

umbarBridge :: CardDef
umbarBridge =
  (location "the-city-of-corsairs-nightmare-the-city-of-corsairs-005" "Umbar Bridge" CoastOfUmbarNightmare 2 3 6)
    { cdCardTraits = setFromList [Umbar, Coastland]
    , cdSailingIcons = 1
    }

-- Core Set Nightmare Decks

abandonedCampNightmare :: CardDef
abandonedCampNightmare =
  (location "core-set-nightmare-passage-through-mirkwood-008" "Abandoned Camp" PassageThroughMirkwoodNightmare 3 2 2)
    { cdCardTraits = setFromList [Mirkwood]
    , cdShadowEffect = True
    }

gladeOfTheSpawn :: CardDef
gladeOfTheSpawn =
  (location "core-set-nightmare-passage-through-mirkwood-009" "Glade of the Spawn" PassageThroughMirkwoodNightmare 2 4 4)
    { cdCardTraits = setFromList [Mirkwood]
    }

aFloodedFord :: CardDef
aFloodedFord =
  (location "core-set-nightmare-journey-along-the-anduin-005" "A Flooded Ford" JourneyAlongTheAnduinNightmare 2 3 6)
    { cdCardTraits = setFromList [Riverland]
    , cdShadowEffect = True
    }

gladdenMarshlands :: CardDef
gladdenMarshlands =
  (location "core-set-nightmare-journey-along-the-anduin-006" "Gladden Marshlands" JourneyAlongTheAnduinNightmare 3 10 10)
    { cdCardTraits = setFromList [Marshland]
    , cdShadowEffect = True
    }

tortureChamber :: CardDef
tortureChamber =
  (location "core-set-nightmare-escape-from-dol-guldur-005" "Torture Chamber" EscapeFromDolGuldurNightmare 2 3 6)
    { cdCardTraits = setFromList [DolGuldur]
    }

dungeonLabyrinth :: CardDef
dungeonLabyrinth =
  (location "core-set-nightmare-escape-from-dol-guldur-006" "Dungeon Labyrinth" EscapeFromDolGuldurNightmare 3 1 4)
    { cdCardTraits = setFromList [DolGuldur]
    }

-- Heirs of Númenor Nightmare Decks

docksideStreet :: CardDef
docksideStreet = (location "heirs-of-numenor-nightmare-peril-in-pelargir-005" "Dockside Street" PerilInPelargirNightmare 3 2 6) {cdCardTraits = setFromList [City]}

pelargirAlley :: CardDef
pelargirAlley =
  (location "heirs-of-numenor-nightmare-peril-in-pelargir-006" "Pelargir Alley" PerilInPelargirNightmare 2 2 5)
    { cdCardTraits = setFromList [City]
    , cdShadowEffect = True
    }

-- The Ring-maker Nightmare Decks

plainsOfDunland :: CardDef
plainsOfDunland =
  (location "the-dunland-trap-nightmare-the-dunland-trap-004" "Plains of Dunland" TheDunlandTrapNightmare 2 4 5)
    { cdCardTraits = setFromList [Enedwaith, Plains]
    }

munuvDuvRiver :: CardDef
munuvDuvRiver =
  (location "the-dunland-trap-nightmare-the-dunland-trap-005" "Munuv Dûv River" TheDunlandTrapNightmare 3 2 3)
    { cdCardTraits = setFromList [Enedwaith, Hills, River]
    , cdShadowEffect = True
    }

hithaeglirWoods :: CardDef
hithaeglirWoods =
  (location "the-dunland-trap-nightmare-the-dunland-trap-006" "Hithaeglir Woods" TheDunlandTrapNightmare 3 3 4)
    { cdCardTraits = setFromList [Enedwaith, Forest, Hills]
    }

sacredForest :: CardDef
sacredForest =
  (location "the-three-trials-nightmare-the-three-trials-005" "Sacred Forest" TheThreeTrialsNightmare 3 3 5)
    { cdCardTraits = setFromList [Forest]
    }

hauntedPillars :: CardDef
hauntedPillars =
  (location "the-three-trials-nightmare-the-three-trials-006" "Haunted Pillars" TheThreeTrialsNightmare 2 2 3)
    { cdCardTraits = setFromList [Forest, Hills]
    , cdWhenRevealed = True
    }

cityOfSpies :: CardDef
cityOfSpies =
  (location "trouble-in-tharbad-nightmare-trouble-in-tharbad-005" "City of Spies" TroubleInTharbadNightmare 3 2 5)
    { cdCardTraits = setFromList [City, Ruins]
    }

darkPathway :: CardDef
darkPathway =
  (location "trouble-in-tharbad-nightmare-trouble-in-tharbad-006" "Dark Pathway" TroubleInTharbadNightmare 3 5 4)
    { cdCardTraits = setFromList [City]
    , cdShadowEffect = True
    }

ithilienOverlook :: CardDef
ithilienOverlook =
  (location "heirs-of-numenor-nightmare-into-ithilien-005" "Ithilien Overlook" IntoIthilienNightmare 3 3 4)
    { cdCardTraits = setFromList [Forest]
    , cdKeywords = setFromList [Archery 2]
    }

denseThickets :: CardDef
denseThickets =
  (location "heirs-of-numenor-nightmare-into-ithilien-006" "Dense Thickets" IntoIthilienNightmare 3 1 8)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

-- | X threat, the Snow cards in the staging area: see its card module.
icyCavern :: CardDef
icyCavern =
  (location "the-redhorn-gate-nightmare-the-redhorn-gate-005" "Icy Cavern" TheRedhornGateNightmare 3 0 6)
    { cdCardTraits = setFromList [Mountain, Snow]
    , cdShadowEffect = True
    }

frozenSilverlode :: CardDef
frozenSilverlode =
  (location "the-redhorn-gate-nightmare-the-redhorn-gate-006" "Frozen Silverlode" TheRedhornGateNightmare 3 3 3)
    { cdCardTraits = setFromList [Mountain, River]
    , cdShadowEffect = True
    }

treacherousBluffs :: CardDef
treacherousBluffs =
  (location "road-to-rivendell-nightmare-road-to-rivendell-007" "Treacherous Bluffs" RoadToRivendellNightmare 3 1 7)
    { cdCardTraits = setFromList [Hills]
    , cdShadowEffect = True
    }

concealedWoods :: CardDef
concealedWoods =
  (location "road-to-rivendell-nightmare-road-to-rivendell-008" "Concealed Woods" RoadToRivendellNightmare 3 4 4)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

cormallenFields :: CardDef
cormallenFields = (location "heirs-of-numenor-nightmare-into-ithilien-007" "Cormallen Fields" IntoIthilienNightmare 2 3 5) {cdCardTraits = setFromList [Plains, Riverland]}

besiegedCourtyard :: CardDef
besiegedCourtyard =
  (location "heirs-of-numenor-nightmare-the-siege-of-cair-andros-005" "Besieged Courtyard" TheSiegeOfCairAndrosNightmare 2 2 5)
    { cdCardTraits = setFromList [CairAndros, Battleground]
    , cdShadowEffect = True
    }

wallsOfCairAndros :: CardDef
wallsOfCairAndros =
  (location "heirs-of-numenor-nightmare-the-siege-of-cair-andros-006" "Walls of Cair Andros" TheSiegeOfCairAndrosNightmare 2 4 4)
    { cdCardTraits = setFromList [CairAndros, Battleground]
    , cdShadowEffect = True
    }

anduinWharf :: CardDef
anduinWharf =
  (location "heirs-of-numenor-nightmare-the-siege-of-cair-andros-007" "Anduin Wharf" TheSiegeOfCairAndrosNightmare 2 3 6)
    { cdCardTraits = setFromList [CairAndros, Battleground, Riverland]
    }

-- The Voice of Isengard Nightmare Decks

westernShore :: CardDef
westernShore =
  (location "the-voice-of-isengard-nightmare-the-fords-of-isen-005" "Western Shore" TheFordsOfIsenNightmare 3 0 4)
    { cdCardTraits = setFromList [River, Valley]
    , cdShadowEffect = True
    }

easternShore :: CardDef
easternShore =
  (location "the-voice-of-isengard-nightmare-the-fords-of-isen-006" "Eastern Shore" TheFordsOfIsenNightmare 3 3 4)
    { cdCardTraits = setFromList [River, Valley]
    , cdShadowEffect = True
    }

isenRiver :: CardDef
isenRiver =
  (location "the-voice-of-isengard-nightmare-the-fords-of-isen-007" "Isen River" TheFordsOfIsenNightmare 3 2 5)
    { cdCardTraits = setFromList [River]
    }

methedrasPeak :: CardDef
methedrasPeak =
  (location "the-voice-of-isengard-nightmare-to-catch-an-orc-005" "Methedras Peak" ToCatchAnOrcNightmare 2 0 5)
    { cdCardTraits = setFromList [Mountain]
    }

orcTrail :: CardDef
orcTrail =
  (location "the-voice-of-isengard-nightmare-to-catch-an-orc-006" "Orc Trail" ToCatchAnOrcNightmare 2 4 2)
    { cdCardTraits = setFromList [Mountain]
    }

tangledForest :: CardDef
tangledForest =
  (location "the-voice-of-isengard-nightmare-into-fangorn-005" "Tangled Forest" IntoFangornNightmare 3 2 6)
    { cdCardTraits = setFromList [Forest]
    }

entwood :: CardDef
entwood =
  (location "the-voice-of-isengard-nightmare-into-fangorn-006" "Entwood" IntoFangornNightmare 3 3 4)
    { cdCardTraits = setFromList [Forest]
    }

darkCornerOfFangorn :: CardDef
darkCornerOfFangorn =
  (location "the-voice-of-isengard-nightmare-into-fangorn-007" "Dark Corner of Fangorn" IntoFangornNightmare 2 3 5)
    { cdCardTraits = setFromList [Forest]
    }

-- The Grey Havens Nightmare Decks: The Fate of Númenor's reprinted Uncharted deck under the same Lost Island side

openWaters :: CardDef
openWaters =
  (location "the-grey-havens-nightmare-voyage-across-belegaer-006" "Open Waters" VoyageAcrossBelegaerNightmare 2 2 6)
    { cdCardTraits = setFromList [Ocean]
    , cdSailingIcons = 1
    }

shatteredSea :: CardDef
shatteredSea =
  (location "the-grey-havens-nightmare-voyage-across-belegaer-007" "Shattered Sea" VoyageAcrossBelegaerNightmare 2 4 4)
    { cdCardTraits = setFromList [Ocean]
    , cdSailingIcons = 1
    }

shrineToMorgothNightmare :: CardDef
shrineToMorgothNightmare =
  uncharted
    $ unique
      (location "the-grey-havens-nightmare-the-fate-of-numenor-002" "Shrine to Morgoth" TheFateOfNumenorNightmare 1 5 8)
        { cdCardTraits = setFromList [Ruins]
        , cdVictoryPoints = Just 5
        }

cursedTempleNightmare :: CardDef
cursedTempleNightmare =
  uncharted (location "the-grey-havens-nightmare-the-fate-of-numenor-003" "Cursed Temple" TheFateOfNumenorNightmare 1 1 3) {cdCardTraits = setFromList [Ruins]}

drownedGravesNightmare :: CardDef
drownedGravesNightmare =
  uncharted (location "the-grey-havens-nightmare-the-fate-of-numenor-004" "Drowned Graves" TheFateOfNumenorNightmare 2 2 2) {cdCardTraits = setFromList [Ruins]}

floodedRuinsNightmare :: CardDef
floodedRuinsNightmare =
  uncharted (location "the-grey-havens-nightmare-the-fate-of-numenor-005" "Flooded Ruins" TheFateOfNumenorNightmare 1 3 5) {cdCardTraits = setFromList [Ruins]}

lushJungleNightmare :: CardDef
lushJungleNightmare =
  uncharted (location "the-grey-havens-nightmare-the-fate-of-numenor-006" "Lush Jungle" TheFateOfNumenorNightmare 2 4 4) {cdCardTraits = setFromList [Forest]}

desecratedGrounds :: CardDef
desecratedGrounds =
  uncharted (location "the-grey-havens-nightmare-the-fate-of-numenor-007" "Desecrated Grounds" TheFateOfNumenorNightmare 2 5 5) {cdCardTraits = setFromList [Ruins]}

forsakenCity :: CardDef
forsakenCity =
  uncharted (location "the-grey-havens-nightmare-the-fate-of-numenor-008" "Forsaken City" TheFateOfNumenorNightmare 2 6 4) {cdCardTraits = setFromList [Ruins]}

overgrownSteps :: CardDef
overgrownSteps =
  (location "the-grey-havens-nightmare-the-fate-of-numenor-010" "Overgrown Steps" TheFateOfNumenorNightmare 3 2 1) {cdCardTraits = setFromList [Forest, Ruins]}

narelenyaLocation :: CardDef
narelenyaLocation =
  unique
    (location "the-grey-havens-nightmare-raid-on-the-grey-havens-006" "Nárelenya" RaidOnTheGreyHavensNightmare 1 3 7)
      { cdCardTraits = setFromList [GreyHavens, Ship]
      , cdKeywords = setFromList [Aflame 10]
      , cdVictoryPoints = Just 1
      }

dawnStarLocation :: CardDef
dawnStarLocation =
  unique
    (location "the-grey-havens-nightmare-raid-on-the-grey-havens-007" "Dawn Star" RaidOnTheGreyHavensNightmare 1 3 6)
      { cdCardTraits = setFromList [Gondor, Ship]
      , cdKeywords = setFromList [Aflame 8]
      , cdVictoryPoints = Just 1
      }

silverWingLocation :: CardDef
silverWingLocation =
  unique
    (location "the-grey-havens-nightmare-raid-on-the-grey-havens-008" "Silver Wing" RaidOnTheGreyHavensNightmare 1 2 5)
      { cdCardTraits = setFromList [Gondor, Ship]
      , cdKeywords = setFromList [Aflame 6]
      , cdVictoryPoints = Just 1
      }

burningTower :: CardDef
burningTower =
  (location "the-grey-havens-nightmare-raid-on-the-grey-havens-009" "Burning Tower" RaidOnTheGreyHavensNightmare 2 2 6)
    { cdCardTraits = setFromList [GreyHavens]
    , cdKeywords = setFromList [Aflame 5]
    }

-- The Lost Realm Nightmare Decks

northernChetwood :: CardDef
northernChetwood =
  (location "the-lost-realm-nightmare-invaders-in-chetwood-004" "Northern Chetwood" IntrudersInChetwoodNightmare 3 4 6)
    { cdCardTraits = setFromList [Arnor, Forest]
    , cdShadowEffect = True
    }

outskirtsOfArchet :: CardDef
outskirtsOfArchet =
  (location "the-lost-realm-nightmare-invaders-in-chetwood-005" "Outskirts of Archet" IntrudersInChetwoodNightmare 2 5 3)
    { cdCardTraits = setFromList [Arnor]
    , cdShadowEffect = True
    }

-- The Wastes of Eriador Nightmare

shroudedWilderness :: CardDef
shroudedWilderness =
  (location "the-wastes-of-eriador-nightmare-the-wastes-of-eriador-003" "Shrouded Wilderness" TheWastesOfEriadorNightmare 3 6 4)
    { cdCardTraits = setFromList [Arnor]
    , cdShadowEffect = True
    }

greenwayPath :: CardDef
greenwayPath =
  (location "the-lost-realm-nightmare-invaders-in-chetwood-006" "Greenway Path" IntrudersInChetwoodNightmare 2 3 5)
    { cdCardTraits = setFromList [Arnor]
    , cdShadowEffect = True
    }

weatherTornValley :: CardDef
weatherTornValley =
  (location "the-lost-realm-nightmare-the-weather-hills-015" "Weather-torn Valley" TheWeatherHillsNightmare 2 5 5)
    { cdCardTraits = setFromList [Arnor, Hills]
    }

ruinedWatchTower :: CardDef
ruinedWatchTower =
  (location "the-lost-realm-nightmare-the-weather-hills-016" "Ruined Watch-tower" TheWeatherHillsNightmare 3 3 4)
    { cdCardTraits = setFromList [Arnor, Ruins]
    }

palaceRuins :: CardDef
palaceRuins =
  (location "the-lost-realm-nightmare-deadmens-dike-022" "Palace Ruins" DeadmensDikeNightmare 2 9 9)
    { cdCardTraits = setFromList [Fornost, Ruins]
    , cdShadowEffect = True
    }

fornostStreet :: CardDef
fornostStreet =
  (location "the-lost-realm-nightmare-deadmens-dike-023" "Fornost Street" DeadmensDikeNightmare 4 1 6)
    { cdCardTraits = setFromList [Fornost, Ruins]
    , cdShadowEffect = True
    }

decrepitCity :: CardDef
decrepitCity =
  (location "the-lost-realm-nightmare-deadmens-dike-024" "Decrepit City" DeadmensDikeNightmare 3 5 5)
    { cdCardTraits = setFromList [Fornost, Ruins]
    , cdShadowEffect = True
    }

-- Khazad-dûm Nightmare Decks

-- | X is the number of Dark locations in play.
twistingShadows :: CardDef
twistingShadows =
  (location "khazad-dum-nightmare-into-the-pit-006" "Twisting Shadows" IntoThePitNightmare 3 0 0)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

narrowFissure :: CardDef
narrowFissure =
  (location "khazad-dum-nightmare-into-the-pit-007" "Narrow Fissure" IntoThePitNightmare 3 3 1)
    { cdCardTraits = setFromList [Underground, Hazard]
    , cdKeywords = setFromList [Doomed 1]
    }

chamberOfRecords :: CardDef
chamberOfRecords =
  (location "khazad-dum-nightmare-the-seventh-level-003" "Chamber of Records" TheSeventhLevelNightmare 1 5 8)
    { cdCardTraits = setFromList [Underground]
    , cdVictoryPoints = Just 5
    }

deepMines :: CardDef
deepMines =
  (location "khazad-dum-nightmare-the-seventh-level-007" "Deep-mines" TheSeventhLevelNightmare 3 2 6)
    { cdCardTraits = setFromList [Underground]
    , cdShadowEffect = True
    }

banksOfSirannon :: CardDef
banksOfSirannon =
  (location "the-watcher-in-the-water-nightmare-the-watcher-in-the-water-005" "Banks of Sirannon" TheWatcherInTheWaterNightmare 3 3 3)
    { cdCardTraits = setFromList [Mountain, Swamp]
    , cdShadowEffect = True
    }

hideousDepths :: CardDef
hideousDepths =
  (location "the-watcher-in-the-water-nightmare-the-watcher-in-the-water-006" "Hideous Depths" TheWatcherInTheWaterNightmare 3 3 5)
    { cdCardTraits = setFromList [Swamp]
    }

easternMines :: CardDef
easternMines =
  (location "the-long-dark-nightmare-the-long-dark-005" "Eastern Mines" TheLongDarkNightmare 2 5 4)
    { cdCardTraits = setFromList [Underground]
    , cdShadowEffect = True
    }

westernHall :: CardDef
westernHall =
  (location "khazad-dum-nightmare-the-seventh-level-008" "Western Hall" TheSeventhLevelNightmare 2 4 4)
    { cdCardTraits = setFromList [Underground]
    }

shadowedCorridor :: CardDef
shadowedCorridor =
  (location "khazad-dum-nightmare-flight-from-moria-007" "Shadowed Corridor" FlightFromMoriaNightmare 4 4 6)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdShadowEffect = True
    }

bottomlessChasm :: CardDef
bottomlessChasm =
  (location "the-long-dark-nightmare-the-long-dark-006" "Bottomless Chasm" TheLongDarkNightmare 3 3 6)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdShadowEffect = True
    }

desolateMeadow :: CardDef
desolateMeadow =
  (location "the-nin-in-eilph-nightmare-the-nin-in-eilph-004" "Desolate Meadow" TheNinInEilphNightmare 3 2 5)
    { cdCardTraits = setFromList [Marsh]
    , cdShadowEffect = True
    }

peatBog :: CardDef
peatBog =
  (location "the-nin-in-eilph-nightmare-the-nin-in-eilph-005" "Peat Bog" TheNinInEilphNightmare 4 3 3)
    { cdCardTraits = setFromList [Marsh]
    }

bonesOfHollin :: CardDef
bonesOfHollin =
  (location "celebrimbors-secret-nightmare-celebrimbors-secret-004" "Bones of Hollin" CelebrimborsSecretNightmare 3 3 4)
    { cdCardTraits = setFromList [OstInEdhil, Ruins]
    }

dustyLabyrinth :: CardDef
dustyLabyrinth =
  (location "celebrimbors-secret-nightmare-celebrimbors-secret-005" "Dusty Labyrinth" CelebrimborsSecretNightmare 3 2 5)
    { cdCardTraits = setFromList [OstInEdhil, Ruins]
    , cdKeywords = setFromList [Scour]
    }

ravenStronghold :: CardDef
ravenStronghold =
  (location "the-antlered-crown-nightmare-the-antlered-crown-005" "Raven Stronghold" TheAntleredCrownNightmare 4 2 5)
    { cdCardTraits = setFromList [Dunland]
    , cdKeywords = setFromList [Time 3]
    }

highlandsOfDunland :: CardDef
highlandsOfDunland =
  (location "the-antlered-crown-nightmare-the-antlered-crown-006" "Highlands of Dunland" TheAntleredCrownNightmare 2 4 4)
    { cdCardTraits = setFromList [Dunland]
    , cdShadowEffect = True
    , cdKeywords = setFromList [Time 2]
    }

-- Shadows of Mirkwood Nightmare Decks

watchedPath :: CardDef
watchedPath =
  (location "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-004" "Watched Path" TheHuntForGollumNightmare 2 3 4)
    { cdCardTraits = setFromList [Forest, Road]
    , cdShadowEffect = True
    }

huntersLookout :: CardDef
huntersLookout =
  (location "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-005" "Hunter's Lookout" TheHuntForGollumNightmare 2 2 6)
    { cdCardTraits = setFromList [Riverland]
    , cdShadowEffect = True
    }

darkPools :: CardDef
darkPools =
  (location "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-006" "Dark Pools" TheHuntForGollumNightmare 3 3 2)
    { cdCardTraits = setFromList [Marshland]
    }

trollLair :: CardDef
trollLair =
  (location "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-008" "Troll Lair" ConflictAtTheCarrockNightmare 3 2 5)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

hivesAndHives :: CardDef
hivesAndHives =
  (location "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-009" "Hives and Hives" ConflictAtTheCarrockNightmare 3 3 3)
    { cdCardTraits = setFromList [WilderlandsTrait]
    , cdShadowEffect = True
    }

darkWoodGrove :: CardDef
darkWoodGrove =
  (location "a-journey-to-rhosgobel-nightmare-a-journey-to-rhosgobel-004" "Dark-wood Grove" AJourneyToRhosgobelNightmare 3 3 4)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

labyrinthineWoods :: CardDef
labyrinthineWoods =
  (location "a-journey-to-rhosgobel-nightmare-a-journey-to-rhosgobel-005" "Labyrinthine Woods" AJourneyToRhosgobelNightmare 3 2 5)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

pitchDarkThicket :: CardDef
pitchDarkThicket =
  (location "a-journey-to-rhosgobel-nightmare-a-journey-to-rhosgobel-006" "Pitch-dark Thicket" AJourneyToRhosgobelNightmare 2 4 2)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

bramblesAndThorns :: CardDef
bramblesAndThorns =
  (location "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-004" "Brambles and Thorns" TheHillsOfEmynMuilNightmare 3 3 5)
    { cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 3
    }

sarnGebirRapids :: CardDef
sarnGebirRapids =
  (location "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-005" "Sarn Gebir Rapids" TheHillsOfEmynMuilNightmare 2 5 5)
    { cdCardTraits = setFromList [EmynMuil]
    , cdVictoryPoints = Just 2
    }

theMereOfDeadFaces :: CardDef
theMereOfDeadFaces =
  (unique (location "the-dead-marshes-nightmare-the-dead-marshes-002" "The Mere of Dead Faces" TheDeadMarshesNightmare 1 0 0))
    { cdCardTraits = setFromList [DeadMarshes]
    , cdQuestPoints = Nothing
    }

stagnantQuagmire :: CardDef
stagnantQuagmire =
  (location "the-dead-marshes-nightmare-the-dead-marshes-003" "Stagnant Quagmire" TheDeadMarshesNightmare 3 3 5)
    { cdCardTraits = setFromList [DeadMarshes]
    , cdEscape = Just 2
    , cdShadowEffect = True
    }

deceptivePath :: CardDef
deceptivePath =
  (location "return-to-mirkwood-nightmare-return-to-mirkwood-005" "Deceptive Path" ReturnToMirkwoodNightmare 2 4 1)
    { cdCardTraits = setFromList [Forest]
    }

webWall :: CardDef
webWall =
  (location "return-to-mirkwood-nightmare-return-to-mirkwood-006" "Web-wall" ReturnToMirkwoodNightmare 3 3 6)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

-- The Morgul Vale Nightmare

banksOfMorgulduin :: CardDef
banksOfMorgulduin =
  (location "the-morgul-vale-nightmare-the-morgul-vale-005" "Banks of Morgulduin" TheMorgulValeNightmare 2 4 4)
    { cdCardTraits = setFromList [River]
    , cdShadowEffect = True
    }

ephelDuath :: CardDef
ephelDuath =
  (location "the-morgul-vale-nightmare-the-morgul-vale-006" "Ephel Dúath" TheMorgulValeNightmare 3 2 5)
    { cdCardTraits = setFromList [Mountain]
    , cdShadowEffect = True
    }

whiteFloweredFields :: CardDef
whiteFloweredFields =
  (location "the-morgul-vale-nightmare-the-morgul-vale-007" "White-Flowered Fields" TheMorgulValeNightmare 2 3 3)
    { cdCardTraits = setFromList [Valley]
    , cdShadowEffect = True
    }

-- The Blood of Gondor Nightmare

-- | Archery X, X the highest number of hidden cards in any player's play area.
edgeOfIthilien :: CardDef
edgeOfIthilien =
  (location "the-blood-of-gondor-nightmare-the-blood-of-gondor-005" "Edge of Ithilien" TheBloodOfGondorNightmare 3 3 6)
    { cdCardTraits = setFromList [Ithilien, Forest]
    , cdShadowEffect = True
    }

mountainsOfShadow :: CardDef
mountainsOfShadow =
  (location "the-blood-of-gondor-nightmare-the-blood-of-gondor-006" "Mountains of Shadow" TheBloodOfGondorNightmare 3 4 4)
    { cdCardTraits = setFromList [Mountain]
    , cdShadowEffect = True
    }

-- The Drúadan Forest Nightmare

devilsMirk :: CardDef
devilsMirk =
  (location "the-druadan-forest-nightmare-the-druadan-forest-005" "Devil's Mirk" TheDruadanForestNightmare 3 3 3)
    { cdCardTraits = setFromList [Forest]
    }

eilenach :: CardDef
eilenach =
  (location "the-druadan-forest-nightmare-the-druadan-forest-006" "Eilenach" TheDruadanForestNightmare 2 5 4)
    { cdCardTraits = setFromList [Forest, Hills]
    , cdShadowEffect = True
    }

stonewainValley :: CardDef
stonewainValley =
  (location "the-druadan-forest-nightmare-the-druadan-forest-007" "Stonewain Valley" TheDruadanForestNightmare 2 1 6)
    { cdCardTraits = setFromList [Forest]
    }

-- Encounter at Amon Dîn Nightmare

besiegedHomestead :: CardDef
besiegedHomestead =
  (location "encounter-at-amon-din-nightmare-encounter-at-amon-din-004" "Besieged Homestead" EncounterAtAmonDinNightmare 3 4 6)
    { cdCardTraits = setFromList [Gondor]
    , cdVictoryPoints = Just 1
    }

overrunVillage :: CardDef
overrunVillage =
  (location "encounter-at-amon-din-nightmare-encounter-at-amon-din-005" "Overrun Village" EncounterAtAmonDinNightmare 2 3 5)
    { cdCardTraits = setFromList [Gondor]
    , cdVictoryPoints = Just 1
    }

-- Assault on Osgiliath Nightmare

-- | X threat, X the resource tokens on it.
palaceRemains :: CardDef
palaceRemains =
  unique
    (location "assault-on-osgiliath-nightmare-assault-on-osgiliath-004" "Palace Remains" AssaultOnOsgiliathNightmare 1 0 5)
      { cdCardTraits = setFromList [Osgiliath]
      }

crumbledBattlements :: CardDef
crumbledBattlements =
  (location "assault-on-osgiliath-nightmare-assault-on-osgiliath-005" "Crumbled Battlements" AssaultOnOsgiliathNightmare 2 3 2)
    { cdCardTraits = setFromList [Osgiliath]
    }

easternQuarter :: CardDef
easternQuarter =
  (location "assault-on-osgiliath-nightmare-assault-on-osgiliath-006" "Eastern Quarter" AssaultOnOsgiliathNightmare 2 3 3)
    { cdCardTraits = setFromList [Osgiliath]
    }

westernQuarter :: CardDef
westernQuarter =
  (location "assault-on-osgiliath-nightmare-assault-on-osgiliath-007" "Western Quarter" AssaultOnOsgiliathNightmare 2 2 4)
    { cdCardTraits = setFromList [Osgiliath]
    }

-- The Steward's Fear Nightmare

traitorsDen :: CardDef
traitorsDen =
  (location "the-stewards-fear-nightmare-the-stewards-fear-006" "Traitors' Den" TheStewardsFearNightmare 2 3 6)
    { cdCardTraits = setFromList [Underworld, City]
    }

rathCelerdain :: CardDef
rathCelerdain =
  (location "the-stewards-fear-nightmare-the-stewards-fear-007" "Rath Celerdain" TheStewardsFearNightmare 2 2 5)
    { cdCardTraits = setFromList [City]
    }

archedTunnel :: CardDef
archedTunnel =
  (location "the-stewards-fear-nightmare-the-stewards-fear-008" "Arched Tunnel" TheStewardsFearNightmare 2 4 3)
    { cdCardTraits = setFromList [Underworld, City]
    }

nestOfHorrors :: CardDef
nestOfHorrors =
  (location "foundations-of-stone-nightmare-foundations-of-stone-006" "Nest of Horrors" FoundationsOfStoneNightmare 3 4 4)
    { cdCardTraits = setFromList [Underground, Dark]
    }

shiveringRiver :: CardDef
shiveringRiver =
  (location "foundations-of-stone-nightmare-foundations-of-stone-007" "Shivering River" FoundationsOfStoneNightmare 3 3 7)
    { cdCardTraits = setFromList [Underground, River]
    , cdShadowEffect = True
    }

fieryDepths :: CardDef
fieryDepths =
  (location "shadow-and-flame-nightmare-shadow-and-flame-009" "Fiery Depths" ShadowAndFlameNightmare 2 5 2)
    { cdCardTraits = setFromList [Underground]
    }

crumblingStairs :: CardDef
crumblingStairs =
  (location "shadow-and-flame-nightmare-shadow-and-flame-010" "Crumbling Stairs" ShadowAndFlameNightmare 2 2 7)
    { cdCardTraits = setFromList [Underground]
    , cdShadowEffect = True
    }

snowyBadlands :: CardDef
snowyBadlands =
  (location "the-wastes-of-eriador-nightmare-the-wastes-of-eriador-004" "Snowy Badlands" TheWastesOfEriadorNightmare 3 3 5)
    { cdCardTraits = setFromList [Arnor, Hills]
    }

-- Escape from Mount Gram Nightmare

prisonCellNightmare :: CardDef
prisonCellNightmare =
  (location "escape-from-mount-gram-nightmare-escape-from-mount-gram-004" "Prison Cell" EscapeFromMountGramNightmare 4 2 3)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Capture 1]
    }

-- | X quest points, twice the cards captured underneath it.
tortureChamberMountGram :: CardDef
tortureChamberMountGram =
  (location "escape-from-mount-gram-nightmare-escape-from-mount-gram-005" "Torture Chamber" EscapeFromMountGramNightmare 2 0 0)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Capture 1, Surge]
    }

-- Across the Ettenmoors Nightmare

rockyFoothills :: CardDef
rockyFoothills =
  (location "across-the-ettenmoors-nightmare-across-the-ettenmoors-006" "Rocky Foothills" AcrossTheEttenmoorsNightmare 2 2 2)
    { cdCardTraits = setFromList [Ettenmoors]
    , cdShadowEffect = True
    }

vastTrollFells :: CardDef
vastTrollFells =
  (location "across-the-ettenmoors-nightmare-across-the-ettenmoors-007" "Vast Troll-fells" AcrossTheEttenmoorsNightmare 2 3 4)
    { cdCardTraits = setFromList [Ettenmoors]
    , cdShadowEffect = True
    }

-- The Treachery of Rhudaur Nightmare

forgottenCourtyard :: CardDef
forgottenCourtyard =
  (location "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-004" "Forgotten Courtyard" TheTreacheryOfRhudaurNightmare 2 1 6)
    { cdCardTraits = setFromList [Ruins]
    }

charredCellar :: CardDef
charredCellar =
  (location "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-005" "Charred Cellar" TheTreacheryOfRhudaurNightmare 2 5 4)
    { cdCardTraits = setFromList [Ruins]
    }

-- | Of the Sift through the Debris encounter set, as its icon shows (cards.json has it as
-- a treachery; the card is a location).
secretAntechamber :: CardDef
secretAntechamber =
  (location "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-008" "Secret Antechamber" TheTreacheryOfRhudaurNightmare 3 2 5)
    { cdCardTraits = setFromList [Ruins]
    }

-- The Battle of Carn Dûm Nightmare

strongholdTower :: CardDef
strongholdTower =
  (location "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-004" "Stronghold Tower" TheBattleOfCarnDumNightmare 3 3 4)
    { cdCardTraits = setFromList [CarnDum]
    }

gatesOfCarnDum :: CardDef
gatesOfCarnDum =
  (location "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-005" "Gates of Carn Dûm" TheBattleOfCarnDumNightmare 2 5 2)
    { cdCardTraits = setFromList [CarnDum]
    , cdShadowEffect = True
    }

-- The Dread Realm Nightmare

innerSanctum :: CardDef
innerSanctum =
  (location "the-dread-realm-nightmare-the-dread-realm-004" "Inner Sanctum" TheDreadRealmNightmare 2 6 4)
    { cdCardTraits = setFromList [CarnDum, Underground]
    }

cursedHalls :: CardDef
cursedHalls =
  (location "the-dread-realm-nightmare-the-dread-realm-005" "Cursed Halls" TheDreadRealmNightmare 3 3 5)
    { cdCardTraits = setFromList [CarnDum, Underground]
    , cdShadowEffect = True
    }

-- * The Sands of Harad: the shared encounter sets (Desert Sands, Jungle Forest). Easy mode
-- copy counts from Hall of Beorn's scenario pages.

toweringDunes :: CardDef
toweringDunes = (location "16049" "Towering Dunes" DesertSands 3 3 4) {cdCardTraits = setFromList [Desert], cdEasyModeQuantity = Just 2}

desertOasis :: CardDef
desertOasis = (location "16050" "Desert Oasis" DesertSands 2 2 2) {cdCardTraits = setFromList [Desert]}

jungleTrail :: CardDef
jungleTrail = (location "16056" "Jungle Trail" JungleForest 2 4 4) {cdCardTraits = setFromList [Forest], cdEasyModeQuantity = Just 1}

forestClearing :: CardDef
forestClearing =
  (location "16057" "Forest Clearing" JungleForest 3 3 2)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

denseJungle :: CardDef
denseJungle =
  (location "16058" "Dense Jungle" JungleForest 3 2 3)
    { cdCardTraits = setFromList [Forest]
    , cdShadowEffect = True
    }

-- | X threat, X the main quest's stage number: its card gives it.
southronBoma :: CardDef
southronBoma =
  (location "16036" "Southron Boma" TheLongArmOfMordor 2 0 4)
    { cdCardTraits = setFromList [Forest, Village]
    , cdShadowEffect = True
    }

villageHut :: CardDef
villageHut = (location "16037" "Village Hut" TheLongArmOfMordor 2 2 2) {cdCardTraits = setFromList [Forest, Village]}
-- * Beneath the Sands

spiderBurrow :: CardDef
spiderBurrow = (location "17068" "Spider Burrow" BeneathTheSands 2 3 2) {cdCardTraits = setFromList [Underground]}

-- | X threat, from its own modifiers. On Track printed in its lower right corner.
pitchBlackTunnel :: CardDef
pitchBlackTunnel =
  (location "17069" "Pitch Black Tunnel" BeneathTheSands 2 0 5)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Track 4]
    }

-- | On Track printed in its lower right corner.
nestOfVermin :: CardDef
nestOfVermin =
  (location "17070" "Nest of Vermin" BeneathTheSands 2 3 4)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Track 3]
    }

cobwebbedCavern :: CardDef
cobwebbedCavern =
  (location "17071" "Cobwebbed Cavern" BeneathTheSands 3 2 3)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Track 2]
    , cdShadowEffect = True
    }

-- | On Track printed in its lower right corner.
blockedPassage :: CardDef
blockedPassage =
  (location "17072" "Blocked Passage" BeneathTheSands 3 2 6)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Track 2]
    , cdShadowEffect = True
    }

-- * Desert Crossing (The Sands of Harad). Easy mode copy counts from Hall of Beorn's
-- scenario page.

burntSands :: CardDef
burntSands = (location "16025" "Burnt Sands" DesertCrossing 4 2 3) {cdCardTraits = setFromList [Desert]}

scorchedHamada :: CardDef
scorchedHamada =
  (location "16026" "Scorched Hamada" DesertCrossing 2 0 4)
    { cdCardTraits = setFromList [Desert]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

parchedWadi :: CardDef
parchedWadi = (location "16027" "Parched Wadi" DesertCrossing 2 3 3) {cdCardTraits = setFromList [Desert]}

-- * Escape from Umbar. Easy mode copy counts from Hall of Beorn's scenario page.

narrowAlleyway :: CardDef
narrowAlleyway = (location "16016" "Narrow Alleyway" EscapeFromUmbar 2 2 2) {cdCardTraits = setFromList [City]}

umbarStreet :: CardDef
umbarStreet =
  (location "16017" "Umbar Street" EscapeFromUmbar 2 3 3)
    { cdCardTraits = setFromList [City]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

umbarSquare :: CardDef
umbarSquare = (location "16018" "Umbar Square" EscapeFromUmbar 3 2 3) {cdCardTraits = setFromList [City]}

-- | Its threat is X, the number of engaged enemies.
portOfHarad :: CardDef
portOfHarad = (location "16019" "Port of Harad" EscapeFromUmbar 2 0 4) {cdCardTraits = setFromList [City]}

haradrimEstate :: CardDef
haradrimEstate = (location "16020" "Haradrim Estate" EscapeFromUmbar 2 3 1) {cdCardTraits = setFromList [City]}

-- * Race Across Harad

riverHarnen :: CardDef
riverHarnen =
  unique
    (location "17039" "River Harnen" RaceAcrossHarad 1 4 10)
      { cdCardTraits = setFromList [Desert, River]
      , cdVictoryPoints = Just 4
      }

expanseOfHarad :: CardDef
expanseOfHarad = (location "17040" "Expanse of Harad" RaceAcrossHarad 2 2 5) {cdCardTraits = setFromList [Desert]}

blisteringErg :: CardDef
blisteringErg = (location "17041" "Blistering Erg" RaceAcrossHarad 3 0 4) {cdCardTraits = setFromList [Desert]}

haradRoad :: CardDef
haradRoad =
  (location "17042" "Harad Road" RaceAcrossHarad 2 5 5)
    { cdCardTraits = setFromList [Desert, Road]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

-- * The Black Serpent. Easy mode copy counts from Hall of Beorn's scenario page.

theSerpentsRedoubt :: CardDef
theSerpentsRedoubt =
  (location "17096" "The Serpent's Redoubt" TheBlackSerpent 2 4 4)
    { cdCardTraits = setFromList [Desert, Ruins]
    , cdEasyModeQuantity = Just 1
    }

rooflessTower :: CardDef
rooflessTower =
  (location "17097" "Roofless Tower" TheBlackSerpent 2 2 4)
    { cdCardTraits = setFromList [Desert, Ruins]
    , cdEasyModeQuantity = Just 1
    }

desertBastion :: CardDef
desertBastion =
  (location "17098" "Desert Bastion" TheBlackSerpent 3 1 5)
    { cdCardTraits = setFromList [Desert, Ruins]
    , cdShadowEffect = True
    }

southronCamp :: CardDef
southronCamp = (location "17099" "Southron Camp" TheBlackSerpent 3 3 3) {cdCardTraits = setFromList [Desert, Ruins]}

brokenCourtyard :: CardDef
brokenCourtyard =
  (location "17100" "Broken Courtyard" TheBlackSerpent 3 1 3)
    { cdCardTraits = setFromList [Desert, Ruins]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

-- The Dungeons of Cirith Gurat (17124-17128)

gateOfIron :: CardDef
gateOfIron = (location "17124" "Gate of Iron" TheDungeonsOfCirithGurat 3 3 2) {cdCardTraits = setFromList [Dungeon]}

dungeonCell :: CardDef
dungeonCell =
  (location "17125" "Dungeon Cell" TheDungeonsOfCirithGurat 3 1 5)
    { cdCardTraits = setFromList [Dungeon]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 2
    }

urukGuardroom :: CardDef
urukGuardroom = (location "17126" "Uruk Guardroom" TheDungeonsOfCirithGurat 2 4 4) {cdCardTraits = setFromList [Dungeon]}

minesOfCirithGurat :: CardDef
minesOfCirithGurat = (location "17127" "Mines of Cirith Gurat" TheDungeonsOfCirithGurat 3 2 3) {cdCardTraits = setFromList [Dungeon]}

tortureRoom :: CardDef
tortureRoom =
  (location "17128" "Torture Room" TheDungeonsOfCirithGurat 2 3 4)
    { cdCardTraits = setFromList [Dungeon]
    , cdWhenRevealed = True
    }

-- The Mûmakil

mumakTrail :: CardDef
mumakTrail = (location "17020" "Mûmak Trail" TheMumakil 4 2 3) {cdCardTraits = setFromList [Forest]}

-- * The Crossings of Poros: its two encounter sets, The Crossings of Poros and Mountains of
-- Shadow. Easy mode copy counts from Hall of Beorn's scenario page.

-- | Its threat is X, worked out by its card.
desolateLand :: CardDef
desolateLand = (location "17150" "Desolate Land" TheCrossingsOfPoros 3 0 4) {cdCardTraits = setFromList [Desert, Hills], cdEasyModeQuantity = Just 1}

harondorWaste :: CardDef
harondorWaste = (location "17151" "Harondor Waste" TheCrossingsOfPoros 4 2 3) {cdCardTraits = setFromList [Desert, Hills], cdShadowEffect = True}

foothillsOfMordor :: CardDef
foothillsOfMordor = (location "17153" "Foothills of Mordor" MountainsOfShadow 3 3 3) {cdCardTraits = setFromList [Hills], cdEasyModeQuantity = Just 2}

shadowyVale :: CardDef
shadowyVale = (location "17154" "Shadowy Vale" MountainsOfShadow 2 2 5) {cdCardTraits = setFromList [Hills], cdEasyModeQuantity = Just 1}

-- * The Wilds of Rhovanion's shared encounter sets

-- | X threat and X quest points: see "LordOfTheRings.Location.Cards.HillsOfWilderland".
hillsOfWilderland :: CardDef
hillsOfWilderland = (location "18023" "Hills of Wilderland" HillsOfWilderland 2 0 0) {cdCardTraits = setFromList [Hills], cdEasyModeQuantity = Just 1}

lonelyLands :: CardDef
lonelyLands = (location "18024" "Lonely Lands" HillsOfWilderland 3 2 5) {cdCardTraits = setFromList [Hills], cdEasyModeQuantity = Just 2}

accursedForest :: CardDef
accursedForest = (location "18038" "Accursed Forest" DarkWoods 2 4 4) {cdCardTraits = setFromList [Forest, Dark], cdEasyModeQuantity = Just 1}

darkBlackWoods :: CardDef
darkBlackWoods = (location "18039" "Dark Black Woods" DarkWoods 3 3 3) {cdCardTraits = setFromList [Forest, Dark], cdEasyModeQuantity = Just 2}

darkTunnel :: CardDef
darkTunnel = (location "18054" "Dark Tunnel" DeepUnderground 4 3 3) {cdCardTraits = setFromList [Underground, Dark], cdKeywords = setFromList [Deep], cdEasyModeQuantity = Just 3}

forkedPassage :: CardDef
forkedPassage = (location "18055" "Forked Passage" DeepUnderground 2 2 2) {cdCardTraits = setFromList [Underground, Dark], cdKeywords = setFromList [Deep]}

-- | The Lost Caves set is the Caves deck (MEC65 rules), never shuffled into the encounter
-- deck.
lostArmory :: CardDef
lostArmory = (location "18060" "Lost Armory" LostCaves 1 2 2) {cdCardTraits = setFromList [Underground], cdVictoryPoints = Just 1}

ancientTreasury :: CardDef
ancientTreasury = (location "18061" "Ancient Treasury" LostCaves 1 3 3) {cdCardTraits = setFromList [Underground], cdVictoryPoints = Just 1}

frightfulDen :: CardDef
frightfulDen = (location "18062" "Frightful Den" LostCaves 1 4 4) {cdCardTraits = setFromList [Underground, Dark], cdVictoryPoints = Just 1}

lightlessGrotto :: CardDef
lightlessGrotto = (location "18063" "Lightless Grotto" LostCaves 1 5 5) {cdCardTraits = setFromList [Underground, Dark], cdVictoryPoints = Just 1}

crumblingCavern :: CardDef
crumblingCavern = (location "18064" "Crumbling Cavern" LostCaves 1 6 6) {cdCardTraits = setFromList [Underground, Dark], cdVictoryPoints = Just 1}

-- | X threat and X quest points, as Hills of Wilderland's.
undergroundLake :: CardDef
undergroundLake = (location "18065" "Underground Lake" LostCaves 1 0 0) {cdCardTraits = setFromList [Underground, Dark], cdVictoryPoints = Just 1}

-- | Journey Up the Anduin's Woodmen Village, whose other side is Haldan (18069B): set aside,
-- out of play, until stage 2A adds it to the staging area.
woodmenVillage :: CardDef
woodmenVillage =
  unique
    $ (location "18069A" "Woodmen Village" JourneyUpTheAnduin 1 4 4)
      { cdCardTraits = setFromList [Riverland]
      , cdOtherSide = Just "18069B"
      }

-- * The Fate of Wilderland

gateOfGundabad :: CardDef
gateOfGundabad =
  unique
    (location "19161" "Gate of Gundabad" TheFateOfWilderland 1 4 4)
      { cdCardTraits = setFromList [Mountain, Gate]
      , cdVictoryPoints = Just 1
      }

hilltopBattlements :: CardDef
hilltopBattlements = (location "19162" "Hilltop Battlements" TheFateOfWilderland 3 3 3) {cdCardTraits = setFromList [Mountain, Fortification]}

dwarvenWatchtower :: CardDef
dwarvenWatchtower =
  (location "19163" "Dwarven Watchtower" TheFateOfWilderland 3 2 5)
    { cdCardTraits = setFromList [Mountain, Fortification]
    , cdVictoryPoints = Just 2
    }

slopesOfGundabad :: CardDef
slopesOfGundabad =
  (location "19164" "Slopes of Gundabad" TheFateOfWilderland 3 2 4)
    { cdCardTraits = setFromList [Mountain]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- * Roam Across Rhovanion

greyMoorland :: CardDef
greyMoorland =
  (location "19041" "Grey Moorland" RoamAcrossRhovanion 2 3 4)
    { cdCardTraits = setFromList [Hills]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | Journey Up the Anduin's, not The Hunt for Gollum's: set aside, out of play, until
-- stage 3A adds it to the staging area.
theOldFordJourneyUpTheAnduin :: CardDef
theOldFordJourneyUpTheAnduin =
  unique
    $ (location "18016" "The Old Ford" JourneyUpTheAnduin 1 5 5)
      { cdCardTraits = setFromList [Riverland]
      }

woodedRiverbank :: CardDef
woodedRiverbank =
  (location "18017" "Wooded Riverbank" JourneyUpTheAnduin 3 3 1)
    { cdCardTraits = setFromList [Riverland]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }
-- * Fire in the Night

townGate :: CardDef
townGate = (location "19077" "Town Gate" FireInTheNight 2 3 4) {cdCardTraits = setFromList [Town], cdEasyModeQuantity = Just 1}

woodenPalisade :: CardDef
woodenPalisade = (location "19078" "Wooden Palisade" FireInTheNight 3 2 3) {cdCardTraits = setFromList [Town]}

burningWatchtower :: CardDef
burningWatchtower = (location "19079" "Burning Watchtower" FireInTheNight 3 0 5) {cdCardTraits = setFromList [Town], cdEasyModeQuantity = Just 1}
dragonHoard :: CardDef
dragonHoard = unique (location "18043" "Dragon Hoard" TheKingsQuest 1 7 7) {cdCardTraits = setFromList [Underground, Dark, Lair], cdVictoryPoints = Just 1}

ironHillsMine :: CardDef
ironHillsMine = (location "18044" "Iron Hills Mine" TheKingsQuest 2 4 4) {cdCardTraits = setFromList [Underground], cdEasyModeQuantity = Just 1}

contestedDepths :: CardDef
contestedDepths = (location "18045" "Contested Depths" TheKingsQuest 3 1 5) {cdCardTraits = setFromList [Underground]}

deepChasm :: CardDef
deepChasm =
  (location "18046" "Deep Chasm" TheKingsQuest 2 2 6)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdKeywords = setFromList [Deep]
    , cdEasyModeQuantity = Just 1
    }

-- * The Ghost of Framsburg (19094-19111)

theCursedTomb :: CardDef
theCursedTomb =
  unique
    (location "19097" "The Cursed Tomb" TheGhostOfFramsburg 1 5 5)
      { cdCardTraits = setFromList [Ruins, Dark]
      , cdVictoryPoints = Just 5
      }

forgottenCrypt :: CardDef
forgottenCrypt =
  (location "19101" "Forgotten Crypt" TheGhostOfFramsburg 2 2 2)
    { cdCardTraits = setFromList [Ruins, Dark]
    , cdVictoryPoints = Just 2
    }

hauntedHall :: CardDef
hauntedHall =
  (location "19102" "Haunted Hall" TheGhostOfFramsburg 2 4 4)
    { cdCardTraits = setFromList [Ruins, Dark]
    , cdKeywords = setFromList [Discover 4]
    , cdEasyModeQuantity = Just 1
    }

-- | Discover X, and X threat: its card gives itself X.
hiddenStaircase :: CardDef
hiddenStaircase =
  (location "19103" "Hidden Staircase" TheGhostOfFramsburg 2 0 4)
    { cdCardTraits = setFromList [Ruins, Dark, Hazard]
    , cdKeywords = setFromList [Discover 0]
    }

moonlitPassage :: CardDef
moonlitPassage =
  (location "19104" "Moonlit Passage" TheGhostOfFramsburg 3 2 4)
    { cdCardTraits = setFromList [Ruins]
    , cdKeywords = setFromList [Discover 2]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

-- * Mount Gundabad. Easy mode copy counts from Hall of Beorn.

theFirstForge :: CardDef
theFirstForge = unique (location "19122" "The First Forge" MountGundabad 1 1 1) {cdCardTraits = setFromList [Underground], cdVictoryPoints = Just 1}

dagnirsHoard :: CardDef
dagnirsHoard = unique (location "19123" "Dagnir's Hoard" MountGundabad 1 7 7) {cdCardTraits = setFromList [Underground], cdVictoryPoints = Just 1}

throatOfTheMountain :: CardDef
throatOfTheMountain =
  unique (location "19124" "Throat of the Mountain" MountGundabad 1 4 4) {cdCardTraits = setFromList [Underground, Dark], cdVictoryPoints = Just 1}

minesOfGundabad :: CardDef
minesOfGundabad =
  (location "19129" "Mines of Gundabad" MountGundabad 3 3 3)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdKeywords = setFromList [Deep]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

cursedTower :: CardDef
cursedTower =
  (location "19105" "Cursed Tower" TheGhostOfFramsburg 3 3 3)
    { cdCardTraits = setFromList [Ruins, Dark]
    , cdKeywords = setFromList [Discover 3]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

narrowStair :: CardDef
narrowStair =
  (location "19130" "Narrow Stair" MountGundabad 2 2 3)
    { cdCardTraits = setFromList [Underground]
    , cdKeywords = setFromList [Deep]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

desecratedRuins :: CardDef
desecratedRuins =
  (location "19131" "Desecrated Ruins" MountGundabad 2 2 4)
    { cdCardTraits = setFromList [Underground, Dark]
    , cdKeywords = setFromList [Deep]
    , cdShadowEffect = True
    }
-- * The Withered Heath

mountainPass :: CardDef
mountainPass = (location "19015" "Mountain Pass" TheWitheredHeath 3 2 4) {cdCardTraits = setFromList [Mountain]}

dwarvenDoor :: CardDef
dwarvenDoor = (location "19016" "Dwarven Door" TheWitheredHeath 3 3 3) {cdCardTraits = setFromList [Mountain], cdKeywords = setFromList [Deep]}

caveEntrance :: CardDef
caveEntrance =
  (location "19017" "Cave Entrance" TheWitheredHeath 3 1 5)
    { cdCardTraits = setFromList [Mountain]
    , cdKeywords = setFromList [Deep]
    , cdEasyModeQuantity = Just 2
    }

creatureDen :: CardDef
creatureDen = (location "19018" "Creature Den" TheWitheredHeath 2 4 4) {cdCardTraits = setFromList [Mountain], cdVictoryPoints = Just 4}

-- | X threat: the number of locations in the staging area.
highFalls :: CardDef
highFalls =
  (location "19019" "High Falls" TheWitheredHeath 2 0 6)
    { cdCardTraits = setFromList [Mountain]
    , cdKeywords = setFromList [Deep]
    , cdEasyModeQuantity = Just 1
    }
deepRavine :: CardDef
deepRavine = (location "19042" "Deep Ravine" RoamAcrossRhovanion 3 2 3) {cdCardTraits = setFromList [Hills], cdEasyModeQuantity = Just 2}

slopesOfEredMithrin :: CardDef
slopesOfEredMithrin =
  (location "19043" "Slopes of Ered Mithrin" RoamAcrossRhovanion 3 2 4)
    { cdCardTraits = setFromList [Hills]
    , cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    }
-- | An objective-location of Lost in Mirkwood: Guarded (X), worth 4 victory points. The
-- scenario's Searching for a Way Out makes it immune to player card effects.
objectiveLocation :: CardCode -> Name -> CardType -> CardDef
objectiveLocation code name guardType =
  unique
    $ (location code name LostInMirkwood 1 0 4)
      { cdCardTraits = setFromList [Forest]
      , cdKeywords = setFromList [GuardedBy [guardType]]
      , cdVictoryPoints = Just 4
      , cdThreat = Nothing
      }

overgrownPath :: CardDef
overgrownPath = objectiveLocation "18029" "Overgrown Path" LocationType

abandonedVillage :: CardDef
abandonedVillage = objectiveLocation "18030" "Abandoned Village" LocationType

bareHilltop :: CardDef
bareHilltop = objectiveLocation "18031" "Bare Hilltop" EnemyType

forestClearingLostInMirkwood :: CardDef
forestClearingLostInMirkwood = objectiveLocation "18032" "Forest Clearing" EnemyType

twilightHall :: CardDef
twilightHall = (location "18033" "Twilight Hall" LostInMirkwood 3 2 6) {cdCardTraits = setFromList [Forest, Dark], cdEasyModeQuantity = Just 2}

-- * The Power of Mordor

-- | The Power of Mordor deck's: its quest points are printed "-".
theTowerOfBaradDur :: CardDef
theTowerOfBaradDur =
  unique
    (location "21067" "The Tower of Barad-dûr" ThePowerOfMordor 1 5 0)
      { cdCardTraits = setFromList [Mordor]
      , cdQuestPoints = Nothing
      }

-- * A Shadow in the East's shared encounter sets

rollingPlains :: CardDef
rollingPlains =
  (location "21025" "Rolling Plains" RollingPlains 4 2 4)
    { cdCardTraits = setFromList [Plains]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 3
    }

rockyOutcrop :: CardDef
rockyOutcrop = (location "21026" "Rocky Outcrop" RollingPlains 2 3 3) {cdCardTraits = setFromList [Plains], cdEasyModeQuantity = Just 1}

cityForum :: CardDef
cityForum = (location "21046" "City Forum" CityOfRhun 2 1 5) {cdCardTraits = setFromList [City]}

seedyTavern :: CardDef
seedyTavern = (location "21047" "Seedy Tavern" CityOfRhun 2 2 5) {cdCardTraits = setFromList [City], cdVictoryPoints = Just 2}

sideStreet :: CardDef
sideStreet = (location "21049" "Side Street" UnderGuard 2 3 3) {cdCardTraits = setFromList [City], cdEasyModeQuantity = Just 1}

catacombs :: CardDef
catacombs = (location "21050" "Catacombs" UnderGuard 2 4 4) {cdCardTraits = setFromList [City], cdEasyModeQuantity = Just 1}

twistedTunnel :: CardDef
twistedTunnel =
  (location "21061" "Twisted Tunnel" TwistedTunnels 2 2 2)
    { cdCardTraits = setFromList [Underground]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

narrowOpening :: CardDef
narrowOpening = (location "21062" "Narrow Opening" TwistedTunnels 2 3 3) {cdCardTraits = setFromList [Underground]}

crumblingPassage :: CardDef
crumblingPassage =
  (location "21063" "Crumbling Passage" TwistedTunnels 2 5 3)
    { cdCardTraits = setFromList [Underground]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 1
    }

-- * The River Running

riverRunning :: CardDef
riverRunning = (location "21014" "River Running" TheRiverRunning 2 2 5) {cdCardTraits = setFromList [Riverland]}

exposedRiverbank :: CardDef
exposedRiverbank = (location "21015" "Exposed Riverbank" TheRiverRunning 2 4 4) {cdCardTraits = setFromList [Riverland], cdEasyModeQuantity = Just 1}
templeOfSauron :: CardDef
templeOfSauron =
  (location "21055" "Temple of Sauron" TheTempleOfDoom 2 4 1)
    { cdCardTraits = setFromList [Underground]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | Its threat is X, given by its own modifiers.
hallOfHorrors :: CardDef
hallOfHorrors = (location "21056" "Hall of Horrors" TheTempleOfDoom 3 0 5) {cdCardTraits = setFromList [Underground], cdEasyModeQuantity = Just 2}
-- * The City of Ulfast

theKingsLawn :: CardDef
theKingsLawn = (location "22041" "The King's Lawn" TheCityOfUlfast 2 3 5) {cdCardTraits = setFromList [City]}

guardedArchway :: CardDef
guardedArchway = (location "22042" "Guarded Archway" TheCityOfUlfast 3 2 4) {cdCardTraits = setFromList [City], cdEasyModeQuantity = Just 2}

crowdedMarket :: CardDef
crowdedMarket = (location "22043" "Crowded Market" TheCityOfUlfast 2 2 2) {cdCardTraits = setFromList [City]}

-- | Its threat is X, printed 0 here: see its card.
streetsOfUlfast :: CardDef
streetsOfUlfast =
  (location "22044" "Streets of Ulfast" TheCityOfUlfast 3 0 5)
    { cdCardTraits = setFromList [City]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }
-- * Under the Ash Mountains

-- | X threat, X 1 more than the number of quest cards in play: given by its own modifiers.
fieryChasm :: CardDef
fieryChasm = (location "22100" "Fiery Chasm" UnderTheAshMountains 3 0 5) {cdCardTraits = setFromList [Underground], cdEasyModeQuantity = Just 2}

hideousDeeps :: CardDef
hideousDeeps = (location "22101" "Hideous Deeps" UnderTheAshMountains 2 3 6) {cdCardTraits = setFromList [Underground], cdWhenRevealed = True}

orcPassage :: CardDef
orcPassage = (location "22102" "Orc Passage" UnderTheAshMountains 2 4 4) {cdCardTraits = setFromList [Underground], cdEasyModeQuantity = Just 1}
-- * Danger in Dorwinion

hillsideManor :: CardDef
hillsideManor = (location "21037" "Hillside Manor" DangerInDorwinion 3 2 4) {cdCardTraits = setFromList [City]}
-- | Its threat is X, 1 more than the number of locations the players control.
gateOfDorwinion :: CardDef
gateOfDorwinion = unique $ (location "22014" "Gate of Dorwinion" WrathAndRuin 1 0 5) {cdCardTraits = setFromList [City], cdVictoryPoints = Just 5}

contestedSquare :: CardDef
contestedSquare =
  (location "22015" "Contested Square" WrathAndRuin 3 1 3)
    { cdCardTraits = setFromList [City]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 2
    }

ransackedHouse :: CardDef
ransackedHouse = (location "22016" "Ransacked House" WrathAndRuin 3 2 4) {cdCardTraits = setFromList [City]}

makeshiftFortification :: CardDef
makeshiftFortification = (location "22017" "Makeshift Fortification" WrathAndRuin 3 3 3) {cdCardTraits = setFromList [City], cdEasyModeQuantity = Just 2}

capturedTower :: CardDef
capturedTower = (location "22018" "Captured Tower" WrathAndRuin 2 4 4) {cdCardTraits = setFromList [City], cdEasyModeQuantity = Just 1}
-- * Challenge of the Wainriders

narrowPass :: CardDef
narrowPass =
  (location "22070" "Narrow Pass" ChallengeOfTheWainriders 3 3 5)
    { cdCardTraits = setFromList [Plains]
    , cdRacingResult = Just Straight
    }

crowdedTrack :: CardDef
crowdedTrack =
  (location "22071" "Crowded Track" ChallengeOfTheWainriders 2 0 6)
    { cdCardTraits = setFromList [Plains]
    , cdEasyModeQuantity = Just 1
    , cdRacingResult = Just Straight
    }

ravineLedge :: CardDef
ravineLedge =
  (location "22072" "Ravine Ledge" ChallengeOfTheWainriders 2 4 4)
    { cdCardTraits = setFromList [Plains]
    , cdEasyModeQuantity = Just 1
    , cdRacingResult = Just Turn
    }
-- * The Fortress of Nurn

fortressWallsTheFortressOfNurn :: CardDef
fortressWallsTheFortressOfNurn = (location "22153" "Fortress Walls" TheFortressOfNurn 2 3 6) {cdCardTraits = setFromList [City], cdEasyModeQuantity = Just 1}

guardTower :: CardDef
guardTower = (location "22154" "Guard Tower" TheFortressOfNurn 3 2 4) {cdCardTraits = setFromList [City]}

-- | Its threat is X, 1 more than the Castle side quests in the victory display.
plazaOfNurn :: CardDef
plazaOfNurn = (location "22155" "Plaza of Nurn" TheFortressOfNurn 3 0 5) {cdCardTraits = setFromList [City], cdEasyModeQuantity = Just 2}

-- * The Land of Sorrow

-- | A Camp: it guards a Captive objective ally and hides a Warden (stage 1A).
camp :: CardCode -> Name -> Int -> CardDef
camp code name questPoints =
  unique (location code name TheLandOfSorrow 1 1 questPoints)
    { cdCardTraits = setFromList [Camp]
    , cdVictoryPoints = Just 1
    }

theBarracks :: CardDef
theBarracks = camp "22121" "The Barracks" 5

slaveQuarters :: CardDef
slaveQuarters = camp "22122" "Slave Quarters" 3

theStorehouse :: CardDef
theStorehouse = camp "22123" "The Storehouse" 4

workingFields :: CardDef
workingFields = (location "22129" "Working Fields" TheLandOfSorrow 3 3 4) {cdCardTraits = setFromList [Plains]}

-- | X threat, X 1 more than the Captive objective allies the players control: given by its
-- own modifiers.
watchTowerTheLandOfSorrow :: CardDef
watchTowerTheLandOfSorrow =
  (location "22130" "Watch Tower" TheLandOfSorrow 3 0 5)
    { cdCardTraits = setFromList [Plains]
    , cdEasyModeQuantity = Just 1
    }

-- * The Passing of the Grey Company

-- | Set aside by stage 1A, added to the staging area by stage 3A.
theStoneOfErech :: CardDef
theStoneOfErech = unique (location "145017" "The Stone of Erech" ThePassingOfTheGreyCompany 1 5 5) {cdCardTraits = setFromList [Hill]}

darkDoor :: CardDef
darkDoor = (location "145018" "Dark Door" ThePassingOfTheGreyCompany 2 4 4) {cdCardTraits = setFromList [Underground], cdEasyModeQuantity = Just 1}

trystingPlace :: CardDef
trystingPlace = (location "145019" "Trysting Place" ThePassingOfTheGreyCompany 3 3 3) {cdCardTraits = setFromList [Underground], cdEasyModeQuantity = Just 2}

deadlyRoad :: CardDef
deadlyRoad = (location "145020" "Deadly Road" ThePassingOfTheGreyCompany 3 2 3) {cdCardTraits = setFromList [Underground]}

hauntedPath :: CardDef
hauntedPath = (location "145021" "Haunted Path" ThePassingOfTheGreyCompany 3 1 4) {cdCardTraits = setFromList [Underground], cdShadowEffect = True}

-- * The Ring Goes South

-- | Immune to player card effects (its card gives it the modifier).
redhornPass :: CardDef
redhornPass =
  unique
    (location "142029" "Redhorn Pass" TheRingGoesSouth 1 4 6)
      { cdCardTraits = setFromList [Mountain]
      , cdVictoryPoints = Just 1
      }

-- | Immune to player card effects (its card gives it the modifier).
doorsOfDurinTheRingGoesSouth :: CardDef
doorsOfDurinTheRingGoesSouth =
  unique
    (location "142030" "Doors of Durin" TheRingGoesSouth 1 1 9)
      { cdCardTraits = setFromList [Gate]
      , cdVictoryPoints = Just 1
      }

treeCrownedHill :: CardDef
treeCrownedHill = (location "142031" "Tree-crowned Hill" TheRingGoesSouth 1 3 6) {cdCardTraits = setFromList [Hills]}

hillsOfHollin :: CardDef
hillsOfHollin = (location "142032" "Hills of Hollin" TheRingGoesSouth 3 2 4) {cdCardTraits = setFromList [Hills]}

redhornFoothills :: CardDef
redhornFoothills = (location "142033" "Redhorn Foothills" TheRingGoesSouth 3 4 5) {cdCardTraits = setFromList [Hills], cdEasyModeQuantity = Just 2}

eregion :: CardDef
eregion = (location "142034" "Eregion" TheRingGoesSouth 3 3 4) {cdCardTraits = setFromList [Hills], cdShadowEffect = True, cdEasyModeQuantity = Just 2}

-- * The Passage of the Marshes

marsh :: CardCode -> Name -> Int -> Int -> Int -> Int -> Int -> CardDef
marsh code name quantity threat questPoints mire victory =
  (location code name ThePassageOfTheMarshes quantity threat questPoints)
    { cdCardTraits = setFromList [Marsh]
    , cdKeywords = setFromList [Mire mire]
    , cdVictoryPoints = Just victory
    }

evilSmellingFens :: CardDef
evilSmellingFens = (marsh "144019" "Evil-smelling Fens" 3 3 4 3 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

shiftingQuagmire :: CardDef
shiftingQuagmire = (marsh "144020" "Shifting Quagmire" 2 1 3 1 1) {cdKeywords = setFromList [Surge, Mire 1]}

hauntedMere :: CardDef
hauntedMere = marsh "144021" "Haunted Mere" 3 4 5 2 4

stagnantPools :: CardDef
stagnantPools = marsh "144022" "Stagnant Pools" 2 3 7 5 3

softMires :: CardDef
softMires = (marsh "144023" "Soft Mires" 4 2 4 1 2) {cdEasyModeQuantity = Just 3}

-- * Flies and Spiders

-- | Set aside by stage 1A, until stage 4A adds it to the staging area.
theSpidersGlade :: CardDef
theSpidersGlade = unique $ (location "132025" "The Spider's Glade" FliesAndSpiders 1 1 9) {cdCardTraits = setFromList [Forest]}

giantWeb :: CardDef
giantWeb = (location "132026" "Giant Web" FliesAndSpiders 4 3 3) {cdCardTraits = setFromList [Forest], cdEasyModeQuantity = Just 3}

forestStream :: CardDef
forestStream = (location "132027" "Forest Stream" FliesAndSpiders 2 2 4) {cdCardTraits = setFromList [Forest, River]}

-- * Wilderland

hiddenPath :: CardDef
hiddenPath = (location "132035" "Hidden Path" Wilderland 3 3 1) {cdCardTraits = setFromList [Forest, Mountain]}

-- * The Battle of Five Armies

northernSlopes :: CardDef
northernSlopes = (location "132060" "Northern Slopes" TheBattleOfFiveArmies 3 3 3) {cdCardTraits = setFromList [Mountain], cdEasyModeQuantity = Just 2}

-- | One of the three places each tied to a quest stage, its progress drained each round.
battlefieldLocation :: CardCode -> Name -> [Trait] -> CardDef
battlefieldLocation code name traits =
  unique
    $ (location code name TheBattleOfFiveArmies 1 2 2)
      { cdCardTraits = setFromList traits
      , cdVictoryPoints = Just 3
      }

ruinsOfDale :: CardDef
ruinsOfDale = battlefieldLocation "132061" "Ruins of Dale" [Ruins, Valley]

ravenhill :: CardDef
ravenhill = battlefieldLocation "132062" "Ravenhill" [Ruins, Mountain]

theFrontGate :: CardDef
theFrontGate = battlefieldLocation "132063" "The Front Gate" [Ruins, Mountain]

-- * We Must Away, Ere Break of Day

trollCamp :: CardDef
trollCamp = (location "131036" "Troll Camp" WeMustAwayEreBreakOfDay 3 3 3) {cdCardTraits = setFromList [Forest, Trollshaws]}

-- | Set aside by stage 1A, added to the staging area by stage 2A.
trollCave :: CardDef
trollCave =
  (location "131037" "Troll Cave" WeMustAwayEreBreakOfDay 1 2 4)
    { cdCardTraits = setFromList [Cave, Trollshaws]
    , cdVictoryPoints = Just 2
    }

hobbitLands :: CardDef
hobbitLands =
  (location "131038" "Hobbit-lands" WeMustAwayEreBreakOfDay 2 1 1)
    { cdCardTraits = setFromList [WesternLandsTrait]
    , cdShadowEffect = True
    }

-- * Western Lands

drearyHills :: CardDef
drearyHills = (location "131039" "Dreary Hills" WesternLands 2 3 2) {cdCardTraits = setFromList [WesternLandsTrait]}

loneLands :: CardDef
loneLands = (location "131040" "Lone-Lands" WesternLands 2 2 3) {cdCardTraits = setFromList [WesternLandsTrait]}

caveEntranceWesternLands :: CardDef
caveEntranceWesternLands = (location "131041" "Cave Entrance" WesternLands 2 1 3) {cdCardTraits = setFromList [WesternLandsTrait]}

-- * Misty Mountain Goblins

theGoblinsCaves :: CardDef
theGoblinsCaves =
  (location "131065" "The Goblins' Caves" MistyMountainGoblins 3 3 3)
    { cdCardTraits = setFromList [Cave]
    , cdShadowEffect = True
    }

-- * Over the Misty Mountains Grim

overhangingRock :: CardDef
overhangingRock = (location "131068" "Overhanging Rock" OverTheMistyMountainsGrim 2 2 3) {cdCardTraits = setFromList [Mountain]}

-- | X threat: given by its own modifiers.
theHighPass :: CardDef
theHighPass = unique (location "131069" "The High Pass" OverTheMistyMountainsGrim 1 0 5) {cdCardTraits = setFromList [Mountain]}

-- * The Great Goblin

frontPorch :: CardDef
frontPorch = (location "131066" "Front Porch" TheGreatGoblin 2 4 4) {cdCardTraits = setFromList [Cave]}

greatCavernRoom :: CardDef
greatCavernRoom = (location "131067" "Great Cavern Room" TheGreatGoblin 3 2 3) {cdCardTraits = setFromList [Cave]}

-- * Dungeons Deep and Caverns Dim

theWargsGlade :: CardDef
theWargsGlade =
  (location "131077" "The Wargs' Glade" DungeonsDeepAndCavernsDim 3 2 4)
    { cdCardTraits = setFromList [Forest]
    , cdRiddle = Just $ Riddle [RiddleSphere] 1
    }

-- | Its threat X is twice the number of players; it has no quest points to travel to.
lakeInTheCavern :: CardDef
lakeInTheCavern = unique (location "131078" "Lake in the Cavern" DungeonsDeepAndCavernsDim 1 0 0) {cdCardTraits = setFromList [Cave]}

-- * The Lonely Mountain

theLonelyMountain :: CardDef
theLonelyMountain =
  unique
    (location "132043" "The Lonely Mountain" TheLonelyMountain 1 12 4)
      { cdCardTraits = setFromList [Mountain]
      , cdVictoryPoints = Just 10
      }

greatHall :: CardDef
greatHall = (location "132044" "Great Hall" TheLonelyMountain 3 4 4) {cdCardTraits = setFromList [Underground]}

-- | Of The Lonely Mountain's set, as its icon and the card database have it.
secretEntrance :: CardDef
secretEntrance = (location "132045" "Secret Entrance" TheLonelyMountain 2 1 1) {cdCardTraits = setFromList [Mountain]}

desolationOfSmaug :: CardDef
desolationOfSmaug = (location "132046" "Desolation of Smaug" TheLonelyMountain 3 3 3) {cdCardTraits = setFromList [Wasteland]}

westernSlopes :: CardDef
westernSlopes = (location "132047" "Western Slopes" TheLonelyMountain 3 2 5) {cdCardTraits = setFromList [Mountain]}

-- * A Shadow of the Past

bagEnd :: CardDef
bagEnd = unique (location "141031" "Bag End" AShadowOfThePast 1 0 3) {cdCardTraits = setFromList [Shire], cdVictoryPoints = Just 1}

woodyEnd :: CardDef
woodyEnd = unique (location "141032" "Woody End" AShadowOfThePast 1 1 1) {cdCardTraits = setFromList [Forest], cdVictoryPoints = Just 1}

stockBrook :: CardDef
stockBrook =
  unique (location "141033" "Stock-brook" AShadowOfThePast 1 1 5)
    { cdCardTraits = setFromList [Forest, Stream]
    , cdVictoryPoints = Just 1
    , cdWhenRevealed = True
    }

bamfurlong :: CardDef
bamfurlong = unique (location "141034" "Bamfurlong" AShadowOfThePast 1 2 2) {cdCardTraits = setFromList [Shire], cdVictoryPoints = Just 1}

-- | Set aside by stage 1A, added to the staging area by stage 3A.
buckleburyFerry :: CardDef
buckleburyFerry = unique (location "141035" "Bucklebury Ferry" AShadowOfThePast 1 3 9) {cdCardTraits = setFromList [River]}

greenHillCountry :: CardDef
greenHillCountry = (location "141036" "Green Hill Country" AShadowOfThePast 3 2 3) {cdCardTraits = setFromList [Forest], cdShadowEffect = True}

stockRoad :: CardDef
stockRoad = (location "141037" "Stock Road" AShadowOfThePast 3 3 3) {cdCardTraits = setFromList [Road], cdEasyModeQuantity = Just 1}

theMarish :: CardDef
theMarish =
  (location "141038" "The Marish" AShadowOfThePast 2 3 4)
    { cdCardTraits = setFromList [Marshland]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- * Hunted

pathlessCountryHunted :: CardDef
pathlessCountryHunted = (location "141072" "Pathless Country" Hunted 3 2 3) {cdCardTraits = setFromList [Forest], cdShadowEffect = True}
-- * A Shadow of the Past

-- * Hunted

-- * Flight to the Ford

-- | X threat, the number of players: its card gives it. Immune to player card effects.
theLastBridge :: CardDef
theLastBridge = unique (location "141058" "The Last Bridge" FlightToTheFord 1 0 4) {cdCardTraits = setFromList [Bridge, River], cdVictoryPoints = Just 3}

-- | Set aside by stage 1A, added to the staging area by stage 2B. Immune to player card
-- effects.
fordOfBruinen :: CardDef
fordOfBruinen = unique (location "141059" "Ford of Bruinen" FlightToTheFord 1 2 6) {cdCardTraits = setFromList [River], cdVictoryPoints = Just 5}

theTrollsCamp :: CardDef
theTrollsCamp = unique (location "141060" "The Troll's Camp" FlightToTheFord 1 1 2) {cdCardTraits = setFromList [Forest], cdVictoryPoints = Just 1}

ettenmoors :: CardDef
ettenmoors = (location "141061" "Ettenmoors" FlightToTheFord 3 2 3) {cdCardTraits = setFromList [Forest]}

theOldRoad :: CardDef
theOldRoad = (location "141062" "The Old Road" FlightToTheFord 3 3 4) {cdCardTraits = setFromList [Road]}

-- * A Knife in the Dark

thePrancingPony :: CardDef
thePrancingPony = unique (location "141045" "The Prancing Pony" AKnifeInTheDark 1 2 4) {cdCardTraits = setFromList [Bree], cdVictoryPoints = Just 1}

-- | Set aside by stage 1A, added to the staging area by stage 3A.
weathertop :: CardDef
weathertop = unique (location "141046" "Weathertop" AKnifeInTheDark 1 4 4) {cdCardTraits = setFromList [Hills, Ruins], cdVictoryPoints = Just 4}

chetwood :: CardDef
chetwood = (location "141047" "Chetwood" AKnifeInTheDark 3 2 2) {cdCardTraits = setFromList [Forest], cdShadowEffect = True}

-- | Set aside by stage 1A, added to the staging area by stage 2B.
midgewater :: CardDef
midgewater = unique (location "141048" "Midgewater" AKnifeInTheDark 1 3 6) {cdCardTraits = setFromList [Marshland], cdVictoryPoints = Just 1}

weatherHills :: CardDef
weatherHills = (location "141049" "Weather Hills" AKnifeInTheDark 3 3 2) {cdCardTraits = setFromList [Forest, Hills]}

-- * The Siege of Gondor

-- | X threat, the resources on The Corsair Fleet: given by its own modifiers. Set aside by
-- the scenario, added to the staging area by stage 1A.
greatCorsairShip :: CardDef
greatCorsairShip =
  unique
    (location "145030" "Great Corsair Ship" TheSiegeOfGondor 1 0 5)
      { cdCardTraits = setFromList [Ship]
      , cdVictoryPoints = Just 5
      }

umbarWarGalley :: CardDef
umbarWarGalley = (location "145031" "Umbar War Galley" TheSiegeOfGondor 3 3 3) {cdCardTraits = setFromList [Ship], cdVictoryPoints = Just 3}

corsairDromon :: CardDef
corsairDromon = (location "145032" "Corsair Dromon" TheSiegeOfGondor 3 2 2) {cdCardTraits = setFromList [Ship], cdVictoryPoints = Just 2}

portOfPelargir :: CardDef
portOfPelargir = (location "145033" "Port of Pelargir" TheSiegeOfGondor 3 2 4) {cdCardTraits = setFromList [City], cdShadowEffect = True}

pelargirQuays :: CardDef
pelargirQuays = (location "145034" "Pelargir Quays" TheSiegeOfGondor 2 3 4) {cdCardTraits = setFromList [City]}

-- * The Battle of the Pelennor Fields

-- | X threat, the resources on it: given by its own modifiers. Set aside by stage 1A.
grond :: CardDef
grond =
  unique (location "145044" "Grond" TheBattleOfThePelennorFields 1 0 3)
    { cdCardTraits = setFromList [SiegeTrait]
    , cdVictoryPoints = Just 4
    }

gateOfGondor :: CardDef
gateOfGondor =
  unique (location "145045" "Gate of Gondor" TheBattleOfThePelennorFields 1 5 3)
    { cdCardTraits = setFromList [MinasTirith]
    , cdVictoryPoints = Just 5
    }

cityWall :: CardDef
cityWall = (location "145046" "City Wall" TheBattleOfThePelennorFields 3 3 4) {cdCardTraits = setFromList [MinasTirith]}

pelennorField :: CardDef
pelennorField = (location "145047" "Pelennor Field" TheBattleOfThePelennorFields 4 2 4) {cdCardTraits = setFromList [Pelennor], cdShadowEffect = True}

fireFilledTrench :: CardDef
fireFilledTrench = (location "145048" "Fire-filled Trench" TheBattleOfThePelennorFields 3 2 3) {cdCardTraits = setFromList [Pelennor, SiegeTrait], cdEasyModeQuantity = Just 2}

siegeTower :: CardDef
siegeTower = (location "145049" "Siege Tower" TheBattleOfThePelennorFields 3 3 3) {cdCardTraits = setFromList [SiegeTrait], cdEasyModeQuantity = Just 2}

-- * The Tower of Cirith Ungol

-- | The Ring-bearer is guarded under it from stage 1A.
topmostChamber :: CardDef
topmostChamber =
  (location "146015" "Topmost Chamber" TheTowerOfCirithUngol 1 1 1)
    { cdCardTraits = setFromList [CirithUngol]
    , cdVictoryPoints = Just 2
    }

echoingPassage :: CardDef
echoingPassage =
  (location "146016" "Echoing Passage" TheTowerOfCirithUngol 3 1 5)
    { cdCardTraits = setFromList [CirithUngol]
    , cdEasyModeQuantity = Just 1
    }

bloodiedCourtyard :: CardDef
bloodiedCourtyard = (location "146017" "Bloodied Courtyard" TheTowerOfCirithUngol 3 3 4) {cdCardTraits = setFromList [CirithUngol]}

windingStairs :: CardDef
windingStairs = (location "146018" "Winding Stairs" TheTowerOfCirithUngol 3 2 4) {cdCardTraits = setFromList [CirithUngol]}

torchLitHall :: CardDef
torchLitHall = (location "146019" "Torch-lit Hall" TheTowerOfCirithUngol 3 2 3) {cdCardTraits = setFromList [CirithUngol]}

-- | The location side of The Two Watchers: X threat, given by its own modifiers, and no
-- quest points. Set aside as the encounter deck is gathered, added to the staging area by
-- stage 1A.
theTwoWatchers :: CardDef
theTwoWatchers =
  unique
    (location "146070" "The Two Watchers" TheTowerOfCirithUngol 1 0 0)
      { cdCardTraits = setFromList [Mordor]
      , cdQuestPoints = Nothing
      , cdOtherSide = Just "146071"
      }
