module LordOfTheRings.Quest.CardDefs (module LordOfTheRings.Quest.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Keyword (Keyword (Battle, Capture, Sailing, Siege, Surge, Time))
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere (Sphere (Leadership, Lore, Neutral, Spirit, Tactics))

-- | Side quests from players' decks: player cards, built as quests once played.
allPlayerSideQuestCards :: Map CardCode CardDef
allPlayerSideQuestCards = Map.fromList $ map (\def -> (cdCardCode def, def)) [gatherInformation, doubleBack, delayTheEnemy, sendForAid, scoutAhead]

scoutAhead :: CardDef
scoutAhead =
  (playerSideQuest "10009" "Scout Ahead" Lore (StaticCost 0) 4)
    { cdVictoryPoints = Just 1
    , cdDeckLimit = 1
    }

gatherInformation :: CardDef
gatherInformation =
  (playerSideQuest "09014" "Gather Information" Neutral (StaticCost 0) 4)
    { cdVictoryPoints = Just 1
    , cdDeckLimit = 1
    }

doubleBack :: CardDef
doubleBack =
  (playerSideQuest "10034" "Double Back" Spirit (StaticCost 0) 4)
    { cdVictoryPoints = Just 1
    , cdDeckLimit = 1
    }

delayTheEnemy :: CardDef
delayTheEnemy =
  (playerSideQuest "10058" "Delay the Enemy" Tactics (StaticCost 0) 8)
    { cdVictoryPoints = Just 1
    , cdDeckLimit = 1
    , cdKeywords = setFromList [Battle]
    }

sendForAid :: CardDef
sendForAid =
  (playerSideQuest "10086" "Send for Aid" Leadership (StaticCost 1) 6)
    { cdVictoryPoints = Just 1
    , cdDeckLimit = 1
    }

allQuestCards :: Map CardCode CardDef
allQuestCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ fliesAndSpiders
      , enteringTheMines
      , goblinPatrol
      , aWayUp
      , aForkInTheRoad
      , aChosenPathDontLeaveThePath
      , aChosenPathBeornsPath
      , theNecromancersTower
      , throughTheCaverns
      , outOfTheDungeons
      , toTheRiver
      , anduinPassage
      , ambushOnTheShore
      , theHuntBegins
      , aNewTerrorAbroad
      , onTheTrail
      , grimbeornsQuest
      , againstTheTrolls
      , theWoundedEagle
      , radagastsRequest
      , returnToRhosgobel
      , theHillsOfEmynMuil
      , intoTheMarshes
      , theCapture
      , throughTheForest
      , escapeAttempt
      , toTheElvenKingsHalls
      , ambush
      , aPresenceInTheDark
      , headingDown
      , headingUp
      , aWrongTurn
      , narrowPaths
      , hastyCouncil
      , blockedByShadow
      , escapeFromDarkness
      , searchForTheChamber
      , theFateOfBalin
      , journeyInTheBlackPit
      , continuingEastward
      , alongTheMistyMountains
      , orcOutpost
      , approachingRivendell
      , upThePass
      , snowdrifts
      , theMountainsPeaks
      , theDrippingWalls
      , theWatersEdge
      , washedAway
      , belowTheMines
      , theEndlessCaves
      , theShiveringBank
      , oldOneLair
      , outOfTheDepths
      , nearingTheGate
      , theRearGuard
      , lastLordOfMoria
      , toTheWestDoor
      , theSeethingLake
      , theDefense
      , reinforcingTheBanks
      , breakthroughAtTheApproach
      , breakthroughAtTheCitadel
      , theLastBattle
      , ambushInIthilien
      , southronCounterAttack
      , theHiddenWay
      , approachingCairAndros
      , theLeapingFishQuest
      , fightingInTheStreets
      , escapeToTheQuays
      , savageryOfTheOrcs
      , protectTheVillagers
      , thePursuit
      , anUntimelyEnd
      , thePassageOut
      , theRearguardTheMorgulVale
      , theBetrayal
      , theMorgulRider
      , conspiracy
      , theGrandDesign
      , theConfrontation
      , retakeTheCity
      , theAmbush
      , captured
      , ordersFromOrthanc
      , searchingForMugash
      , theWizardsPrize
      , intoTheWoods
      , escapeFromFangorn
      , theAngryForest
      , fightAtTheFords
      , dunlendingAttack
      , holdTheFords
      , battleForDunland
      , theRavenClan
      , theLastStage
      , theCatacombsOfCarnDum
      , angmarAwakened
      , daechanarsFall
      , theRoadToTharbad
      , aWellLaidTrap
      , noWayOut
      , doubleDealings
      , escapeFromTharbad
      , theRuinsOfOstInEdhil
      , theEnemysServant
      , fleeingFromTharbad
      , throughTheMarshNoEndInSight
      , throughTheMarshAWearyPassage
      , throughTheMarshAForgottenLand
      , lostInTheSwanfleetImpassableMarshland
      , throughTheMarshAnArduousJourney
      , lostInTheSwanfleetDeadlyWaters
      , lostInTheSwanfleetATreacherousSwamp
      , lostInTheSwanfleetCreaturesOfAForgottenAge
      , outOfTheSwamp
      , theTrialsBegin
      , theTrialOfStrength
      , theTrialOfPerseverance
      , theTrialOfIntuition
      , theAntleredCrownTheThreeTrials
      , searchTheRuins
      , theClutchesOfCarnDum
      , midwintersCrux
      , furiousCharge
      , acrossTheWastes
      , howlingAtNight
      , battleWithThePack
      , unexpectedRescue
      , gornakhsDungeons
      , flightFromMountGram
      , stopTheExecutioners
      , orcRearguard
      , rescueIarion
      , orcAmbush
      , lostInTheWilderness
      , stopTheWarParty
      , findShelter
      , makeCamp
      , scatteredAmongTheHills
      , corneredAnimals
      , intoTheEttenmoors
      , aMiserableJourney
      , journeysEnd
      , lieLow
      , forageForFood
      , scavengeForSupplies
      , theShadesOfAngmar
      , aFellWraith
      , theShadowWorld
      , sealTheTomb
      , thePowerOfAngmar
      , shadowTheWarParty
      , protectTheBreeLanders
      , trackTheOrcs
      , gatherYourCourage
      , secretsOfRhudaur
      , thaurdirsPursuit
      , quietTheSpirits
      , siftThroughTheDebris
      , decipherAncientTexts
      , departureFromTheHavens
      , intoTheStorm
      , cursedMists
      , calphonsDivination
      , theStarsGuidance
      , corsairPursuit
      , exploreTheIsland
      , aFatefulDiscovery
      , theCorsairsAssault
      , sahirsAdvance
      , swiftDeparture
      , fullSailAhead
      , theWeatherTurnsFoul
      , theCapeOfAndrast
      , fullSailAheadStormcaller
      , theWeatherTurnsFoulStormcaller
      , theCapeOfAndrastStormcaller
      , takeTheShip
      , changeOfPlans
      , defendTheShip
      , theGrotto
      , sahirsBetrayal
      , searchingTheIsland
      , outmaneuverTheEnemy
      , battleInTheBay
      , breakThroughTheFleet
      , theCoastOfUmbar
      , fateOfTheDreamChaser
      , theHavenOfUmbar
      , finalConfrontation
      , corsairConfrontation
      , lostAtSea
      , sahirsAdvanceNightmare
      , goblinAmbush
      , pursuedByShadowStage
      , blockedByFlame
      , ghash
      , grimbeornsFolly
      , ambushInTheNight
      , makeAStand
      , forcedOffTrack
      , raidGolfimbulsVault
      , intoTheEttenmoorsNightmare
      , isolated
      , rescueFromTheTrolls
      , protectTheFlanks
      , rallyTheFighters
      , severTheHead
      , theWitchKingsGift
      ]

theDrippingWalls :: CardDef
theDrippingWalls = quest "04111" "The Dripping Walls" FoundationsOfStone 1 9

theWatersEdge :: CardDef
theWatersEdge = quest "04112" "The Water's Edge" FoundationsOfStone 2 12

washedAway :: CardDef
washedAway = quest "04113" "Washed Away!" FoundationsOfStone 3 0

-- | The four stage 4s share a side A, Below the Mines: 04114's B side is Sheltered Rocks.
belowTheMines :: CardDef
belowTheMines = quest "04114" "Sheltered Rocks" FoundationsOfStone 4 11

theEndlessCaves :: CardDef
theEndlessCaves = quest "04115" "The Endless Caves" FoundationsOfStone 4 17

theShiveringBank :: CardDef
theShiveringBank = quest "04116" "The Shivering Bank" FoundationsOfStone 4 7

oldOneLair :: CardDef
oldOneLair = quest "04117" "Old One Lair" FoundationsOfStone 4 5

outOfTheDepths :: CardDef
outOfTheDepths = quest "04118" "Out of the Depths" FoundationsOfStone 5 11

enteringTheMines :: CardDef
enteringTheMines = quest "03064" "Entering the Mines" IntoThePit 1 7

goblinPatrol :: CardDef
goblinPatrol = quest "03065" "Goblin Patrol" IntoThePit 2 11

aWayUp :: CardDef
aWayUp = quest "03066" "A Way Up" IntoThePit 3 12

fliesAndSpiders :: CardDef
fliesAndSpiders = quest "01119" "Flies and Spiders" PassageThroughMirkwood 1 8

aForkInTheRoad :: CardDef
aForkInTheRoad = quest "01120" "A Fork in the Road" PassageThroughMirkwood 2 2

aChosenPathDontLeaveThePath :: CardDef
aChosenPathDontLeaveThePath = quest "01121" "A Chosen Path" PassageThroughMirkwood 3 0

aChosenPathBeornsPath :: CardDef
aChosenPathBeornsPath = quest "01122" "A Chosen Path" PassageThroughMirkwood 3 10

theNecromancersTower :: CardDef
theNecromancersTower = quest "01123" "The Necromancer's Tower" EscapeFromDolGuldur 1 9

throughTheCaverns :: CardDef
throughTheCaverns = quest "01124" "Through the Caverns" EscapeFromDolGuldur 2 15

outOfTheDungeons :: CardDef
outOfTheDungeons = quest "01125" "Out of the Dungeons" EscapeFromDolGuldur 3 7

toTheRiver :: CardDef
toTheRiver = quest "01126" "To the River..." JourneyAlongTheAnduin 1 8

anduinPassage :: CardDef
anduinPassage = quest "01127" "Anduin Passage" JourneyAlongTheAnduin 2 16

ambushOnTheShore :: CardDef
ambushOnTheShore = quest "01128" "Ambush on the Shore" JourneyAlongTheAnduin 3 0

theHuntBegins :: CardDef
theHuntBegins = quest "02011" "The Hunt Begins" TheHuntForGollum 1 8

aNewTerrorAbroad :: CardDef
aNewTerrorAbroad = quest "02012" "A New Terror Abroad" TheHuntForGollum 2 10

onTheTrail :: CardDef
onTheTrail = quest "02013" "On the Trail" TheHuntForGollum 3 8

grimbeornsQuest :: CardDef
grimbeornsQuest = quest "02035" "Grimbeorn's Quest" ConflictAtTheCarrock 1 7

againstTheTrolls :: CardDef
againstTheTrolls = quest "02036" "Against the Trolls" ConflictAtTheCarrock 2 1

theWoundedEagle :: CardDef
theWoundedEagle = quest "02060" "The Wounded Eagle" AJourneyToRhosgobel 1 8

radagastsRequest :: CardDef
radagastsRequest = quest "02061" "Radagast's Request" AJourneyToRhosgobel 2 12

returnToRhosgobel :: CardDef
returnToRhosgobel = quest "02062" "Return to Rhosgobel" AJourneyToRhosgobel 3 0

theHillsOfEmynMuil :: CardDef
theHillsOfEmynMuil = quest "02082" "The Hills of Emyn Muil" TheHillsOfEmynMuil 1 1

intoTheMarshes :: CardDef
intoTheMarshes = quest "02105" "Into the Marshes" TheDeadMarshes 1 12

theCapture :: CardDef
theCapture = quest "02106" "The Capture" TheDeadMarshes 2 3

throughTheForest :: CardDef
throughTheForest = quest "02126" "Through the Forest" ReturnToMirkwood 1 12

escapeAttempt :: CardDef
escapeAttempt = quest "02127" "Escape Attempt" ReturnToMirkwood 2 3

toTheElvenKingsHalls :: CardDef
toTheElvenKingsHalls = quest "02128" "To the Elven King's Halls" ReturnToMirkwood 3 7

ambush :: CardDef
ambush = quest "02129" "Ambush" ReturnToMirkwood 4 2

searchForTheChamber :: CardDef
searchForTheChamber = quest "03067" "Search for the Chamber" TheSeventhLevel 1 15

theFateOfBalin :: CardDef
theFateOfBalin = quest "03068" "The Fate of Balin" TheSeventhLevel 2 17

aPresenceInTheDark :: CardDef
aPresenceInTheDark = (quest "03069" "A Presence in the Dark" FlightFromMoria 1 0) {cdVictoryPoints = Just 2}

-- | A Flight from Moria stage 2: its side A, shared by all, reads "Search for an Exit".
searchForAnExit :: CardCode -> Text -> Int -> Maybe Int -> CardDef
searchForAnExit code subtitle questPoints victory =
  (quest code (Name "Search for an Exit" (Just subtitle)) FlightFromMoria 2 questPoints) {cdVictoryPoints = victory}

headingDown :: CardDef
headingDown = searchForAnExit "03070" "Heading Down" 5 (Just 1)

headingUp :: CardDef
headingUp = searchForAnExit "03071" "Heading Up" 7 (Just 1)

aWrongTurn :: CardDef
aWrongTurn = searchForAnExit "03072" "A Wrong Turn" 1 (Just 2)

narrowPaths :: CardDef
narrowPaths = searchForAnExit "03073" "Narrow Paths" 3 (Just 1)

hastyCouncil :: CardDef
hastyCouncil = searchForAnExit "03074" "Hasty Council" 0 (Just 2)

blockedByShadow :: CardDef
blockedByShadow = searchForAnExit "03075" "Blocked by Shadow" 9 Nothing

escapeFromDarkness :: CardDef
escapeFromDarkness = searchForAnExit "03076" "Escape from Darkness" 4 Nothing

journeyInTheBlackPit :: CardDef
journeyInTheBlackPit = quest "04086" "Journey in the Black Pit" TheLongDark 1 13

continuingEastward :: CardDef
continuingEastward = quest "04087" "Continuing Eastward" TheLongDark 2 17

alongTheMistyMountains :: CardDef
alongTheMistyMountains = quest "04038" "Along the Misty Mountains" RoadToRivendell 1 20

orcOutpost :: CardDef
orcOutpost = quest "04039" "Orc Outpost" RoadToRivendell 2 7

approachingRivendell :: CardDef
approachingRivendell = quest "04040" "Approaching Rivendell" RoadToRivendell 3 13

upThePass :: CardDef
upThePass = quest "04011" "Up the Pass" TheRedhornGate 1 9

snowdrifts :: CardDef
snowdrifts = quest "04012" "Snowdrifts" TheRedhornGate 2 11

theMountainsPeaks :: CardDef
theMountainsPeaks = quest "04013" "The Mountains' Peaks" TheRedhornGate 3 13

nearingTheGate :: CardDef
nearingTheGate = quest "04138" "Nearing the Gate" ShadowAndFlame 1 9

theRearGuard :: CardDef
theRearGuard = quest "04139" "The Rear Guard" ShadowAndFlame 2 16

lastLordOfMoria :: CardDef
lastLordOfMoria = quest "04140" "Last Lord of Moria" ShadowAndFlame 3 1

toTheWestDoor :: CardDef
toTheWestDoor = quest "04063" "To the West-door" TheWatcherInTheWater 1 13

theSeethingLake :: CardDef
theSeethingLake = quest "04064" "The Seething Lake" TheWatcherInTheWater 2 5

theDefense :: CardDef
theDefense = (quest "05072" "The Defense" TheSiegeOfCairAndros 1 9) {cdKeywords = setFromList [Siege]}

reinforcingTheBanks :: CardDef
reinforcingTheBanks = quest "05073" "Reinforcing the Banks" TheSiegeOfCairAndros 2 9

breakthroughAtTheApproach :: CardDef
breakthroughAtTheApproach =
  (quest "05074" "Breakthrough at the Approach" TheSiegeOfCairAndros 3 7) {cdKeywords = setFromList [Battle]}

breakthroughAtTheCitadel :: CardDef
breakthroughAtTheCitadel =
  (quest "05075" "Breakthrough at the Citadel" TheSiegeOfCairAndros 4 5) {cdKeywords = setFromList [Siege]}

theLastBattle :: CardDef
theLastBattle = (quest "05076" "The Last Battle" TheSiegeOfCairAndros 5 15) {cdKeywords = setFromList [Siege]}

ambushInIthilien :: CardDef
ambushInIthilien = (quest "05068" "Ambush in Ithilien" IntoIthilien 1 15) {cdKeywords = setFromList [Battle]}

-- | Archery X, X the number of players: gained from its own modifiers.
southronCounterAttack :: CardDef
southronCounterAttack = (quest "05069" "Southron Counter-attack" IntoIthilien 2 9) {cdKeywords = setFromList [Siege]}

theHiddenWay :: CardDef
theHiddenWay = quest "05070" "The Hidden Way" IntoIthilien 3 12

approachingCairAndros :: CardDef
approachingCairAndros = quest "05071" "Approaching Cair Andros" IntoIthilien 4 15

theLeapingFishQuest :: CardDef
theLeapingFishQuest = quest "05065" "The Leaping Fish" PerilInPelargir 1 6

fightingInTheStreets :: CardDef
fightingInTheStreets =
  (quest "05066" "Fighting in the Streets" PerilInPelargir 2 13)
    { cdKeywords = setFromList [Battle]
    }

escapeToTheQuays :: CardDef
escapeToTheQuays = quest "05067" "Escape to the Quays" PerilInPelargir 3 15

-- | No quest points: progress placed on it moves its villagers to Rescued Villagers.
savageryOfTheOrcs :: CardDef
savageryOfTheOrcs = (quest "06066" "Savagery of the Orcs" EncounterAtAmonDin 1 0) {cdQuestPoints = Nothing}

protectTheVillagers :: CardDef
protectTheVillagers = quest "06067" "Protect the Villagers" EncounterAtAmonDin 2 15

thePursuit :: CardDef
thePursuit = quest "06042" "The Pursuit" TheDruadanForest 1 11

anUntimelyEnd :: CardDef
anUntimelyEnd = quest "06043" "An Untimely End" TheDruadanForest 2 17

thePassageOut :: CardDef
thePassageOut = (quest "06044" "The Passage Out" TheDruadanForest 3 14) {cdKeywords = setFromList [Siege]}

-- | Its quest points are "-": the stage advances only as Murzag is defeated.
theRearguardTheMorgulVale :: CardDef
theRearguardTheMorgulVale = quest "06144" "The Rearguard" TheMorgulVale 1 0

theBetrayal :: CardDef
theBetrayal = quest "06145" "The Betrayal" TheMorgulVale 2 0

theMorgulRider :: CardDef
theMorgulRider = quest "06146" "The Morgul Rider" TheMorgulVale 3 0

conspiracy :: CardDef
conspiracy = quest "06011" "Conspiracy" TheStewardsFear 1 0

theGrandDesign :: CardDef
theGrandDesign = quest "06012" "The Grand Design" TheStewardsFear 2 0

theConfrontation :: CardDef
theConfrontation = quest "06013" "The Confrontation" TheStewardsFear 3 15

retakeTheCity :: CardDef
retakeTheCity = quest "06091" "Retake the City" AssaultOnOsgiliath 1 0

theAmbush :: CardDef
theAmbush = quest "06117" "The Ambush" TheBloodOfGondor 1 11

captured :: CardDef
captured = (quest "06118" "Captured!" TheBloodOfGondor 2 15) {cdKeywords = setFromList [Battle]}

ordersFromOrthanc :: CardDef
ordersFromOrthanc = quest "07062" "Orders from Orthanc" ToCatchAnOrc 1 0

searchingForMugash :: CardDef
searchingForMugash = (quest "07063" "Searching for Mugash" ToCatchAnOrc 2 0) {cdKeywords = setFromList [Time 2]}

theWizardsPrize :: CardDef
theWizardsPrize = (quest "07064" "The Wizard's Prize" ToCatchAnOrc 3 15) {cdKeywords = setFromList [Time 3]}

intoTheWoods :: CardDef
intoTheWoods = (quest "07065" "Into the Woods" IntoFangorn 1 9) {cdKeywords = setFromList [Time 4]}

escapeFromFangorn :: CardDef
escapeFromFangorn = (quest "07066" "Escape from Fangorn" IntoFangorn 2 12) {cdKeywords = setFromList [Time 4]}

-- | Its quest points are "-": it advances back to stage 2 on 6 progress with Mugash on a hero.
theAngryForest :: CardDef
theAngryForest = (quest "07067" "The Angry Forest" IntoFangorn 3 0) {cdKeywords = setFromList [Time 3]}

fightAtTheFords :: CardDef
fightAtTheFords = (quest "07059" "Fight at the Fords" TheFordsOfIsen 1 6) {cdKeywords = setFromList [Time 5]}

dunlendingAttack :: CardDef
dunlendingAttack = (quest "07060" "Dunlending Attack" TheFordsOfIsen 2 14) {cdKeywords = setFromList [Time 2]}

holdTheFords :: CardDef
holdTheFords = (quest "07061" "Hold the Fords" TheFordsOfIsen 3 16) {cdKeywords = setFromList [Time 3]}

battleForDunland :: CardDef
battleForDunland = (quest "08147" "Battle for Dunland" TheAntleredCrown 1 10) {cdKeywords = setFromList [Time 3]}

theRavenClan :: CardDef
theRavenClan = (quest "08148" "The Raven Clan" TheAntleredCrown 2 15) {cdKeywords = setFromList [Time 3]}

-- | Its quest points are "-": the players win once Raven Chief is in the victory display.
theLastStage :: CardDef
theLastStage = (quest "08149" "The Last Stage" TheAntleredCrown 3 0) {cdKeywords = setFromList [Time 2]}

theRoadToTharbad :: CardDef
theRoadToTharbad = (quest "08011" "The Road to Tharbad" TheDunlandTrap 1 18) {cdKeywords = setFromList [Time 2]}

-- | Its quest points are "-": it advances at the end of the combat phase.
aWellLaidTrap :: CardDef
aWellLaidTrap = quest "08012" "A Well Laid Trap" TheDunlandTrap 2 0

-- | Its quest points are "-". Time X, X 5 times the number of players: gained from its
-- own modifiers.
noWayOut :: CardDef
noWayOut = quest "08013" "No Way Out" TheDunlandTrap 3 0

doubleDealings :: CardDef
doubleDealings = (quest "08066" "Double Dealings" TroubleInTharbad 1 0) {cdKeywords = setFromList [Time 4]}

escapeFromTharbad :: CardDef
escapeFromTharbad = (quest "08067" "Escape from Tharbad" TroubleInTharbad 2 0) {cdKeywords = setFromList [Time 3]}

theRuinsOfOstInEdhil :: CardDef
theRuinsOfOstInEdhil = (quest "08122" "The Ruins of Ost-in-Edhil" CelebrimborsSecret 1 14) {cdKeywords = setFromList [Time 3]}

theEnemysServant :: CardDef
theEnemysServant = (quest "08123" "The Enemy's Servant" CelebrimborsSecret 2 12) {cdKeywords = setFromList [Time 3]}

fleeingFromTharbad :: CardDef
fleeingFromTharbad = quest "08094" "Fleeing from Tharbad" TheNinInEilph 1 0

-- | The stage 2s of The Nîn-in-Eilph: Time 3, and the players wander among them at random.
throughTheMarsh :: CardCode -> Text -> CardDef
throughTheMarsh code subtitle =
  (quest code (Name "Through the Marsh" (Just subtitle)) TheNinInEilph 2 13) {cdKeywords = setFromList [Time 3]}

throughTheMarshNoEndInSight :: CardDef
throughTheMarshNoEndInSight = throughTheMarsh "08095" "No End in Sight"

throughTheMarshAWearyPassage :: CardDef
throughTheMarshAWearyPassage = throughTheMarsh "08096" "A Weary Passage"

throughTheMarshAForgottenLand :: CardDef
throughTheMarshAForgottenLand = throughTheMarsh "08097" "A Forgotten Land"

-- | The stage 3s of The Nîn-in-Eilph, as the stage 2s.
lostInTheSwanfleet :: CardCode -> Text -> CardDef
lostInTheSwanfleet code subtitle =
  (quest code (Name "Lost in the Swanfleet" (Just subtitle)) TheNinInEilph 3 16) {cdKeywords = setFromList [Time 3]}

lostInTheSwanfleetImpassableMarshland :: CardDef
lostInTheSwanfleetImpassableMarshland = lostInTheSwanfleet "08098" "Impassable Marshland"

lostInTheSwanfleetATreacherousSwamp :: CardDef
lostInTheSwanfleetATreacherousSwamp = lostInTheSwanfleet "08099" "A Treacherous Swamp"

lostInTheSwanfleetCreaturesOfAForgottenAge :: CardDef
lostInTheSwanfleetCreaturesOfAForgottenAge = lostInTheSwanfleet "08100" "Creatures of a Forgotten Age"

-- | No quest points: the players win by defeating Ancient Marsh-dweller.
outOfTheSwamp :: CardDef
outOfTheSwamp =
  (quest "08101" "Out of the Swamp" TheNinInEilph 4 0) {cdQuestPoints = Nothing, cdKeywords = setFromList [Time 2]}

theTrialsBegin :: CardDef
theTrialsBegin = quest "08035" "The Trials Begin" TheThreeTrials 1 0

theTrialOfStrength :: CardDef
theTrialOfStrength = quest "08036" "The Trial of Strength" TheThreeTrials 2 0

theTrialOfPerseverance :: CardDef
theTrialOfPerseverance = quest "08037" "The Trial of Perseverance" TheThreeTrials 2 0

theTrialOfIntuition :: CardDef
theTrialOfIntuition = quest "08038" "The Trial of Intuition" TheThreeTrials 2 0

theAntleredCrownTheThreeTrials :: CardDef
theAntleredCrownTheThreeTrials = quest "08039" "The Antlered Crown" TheThreeTrials 3 1

searchTheRuins :: CardDef
searchTheRuins =
  (encounterSideQuest "09059" "Search the Ruins" RuinsOfArnor 8)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    }

theClutchesOfCarnDum :: CardDef
theClutchesOfCarnDum = quest "10125" "The Clutches of Carn Dûm" TheBattleOfCarnDum 1 15

midwintersCrux :: CardDef
midwintersCrux = quest "10126" "Midwinter's Crux" TheBattleOfCarnDum 2 15

furiousCharge :: CardDef
furiousCharge = (encounterSideQuest "10139" "Furious Charge" TheBattleOfCarnDum 5) {cdVictoryPoints = Just 10}

acrossTheWastes :: CardDef
acrossTheWastes = quest "10013" "Across the Wastes" TheWastesOfEriador 1 20

howlingAtNight :: CardDef
howlingAtNight = quest "10014" "Howling at Night" TheWastesOfEriador 2 15

battleWithThePack :: CardDef
battleWithThePack = quest "10015" "Battle with the Pack" TheWastesOfEriador 3 5

unexpectedRescue :: CardDef
unexpectedRescue = quest "10038" "Unexpected Rescue" EscapeFromMountGram 1 0

-- | Four copies, one for each player's own stage 2.
gornakhsDungeons :: CardDef
gornakhsDungeons = (quest "10039" "Gornákh's Dungeons" EscapeFromMountGram 2 0) {cdKeywords = setFromList [Capture 7]}

flightFromMountGram :: CardDef
flightFromMountGram = quest "10040" "Flight from Mount Gram" EscapeFromMountGram 3 14

stopTheExecutioners :: CardDef
stopTheExecutioners =
  (encounterSideQuest "10053" "Stop the Executioners!" EscapeFromMountGram 6)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Capture 5, Time 4]
    }

orcRearguard :: CardDef
orcRearguard = (encounterSideQuest "09023" "Orc Rearguard" IntrudersInChetwood 3) {cdVictoryPoints = Just 10}

rescueIarion :: CardDef
rescueIarion =
  (encounterSideQuest "09024" "Rescue Iârion" IntrudersInChetwood 6)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Time 4]
    , cdWhenRevealed = True
    }

orcAmbush :: CardDef
orcAmbush =
  (encounterSideQuest "09046" "Orc Ambush" AngmarOrcs 4)
    { cdVictoryPoints = Just 10
    , cdWhenRevealed = True
    }

lostInTheWilderness :: CardDef
lostInTheWilderness =
  (encounterSideQuest "09051" "Lost in the Wilderness" EriadorWilds 4)
    { cdVictoryPoints = Just 10
    , cdWhenRevealed = True
    }

stopTheWarParty :: CardDef
stopTheWarParty = quest "09068" "Stop the War Party" IntrudersInChetwood 1 30

findShelter :: CardDef
findShelter =
  (encounterSideQuest "09032" "Find Shelter" TheWeatherHills 6)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge, Time 4]
    }

makeCamp :: CardDef
makeCamp =
  (encounterSideQuest "09055" "Make Camp" FoulWeather 6)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    }

-- | No quest points: the players leave it as Hunting the Orcs flips.
scatteredAmongTheHills :: CardDef
scatteredAmongTheHills = (quest "09069" "Scattered Among the Hills" TheWeatherHills 1 0) {cdQuestPoints = Nothing}

corneredAnimals :: CardDef
corneredAnimals = quest "09070" "Cornered Animals" TheWeatherHills 2 20

intoTheEttenmoors :: CardDef
intoTheEttenmoors = quest "10064" "Into the Ettenmoors" AcrossTheEttenmoors 1 10

aMiserableJourney :: CardDef
aMiserableJourney = quest "10065" "A Miserable Journey" AcrossTheEttenmoors 2 20

journeysEnd :: CardDef
journeysEnd = quest "10066" "Journey's End" AcrossTheEttenmoors 3 17

lieLow :: CardDef
lieLow = (encounterSideQuest "10081" "Lie Low" AcrossTheEttenmoors 8) {cdVictoryPoints = Just 10}

forageForFood :: CardDef
forageForFood = (encounterSideQuest "10082" "Forage for Food" AcrossTheEttenmoors 6) {cdVictoryPoints = Just 10}

scavengeForSupplies :: CardDef
scavengeForSupplies = (encounterSideQuest "10083" "Scavenge for Supplies" AcrossTheEttenmoors 7) {cdVictoryPoints = Just 10}

theShadesOfAngmar :: CardDef
theShadesOfAngmar = quest "09073" "The Shades of Angmar" DeadmensDike 1 11

aFellWraith :: CardDef
aFellWraith = quest "09074" "A Fell Wraith" DeadmensDike 2 13

theShadowWorld :: CardDef
theShadowWorld = (encounterSideQuest "09042" "The Shadow World" DeadmensDike 5) {cdKeywords = setFromList [Surge]}

sealTheTomb :: CardDef
sealTheTomb =
  (encounterSideQuest "09063" "Seal the Tomb" CursedDead 6)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    }

thePowerOfAngmar :: CardDef
thePowerOfAngmar =
  (encounterSideQuest "09067" "The Power of Angmar" DarkSorcery 8)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    }

theCatacombsOfCarnDum :: CardDef
theCatacombsOfCarnDum = quest "10150" "The Catacombs of Carn Dûm" TheDreadRealm 1 18

-- | No quest points: the players advance once Daechanar is defeated.
angmarAwakened :: CardDef
angmarAwakened = (quest "10151" "Angmar Awakened" TheDreadRealm 2 0) {cdQuestPoints = Nothing}

-- | No quest points: the players win once no location is left in play.
daechanarsFall :: CardDef
daechanarsFall = (quest "10152" "Daechanar's Fall" TheDreadRealm 3 0) {cdQuestPoints = Nothing}

-- | No quest points: it advances as its side quests are gone, or its time runs out.
secretsOfRhudaur :: CardDef
secretsOfRhudaur = (quest "10094" "Secrets of Rhudaur" TheTreacheryOfRhudaur 1 0) {cdKeywords = setFromList [Time 5]}

thaurdirsPursuit :: CardDef
thaurdirsPursuit = quest "10095" "Thaurdir's Pursuit" TheTreacheryOfRhudaur 2 30

-- | Flipped to Daechanar's Brand once defeated.
quietTheSpirits :: CardDef
quietTheSpirits = encounterSideQuest "10109" "Quiet the Spirits" TheTreacheryOfRhudaur 16

-- | Flipped to Heirloom of Iârchon once defeated.
siftThroughTheDebris :: CardDef
siftThroughTheDebris = encounterSideQuest "10111" "Sift through the Debris" TheTreacheryOfRhudaur 18

-- | Flipped to Orders from Angmar once defeated.
decipherAncientTexts :: CardDef
decipherAncientTexts = encounterSideQuest "10113" "Decipher Ancient Texts" TheTreacheryOfRhudaur 14

-- * Voyage Across Belegaer: each stage has the Sailing keyword

departureFromTheHavens :: CardDef
departureFromTheHavens = (quest "11016" "Departure from the Havens" VoyageAcrossBelegaer 1 8) {cdKeywords = setFromList [Sailing]}

intoTheStorm :: CardDef
intoTheStorm = (quest "11017" "Into the Storm" VoyageAcrossBelegaer 2 2) {cdKeywords = setFromList [Sailing]}

cursedMists :: CardDef
cursedMists = (quest "11018" "Cursed Mists" VoyageAcrossBelegaer 2 6) {cdKeywords = setFromList [Sailing]}

calphonsDivination :: CardDef
calphonsDivination = (quest "11019" "Calphon's Divination" VoyageAcrossBelegaer 2 4) {cdKeywords = setFromList [Sailing]}

theStarsGuidance :: CardDef
theStarsGuidance = (quest "11020" "The Star's Guidance" VoyageAcrossBelegaer 2 4) {cdKeywords = setFromList [Sailing]}

corsairPursuit :: CardDef
corsairPursuit = (quest "11021" "Corsair Pursuit" VoyageAcrossBelegaer 3 10) {cdKeywords = setFromList [Sailing]}

exploreTheIsland :: CardDef
exploreTheIsland = quest "11025" "Explore the Island" TheFateOfNumenor 1 20

-- | No quest points: the players win as Shrine to Morgoth goes to the victory display.
aFatefulDiscovery :: CardDef
aFatefulDiscovery = (quest "11026" "A Fateful Discovery" TheFateOfNumenor 2 0) {cdQuestPoints = Nothing}

theCorsairsAssault :: CardDef
theCorsairsAssault = quest "11036" "The Corsair's Assault" RaidOnTheGreyHavens 1 18

-- | No quest points: the players win once Captain Sahír is in the victory display and no
-- location in play has damage.
sahirsAdvance :: CardDef
sahirsAdvance = (quest "11037" "Sahír's Advance" RaidOnTheGreyHavens 2 0) {cdQuestPoints = Nothing}

-- * Flight of the Stormcaller: the players' stages have the Sailing keyword; the
-- Stormcaller's own stages, 2C to 4C, make up the Stormcaller's quest deck (sides C and D)

swiftDeparture :: CardDef
swiftDeparture = (quest "12012" "Swift Departure" FlightOfTheStormcaller 1 8) {cdKeywords = setFromList [Sailing]}

fullSailAhead :: CardDef
fullSailAhead = (quest "12013" "Full Sail Ahead!" FlightOfTheStormcaller 2 12) {cdKeywords = setFromList [Sailing]}

theWeatherTurnsFoul :: CardDef
theWeatherTurnsFoul = (quest "12014" "The Weather Turns Foul" FlightOfTheStormcaller 3 18) {cdKeywords = setFromList [Sailing]}

theCapeOfAndrast :: CardDef
theCapeOfAndrast = (quest "12015" "The Cape of Andrast" FlightOfTheStormcaller 4 24) {cdKeywords = setFromList [Sailing]}

fullSailAheadStormcaller :: CardDef
fullSailAheadStormcaller = quest "12016" "Full Sail Ahead!" FlightOfTheStormcaller 2 12

theWeatherTurnsFoulStormcaller :: CardDef
theWeatherTurnsFoulStormcaller = quest "12017" "The Weather Turns Foul" FlightOfTheStormcaller 3 18

theCapeOfAndrastStormcaller :: CardDef
theCapeOfAndrastStormcaller = quest "12018" "The Cape of Andrast" FlightOfTheStormcaller 4 24

-- * The Thing in the Depths

takeTheShip :: CardDef
takeTheShip = quest "12039" "Take the Ship!" TheThingInTheDepths 1 4

-- | No quest points: its When Revealed effect advances to stage 3.
changeOfPlans :: CardDef
changeOfPlans = (quest "12040" "Change of Plans" TheThingInTheDepths 2 0) {cdQuestPoints = Nothing}

-- | No quest points: the players win as The Thing in the Depths is destroyed.
defendTheShip :: CardDef
defendTheShip = (quest "12041" "Defend the Ship!" TheThingInTheDepths 3 0) {cdQuestPoints = Nothing}

theGrotto :: CardDef
theGrotto = quest "12092" "The Grotto" TheDrownedRuins 1 15

-- | No quest points: the players win as Shrine to Morgoth goes to the victory display.
sahirsBetrayal :: CardDef
sahirsBetrayal = (quest "12093" "Sahír's Betrayal" TheDrownedRuins 2 0) {cdQuestPoints = Nothing}

-- | No quest points: the players win at The Grotto's Entrance.
searchingTheIsland :: CardDef
searchingTheIsland = (quest "12067" "Searching the Island" TempleOfTheDeceived 1 0) {cdQuestPoints = Nothing}

-- * A Storm on Cobas Haven: each stage has the Sailing keyword

outmaneuverTheEnemy :: CardDef
outmaneuverTheEnemy = (quest "12120" "Outmaneuver the Enemy" AStormOnCobasHaven 1 10) {cdKeywords = setFromList [Sailing]}

battleInTheBay :: CardDef
battleInTheBay = (quest "12121" "Battle in the Bay" AStormOnCobasHaven 2 16) {cdKeywords = setFromList [Sailing]}

breakThroughTheFleet :: CardDef
breakThroughTheFleet = (quest "12122" "Break Through the Fleet!" AStormOnCobasHaven 3 5) {cdKeywords = setFromList [Sailing]}

-- * The City of Corsairs: the first two stages are Coast of Umbar's, the last two The City
-- of Corsairs'

theCoastOfUmbar :: CardDef
theCoastOfUmbar = (quest "12147" "The Coast of Umbar" CoastOfUmbar 1 15) {cdKeywords = setFromList [Sailing]}

fateOfTheDreamChaser :: CardDef
fateOfTheDreamChaser = quest "12148" "Fate of the Dream-chaser" CoastOfUmbar 2 0

theHavenOfUmbar :: CardDef
theHavenOfUmbar = quest "12149" "The Haven of Umbar" TheCityOfCorsairs 3 18

finalConfrontation :: CardDef
finalConfrontation = quest "12150" "Final Confrontation" TheCityOfCorsairs 4 0

-- The Grey Havens Nightmare Decks

corsairConfrontation :: CardDef
corsairConfrontation =
  (quest "the-grey-havens-nightmare-voyage-across-belegaer-002" "Corsair Confrontation" VoyageAcrossBelegaerNightmare 2 0)
    { cdQuestPoints = Nothing
    , cdKeywords = setFromList [Sailing]
    , cdVictoryPoints = Just 3
    }

lostAtSea :: CardDef
lostAtSea =
  (quest "the-grey-havens-nightmare-voyage-across-belegaer-003" "Lost at Sea" VoyageAcrossBelegaerNightmare 2 0)
    { cdQuestPoints = Nothing
    , cdKeywords = setFromList [Sailing]
    , cdVictoryPoints = Just 3
    }

sahirsAdvanceNightmare :: CardDef
sahirsAdvanceNightmare =
  (quest "the-grey-havens-nightmare-raid-on-the-grey-havens-002" "Sahír's Advance" RaidOnTheGreyHavensNightmare 2 0) {cdQuestPoints = Nothing}

-- The Lost Realm Nightmare Decks

shadowTheWarParty :: CardDef
shadowTheWarParty =
  (encounterSideQuest "the-lost-realm-nightmare-invaders-in-chetwood-009" "Shadow the War Party" IntrudersInChetwoodNightmare 8)
    { cdVictoryPoints = Just 10
    , cdWhenRevealed = True
    }

protectTheBreeLanders :: CardDef
protectTheBreeLanders =
  (encounterSideQuest "the-lost-realm-nightmare-invaders-in-chetwood-010" "Protect the Bree-landers" IntrudersInChetwoodNightmare 10)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    }

-- The Wastes of Eriador Nightmare

-- | No quest points: it leaves as its resource tokens say.
makeAStand :: CardDef
makeAStand =
  (encounterSideQuest "the-wastes-of-eriador-nightmare-the-wastes-of-eriador-007" "Make a Stand" TheWastesOfEriadorNightmare 0)
    { cdQuestPoints = Nothing
    , cdVictoryPoints = Just 10
    , cdWhenRevealed = True
    }

forcedOffTrack :: CardDef
forcedOffTrack =
  (encounterSideQuest "the-wastes-of-eriador-nightmare-the-wastes-of-eriador-008" "Forced Off Track" TheWastesOfEriadorNightmare 8)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    }

trackTheOrcs :: CardDef
trackTheOrcs =
  (encounterSideQuest "the-lost-realm-nightmare-the-weather-hills-018" "Track the Orcs" TheWeatherHillsNightmare 8)
    { cdVictoryPoints = Just 10
    , cdWhenRevealed = True
    }

gatherYourCourage :: CardDef
gatherYourCourage =
  (encounterSideQuest "the-lost-realm-nightmare-deadmens-dike-027" "Gather Your Courage" DeadmensDikeNightmare 8)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    , cdWhenRevealed = True
    }

-- Khazad-dûm Nightmare Decks

-- | X is 1 plus the number of players in the game: the stage counts them.
goblinAmbush :: CardDef
goblinAmbush = quest "khazad-dum-nightmare-into-the-pit-002" "Goblin Ambush" IntoThePitNightmare 2 1

nightmareSearchForAnExit :: CardCode -> Text -> Maybe Int -> Maybe Int -> CardDef
nightmareSearchForAnExit code subtitle questPoints victory =
  (quest code (Name "Search for an Exit" (Just subtitle)) FlightFromMoriaNightmare 2 0)
    { cdQuestPoints = questPoints
    , cdVictoryPoints = victory
    }

pursuedByShadowStage :: CardDef
pursuedByShadowStage = nightmareSearchForAnExit "khazad-dum-nightmare-flight-from-moria-002" "Pursued by Shadow" (Just 5) Nothing

-- | No quest points: The Nameless Fear's damage defeats it.
blockedByFlame :: CardDef
blockedByFlame = nightmareSearchForAnExit "khazad-dum-nightmare-flight-from-moria-003" "Blocked by Flame" Nothing (Just 1)

ghash :: CardDef
ghash = nightmareSearchForAnExit "khazad-dum-nightmare-flight-from-moria-004" "Ghâsh!" (Just 1) (Just 1)

-- The Nîn-in-Eilph Nightmare Deck: B sides in place of A Weary Passage's and Impassable Marshland's

throughTheMarshAnArduousJourney :: CardDef
throughTheMarshAnArduousJourney = (throughTheMarsh "the-nin-in-eilph-nightmare-the-nin-in-eilph-008" "An Arduous Journey") {cdEncounterSet = Just TheNinInEilphNightmare}

lostInTheSwanfleetDeadlyWaters :: CardDef
lostInTheSwanfleetDeadlyWaters = (lostInTheSwanfleet "the-nin-in-eilph-nightmare-the-nin-in-eilph-009" "Deadly Waters") {cdEncounterSet = Just TheNinInEilphNightmare}

-- Shadows of Mirkwood Nightmare Decks

grimbeornsFolly :: CardDef
grimbeornsFolly =
  (quest "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-011" "Grimbeorn's Folly" ConflictAtTheCarrockNightmare 3 0)
    { cdQuestPoints = Nothing
    }

-- | Road to Rivendell's nightmare stage 2, in place of Orc Outpost.
ambushInTheNight :: CardDef
ambushInTheNight = quest "road-to-rivendell-nightmare-road-to-rivendell-002" "Ambush in the Night" RoadToRivendellNightmare 2 12

-- Escape from Mount Gram Nightmare

raidGolfimbulsVault :: CardDef
raidGolfimbulsVault =
  (encounterSideQuest "escape-from-mount-gram-nightmare-escape-from-mount-gram-008" "Raid Golfimbul's Vault" EscapeFromMountGramNightmare 9)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge, Capture 3]
    }

-- Across the Ettenmoors Nightmare

intoTheEttenmoorsNightmare :: CardDef
intoTheEttenmoorsNightmare = quest "across-the-ettenmoors-nightmare-across-the-ettenmoors-002" "Into the Ettenmoors" AcrossTheEttenmoorsNightmare 1 10

isolated :: CardDef
isolated =
  (encounterSideQuest "across-the-ettenmoors-nightmare-across-the-ettenmoors-010" "Isolated" AcrossTheEttenmoorsNightmare 7)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    }

rescueFromTheTrolls :: CardDef
rescueFromTheTrolls =
  (encounterSideQuest "across-the-ettenmoors-nightmare-across-the-ettenmoors-011" "Rescue from the Trolls" AcrossTheEttenmoorsNightmare 15)
    { cdVictoryPoints = Just 10
    , cdWhenRevealed = True
    }

-- The Battle of Carn Dûm Nightmare

protectTheFlanks :: CardDef
protectTheFlanks =
  (encounterSideQuest "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-009" "Protect the Flanks" TheBattleOfCarnDumNightmare 6)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Siege]
    }

rallyTheFighters :: CardDef
rallyTheFighters =
  (encounterSideQuest "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-010" "Rally the Fighters" TheBattleOfCarnDumNightmare 8)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Surge]
    }

severTheHead :: CardDef
severTheHead =
  (encounterSideQuest "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-011" "Sever the Head" TheBattleOfCarnDumNightmare 6)
    { cdVictoryPoints = Just 10
    , cdKeywords = setFromList [Battle]
    }

-- The Dread Realm Nightmare

-- | No victory points: defeated, it goes to the encounter discard pile.
theWitchKingsGift :: CardDef
theWitchKingsGift =
  (encounterSideQuest "the-dread-realm-nightmare-the-dread-realm-009" "The Witch-king's Gift" TheDreadRealmNightmare 8)
    { cdKeywords = setFromList [Time 5]
    }
