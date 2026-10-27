module LordOfTheRings.Quest.CardDefs (module LordOfTheRings.Quest.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude

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
      ]

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
