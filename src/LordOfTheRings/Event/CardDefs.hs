module LordOfTheRings.Event.CardDefs (module LordOfTheRings.Event.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere
import LordOfTheRings.Trait

allEventCards :: Map CardCode CardDef
allEventCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ everVigilant
      , commonCause
      , forGondor
      , sneakAttack
      , valiantSacrifice
      , grimResolve
      , bladeMastery
      , rainOfArrows
      , feint
      , quickStrike
      , thicketOfSpears
      , swiftStrike
      , standTogether
      , theGaladhrimsGreeting
      , strengthOfWill
      , hastyStroke
      , willOfTheWest
      , aTestOfWill
      , standAndFight
      , mendorsSupport
      , aLightInTheDark
      , dwarvenTomb
      , fortuneOrFate
      , loreOfImladris
      , loriensWealth
      , radagastsCunning
      , secretPaths
      , gandalfsSearch
      , beornsHospitality
      , astonishingSpeed
      , durinsSong
      , lureOfMoria
      , elrondsCounsel
      , daeronsRunes
      , weAreNotIdle
      , handsUponTheBow
      , behindStrongWalls
      , mutualAccord
      , wealthOfGondor
      , lightTheBeacons
      , aWatchfulPeace
      , forthEorlingas
      , hiddenCache
      , theTreePeople
      , feignedVoices
      , pursuingTheEnemy
      , chargeOfTheRohirrim
      , aVeryGoodTale
      , foeHammer
      , waitNoLonger
      , coneyInATrap
      , oathOfEorl
      , pillarsOfTheKings
      , needDrivesThem
      , hostOfGaladhrim
      , theMusterOfRohan
      , helmOfSecrecy
      , campfireTales
      , theEaglesAreComing
      , musteringTheRohirrim
      , stridersPath
      , secondBreakfast
      , partingGifts
      , toTheEyrie
      , infighting
      , rearGuard
      , meneldorsFlight
      , rideToRuin
      , gildorsCounsel
      , weDoNotSleep
      , dawnTakeYouAll
      , rumourFromTheEarth
      , shadowOfThePast
      , everOnward
      , khazadKhazad
      , untroubledByDarkness
      , ancestralKnowledge
      , takingInitiative
      , timelyAid
      , unseenStrike
      , renewedFriendship
      , needfulToKnow
      , aGoodHarvest
      , elvenLight
      , heedTheDream
      , hailOfStones
      , outOfTheWild
      , theEndComes
      , graveCairn
      , shortCut
      , freshTracks
      , outOfSight
      , wordOfCommand
      , heavyStroke
      , aElberethGilthoniel
      , peaceAndThought
      , riskSomeLight
      , gainingStrength
      , mithrandirsAdvice
      , strengthOfArms
      , trainedForWar
      , againstTheShadow
      , advanceWarning
      , gondorianDiscipline
      , smallTarget
      , hobbitSense
      , menOfTheWest
      , forestPatrol
      , theHammerStroke
      , childrenOfTheSea
      , wellEquipped
      , layOfNimrodel
      , legacyOfNumenor
      , deepKnowledge
      , theWizardsVoice
      , powerOfOrthanc
      , theSeeingStone
      , watersOfNimrodel
      , swiftAndSilent
      , noiselessMovement
      , courageAwakened
      , islandAmidPerils
      , followMe
      , shadowsGiveWay
      , freeToChoose
      , closeCall
      , theWhiteCouncil
      , messageFromElrond
      , tightenOurBelts
      , rideThemDown
      , dontBeHasty
      ]

everVigilant :: CardDef
everVigilant = event "01020" "Ever Vigilant" Leadership (StaticCost 1)

commonCause :: CardDef
commonCause = event "01021" "Common Cause" Leadership (StaticCost 0)

forGondor :: CardDef
forGondor = event "01022" "For Gondor!" Leadership (StaticCost 2)

sneakAttack :: CardDef
sneakAttack = event "01023" "Sneak Attack" Leadership (StaticCost 1)

valiantSacrifice :: CardDef
valiantSacrifice = event "01024" "Valiant Sacrifice" Leadership (StaticCost 1)

grimResolve :: CardDef
grimResolve = event "01025" "Grim Resolve" Leadership (StaticCost 5)

bladeMastery :: CardDef
bladeMastery = event "01032" "Blade Mastery" Tactics (StaticCost 1)

rainOfArrows :: CardDef
rainOfArrows = event "01033" "Rain of Arrows" Tactics (StaticCost 1)

feint :: CardDef
feint = event "01034" "Feint" Tactics (StaticCost 1)

quickStrike :: CardDef
quickStrike = event "01035" "Quick Strike" Tactics (StaticCost 1)

thicketOfSpears :: CardDef
thicketOfSpears = (\d -> d {cdDistinctPayers = Just 3}) $ event "01036" "Thicket of Spears" Tactics (StaticCost 3)

swiftStrike :: CardDef
swiftStrike = event "01037" "Swift Strike" Tactics (StaticCost 2)

standTogether :: CardDef
standTogether = event "01038" "Stand Together" Tactics (StaticCost 0)

theGaladhrimsGreeting :: CardDef
theGaladhrimsGreeting = event "01046" "The Galadhrim's Greeting" Spirit (StaticCost 3)

strengthOfWill :: CardDef
strengthOfWill = event "01047" "Strength of Will" Spirit (StaticCost 0)

hastyStroke :: CardDef
hastyStroke = event "01048" "Hasty Stroke" Spirit (StaticCost 1)

willOfTheWest :: CardDef
willOfTheWest = event "01049" "Will of the West" Spirit (StaticCost 1)

aTestOfWill :: CardDef
aTestOfWill = event "01050" "A Test of Will" Spirit (StaticCost 1)

mendorsSupport :: CardDef
mendorsSupport =
  (event "RC003" "Mendor's Support" Neutral (StaticCost 0))
    { cdCardTraits = setFromList [Boon]
    }

standAndFight :: CardDef
standAndFight = event "01051" "Stand and Fight" Spirit DynamicCost

aLightInTheDark :: CardDef
aLightInTheDark = event "01052" "A Light in the Dark" Spirit (StaticCost 2)

dwarvenTomb :: CardDef
dwarvenTomb = event "01053" "Dwarven Tomb" Spirit (StaticCost 1)

fortuneOrFate :: CardDef
fortuneOrFate = event "01054" "Fortune or Fate" Spirit (StaticCost 5)

loreOfImladris :: CardDef
loreOfImladris = event "01063" "Lore of Imladris" Lore (StaticCost 2)

loriensWealth :: CardDef
loriensWealth = event "01064" "Lórien's Wealth" Lore (StaticCost 3)

radagastsCunning :: CardDef
radagastsCunning = event "01065" "Radagast's Cunning" Lore (StaticCost 1)

secretPaths :: CardDef
secretPaths = event "01066" "Secret Paths" Lore (StaticCost 1)

gandalfsSearch :: CardDef
gandalfsSearch = event "01067" "Gandalf's Search" Lore DynamicCost

beornsHospitality :: CardDef
beornsHospitality = event "01068" "Beorn's Hospitality" Lore (StaticCost 5)

astonishingSpeed :: CardDef
astonishingSpeed = event "02122" "Astonishing Speed" Spirit (StaticCost 3)

durinsSong :: CardDef
durinsSong =
  (event "03004" "Durin's Song" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

lureOfMoria :: CardDef
lureOfMoria = event "04030" "Lure of Moria" Leadership (StaticCost 3)

elrondsCounsel :: CardDef
elrondsCounsel = event "04059" "Elrond's Counsel" Spirit (StaticCost 0)

daeronsRunes :: CardDef
daeronsRunes = event "04108" "Daeron's Runes" Lore (StaticCost 0)

weAreNotIdle :: CardDef
weAreNotIdle = event "04129" "We Are Not Idle" Leadership (StaticCost 0)

handsUponTheBow :: CardDef
handsUponTheBow = event "04131" "Hands Upon the Bow" Tactics (StaticCost 1)

behindStrongWalls :: CardDef
behindStrongWalls = event "05008" "Behind Strong Walls" Tactics (StaticCost 1)

mutualAccord :: CardDef
mutualAccord = event "05005" "Mutual Accord" Leadership (StaticCost 0)

wealthOfGondor :: CardDef
wealthOfGondor = event "05006" "Wealth of Gondor" Leadership (StaticCost 0)

lightTheBeacons :: CardDef
lightTheBeacons = event "05011" "Light the Beacons" Spirit (StaticCost 5)

aWatchfulPeace :: CardDef
aWatchfulPeace = event "05012" "A Watchful Peace" Spirit (StaticCost 1)

forthEorlingas :: CardDef
forthEorlingas = event "06138" "Forth Eorlingas!" Tactics (StaticCost 2)

hiddenCache :: CardDef
hiddenCache = event "06143" "Hidden Cache" Neutral (StaticCost 0)

theTreePeople :: CardDef
theTreePeople = event "08009" "The Tree People" Lore (StaticCost 0)

feignedVoices :: CardDef
feignedVoices = event "08027" "Feigned Voices" Leadership (StaticCost 0)

pursuingTheEnemy :: CardDef
pursuingTheEnemy = event "08060" "Pursuing the Enemy" Tactics (StaticCost 0)

chargeOfTheRohirrim :: CardDef
chargeOfTheRohirrim = event "08116" "Charge of the Rohirrim" Tactics (StaticCost 2)

aVeryGoodTale :: CardDef
aVeryGoodTale = event "131014" "A Very Good Tale" Leadership (StaticCost 0)

foeHammer :: CardDef
foeHammer = event "131015" "Foe-hammer" Tactics (StaticCost 0)

waitNoLonger :: CardDef
waitNoLonger = event "17005" "Wait no Longer" Tactics (StaticCost 2)

coneyInATrap :: CardDef
coneyInATrap = event "17009" "Coney in a Trap" Lore (StaticCost 1)

oathOfEorl :: CardDef
oathOfEorl = event "17085" "Oath of Eorl" Tactics (StaticCost 3)

pillarsOfTheKings :: CardDef
pillarsOfTheKings = event "22003" "Pillars of the Kings" Leadership (StaticCost 4)

needDrivesThem :: CardDef
needDrivesThem = event "22029" "Need Drives Them" Leadership (StaticCost 3)

hostOfGaladhrim :: CardDef
hostOfGaladhrim = event "22036" "Host of Galadhrim" Neutral (StaticCost 4)

theMusterOfRohan :: CardDef
theMusterOfRohan = event "22062" "The Muster of Rohan" Spirit (StaticCost 4)

helmOfSecrecy :: CardDef
helmOfSecrecy = (event "22090" "Helm of Secrecy" Neutral (StaticCost 4)) {cdDeckLimit = 1}

campfireTales :: CardDef
campfireTales = event "02003" "Campfire Tales" Leadership (StaticCost 1)

theEaglesAreComing :: CardDef
theEaglesAreComing =
  (event "02005" "The Eagles Are Coming!" Tactics (StaticCost 0))
    { cdCardTraits = setFromList [Eagle]
    }

musteringTheRohirrim :: CardDef
musteringTheRohirrim = event "02007" "Mustering the Rohirrim" Spirit (StaticCost 1)

stridersPath :: CardDef
stridersPath = event "02009" "Strider's Path" Lore (StaticCost 1)

secondBreakfast :: CardDef
secondBreakfast = event "02027" "Second Breakfast" Leadership (StaticCost 1)

partingGifts :: CardDef
partingGifts = event "02052" "Parting Gifts" Leadership (StaticCost 0)

toTheEyrie :: CardDef
toTheEyrie = event "02054" "To the Eyrie" Tactics (StaticCost 2)

infighting :: CardDef
infighting = event "02058" "Infighting" Lore (StaticCost 1)

rearGuard :: CardDef
rearGuard = event "02074" "Rear Guard" Leadership (StaticCost 1)

meneldorsFlight :: CardDef
meneldorsFlight = event "02076" "Meneldor's Flight" Tactics (StaticCost 0)

rideToRuin :: CardDef
rideToRuin = event "02078" "Ride to Ruin" Spirit (StaticCost 1)

gildorsCounsel :: CardDef
gildorsCounsel = event "02080" "Gildor's Counsel" Lore (StaticCost 3)

weDoNotSleep :: CardDef
weDoNotSleep = event "02101" "We Do Not Sleep" Spirit (StaticCost 5)

dawnTakeYouAll :: CardDef
dawnTakeYouAll = event "02118" "Dawn Take You All" Leadership (StaticCost 2)

rumourFromTheEarth :: CardDef
rumourFromTheEarth = event "02124" "Rumour from the Earth" Lore (StaticCost 0)

shadowOfThePast :: CardDef
shadowOfThePast = event "02125" "Shadow of the Past" Neutral (StaticCost 2)

everOnward :: CardDef
everOnward = event "03005" "Ever Onward" Leadership (StaticCost 3)

khazadKhazad :: CardDef
khazadKhazad = event "03008" "Khazâd! Khazâd!" Tactics (StaticCost 0)

untroubledByDarkness :: CardDef
untroubledByDarkness = event "03010" "Untroubled by Darkness" Spirit (StaticCost 2)

ancestralKnowledge :: CardDef
ancestralKnowledge = event "03012" "Ancestral Knowledge" Lore (StaticCost 1)

takingInitiative :: CardDef
takingInitiative = event "04002" "Taking Initiative" Leadership (StaticCost 0)

timelyAid :: CardDef
timelyAid = (event "04003" "Timely Aid" Leadership (StaticCost 4)) {cdKeywords = setFromList [Secrecy 3]}

unseenStrike :: CardDef
unseenStrike = event "04004" "Unseen Strike" Tactics (StaticCost 0)

renewedFriendship :: CardDef
renewedFriendship = event "04007" "Renewed Friendship" Spirit (StaticCost 0)

needfulToKnow :: CardDef
needfulToKnow = (event "04009" "Needful to Know" Lore (StaticCost 2)) {cdKeywords = setFromList [Secrecy 2]}

hailOfStones :: CardDef
hailOfStones = event "04032" "Hail of Stones" Tactics (StaticCost 1)

outOfTheWild :: CardDef
outOfTheWild = (event "04036" "Out of the Wild" Lore (StaticCost 3)) {cdKeywords = setFromList [Secrecy 2]}

theEndComes :: CardDef
theEndComes = event "04037" "The End Comes" Neutral (StaticCost 0)

graveCairn :: CardDef
graveCairn = event "04054" "Grave Cairn" Leadership (StaticCost 1)

shortCut :: CardDef
shortCut = event "04060" "Short Cut" Lore (StaticCost 1)

freshTracks :: CardDef
freshTracks = event "04078" "Fresh Tracks" Leadership (StaticCost 1)

outOfSight :: CardDef
outOfSight = (event "04081" "Out of Sight" Spirit (StaticCost 5)) {cdKeywords = setFromList [Secrecy 3]}

wordOfCommand :: CardDef
wordOfCommand = event "04084" "Word of Command" Lore (StaticCost 1)

heavyStroke :: CardDef
heavyStroke = event "04105" "Heavy Stroke" Tactics (StaticCost 1)

aElberethGilthoniel :: CardDef
aElberethGilthoniel = (event "04132" "A Elbereth! Gilthoniel!" Spirit (StaticCost 4)) {cdKeywords = setFromList [Secrecy 4]}

peaceAndThought :: CardDef
peaceAndThought = event "04135" "Peace, and Thought" Lore (StaticCost 1)

riskSomeLight :: CardDef
riskSomeLight = (event "04136" "Risk Some Light" Lore (StaticCost 3)) {cdKeywords = setFromList [Secrecy 3]}

aGoodHarvest :: CardDef
aGoodHarvest = event "06010" "A Good Harvest" Neutral (StaticCost 0)

elvenLight :: CardDef
elvenLight = (event "10145" "Elven-light" Spirit (StaticCost 1)) {cdPlayableFrom = FromDiscardOnly}

heedTheDream :: CardDef
heedTheDream = event "12009" "Heed the Dream" Lore (StaticCost 1)

gainingStrength :: CardDef
gainingStrength = event "06003" "Gaining Strength" Leadership (StaticCost 0)

mithrandirsAdvice :: CardDef
mithrandirsAdvice = event "06009" "Mithrandir's Advice" Lore (StaticCost 1)

strengthOfArms :: CardDef
strengthOfArms = event "06034" "Strength of Arms" Leadership (StaticCost 2)

trainedForWar :: CardDef
trainedForWar = event "06036" "Trained for War" Tactics (StaticCost 2)

againstTheShadow :: CardDef
againstTheShadow = event "06038" "Against the Shadow" Spirit (StaticCost 2)

advanceWarning :: CardDef
advanceWarning = event "06040" "Advance Warning" Lore (StaticCost 2)

gondorianDiscipline :: CardDef
gondorianDiscipline = (event "06060" "Gondorian Discipline" Tactics (StaticCost 0)) {cdCardTraits = setFromList [Gondor]}

smallTarget :: CardDef
smallTarget = event "06062" "Small Target" Spirit (StaticCost 1)

hobbitSense :: CardDef
hobbitSense = event "06065" "Hobbit-sense" Neutral (StaticCost 2)

menOfTheWest :: CardDef
menOfTheWest = (event "06083" "Men of the West" Leadership DynamicCost) {cdCardTraits = setFromList [Outlands]}

forestPatrol :: CardDef
forestPatrol = event "06089" "Forest Patrol" Lore (StaticCost 1)

theHammerStroke :: CardDef
theHammerStroke = event "06111" "The Hammer-stroke" Tactics (StaticCost 2)

childrenOfTheSea :: CardDef
childrenOfTheSea = event "06113" "Children of the Sea" Spirit (StaticCost 0)

wellEquipped :: CardDef
wellEquipped = event "06116" "Well-Equipped" Neutral (StaticCost 0)

layOfNimrodel :: CardDef
layOfNimrodel = (event "06140" "Lay of Nimrodel" Spirit (StaticCost 1)) {cdCardTraits = setFromList [Song]}

legacyOfNumenor :: CardDef
legacyOfNumenor = (event "07011" "Legacy of Númenor" Leadership (StaticCost 0)) {cdKeywords = setFromList [Doomed 4]}

deepKnowledge :: CardDef
deepKnowledge = (event "07012" "Deep Knowledge" Lore (StaticCost 0)) {cdKeywords = setFromList [Doomed 2]}

theWizardsVoice :: CardDef
theWizardsVoice =
  (event "07013" "The Wizards's Voice" Tactics (StaticCost 0))
    { cdCardTraits = setFromList [Spell]
    , cdKeywords = setFromList [Doomed 3]
    }

powerOfOrthanc :: CardDef
powerOfOrthanc =
  (event "07014" "Power of Orthanc" Spirit (StaticCost 0))
    { cdCardTraits = setFromList [Spell]
    , cdKeywords = setFromList [Doomed 2]
    }

theSeeingStone :: CardDef
theSeeingStone = (event "07015" "The Seeing-stone" Neutral (StaticCost 0)) {cdKeywords = setFromList [Doomed 1]}

watersOfNimrodel :: CardDef
watersOfNimrodel = (event "08145" "Waters of Nimrodel" Lore (StaticCost 3)) {cdKeywords = setFromList [Doomed 3]}

swiftAndSilent :: CardDef
swiftAndSilent = event "08003" "Swift and Silent" Leadership (StaticCost 1)

noiselessMovement :: CardDef
noiselessMovement = event "08033" "Noiseless Movement" Lore (StaticCost 1)

courageAwakened :: CardDef
courageAwakened = event "08061" "Courage Awakened" Spirit (StaticCost 1)

islandAmidPerils :: CardDef
islandAmidPerils = event "08090" "Island Amid Perils" Spirit (StaticCost 0)

followMe :: CardDef
followMe = event "08085" "Follow Me!" Leadership (StaticCost 1)

shadowsGiveWay :: CardDef
shadowsGiveWay = (event "08143" "Shadows Give Way" Spirit (StaticCost 3)) {cdDistinctPayers = Just 3}

freeToChoose :: CardDef
freeToChoose = event "08062" "Free to Choose" Spirit (StaticCost 0)

closeCall :: CardDef
closeCall = (event "08005" "Close Call" Tactics (StaticCost 0)) {cdKeywords = setFromList [Doomed 0]}

theWhiteCouncil :: CardDef
theWhiteCouncil = event "08010" "The White Council" Neutral XCost

messageFromElrond :: CardDef
messageFromElrond = event "08032" "Message from Elrond" Lore (StaticCost 0)

tightenOurBelts :: CardDef
tightenOurBelts = event "08086" "Tighten Our Belts" Leadership (StaticCost 0)

rideThemDown :: CardDef
rideThemDown = event "08142" "Ride Them Down" Spirit (StaticCost 2)

dontBeHasty :: CardDef
dontBeHasty = event "08144" "Don't Be Hasty!" Lore (StaticCost 0)
