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
      , determination
      , loreOfAncientArnor
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
      , hornsHornsHorns
      , farSighted
      , recklessAttack
      , defiantChallenge
      , theMasterRing
      , theRulingRing
      , theRingOfPower
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
      , doomHangsStill
      , holdYourGround
      , lordsOfTheEldar
      , quickEars
      , hourOfWrath
      , taleOfTinuviel
      , descendantsOfKings
      , boomedAndTrumpeted
      , leaveNoTrace
      , distantStars
      , keenAsLances
      , dunedainMessage
      , fairAndPerilous
      , noneReturn
      , hopeRekindled
      , reinforcements
      , hornsCry
      , theDoorIsClosed
      , rangerSummons
      , tirelessHunters
      , expertTrackers
      , rallyingCry
      , skywardVolley
      , anchorWatch
      , theEveningStar
      , elwingsFlight
      , lastStand
      , tidesOfFate
      , vanishFromSight
      , captainsWisdom
      , elevenses
      , revealedInWrath
      , scoutingParty
      , arrowsFromTheTrees
      , battleFury
      , interrogation
      , terribleToBehold
      , knifeWork
      , theHousesOfHealing
      , justiceShallBeDone
      , holdTheLine
      , inspiringPresence
      , galdorsCounselLeadership
      , galdorsCounselTactics
      , galdorsCounselSpirit
      , galdorsCounselLore
      , comeTogether
      , commandLeadership
      , commandLore
      , commandSpirit
      , commandTactics
      , unlikelyFriendship
      , wellWarned
      , dourHanded
      , proudHunters
      , backtrack
      , huntingParty
      , theFreePeoples
      , burstIntoSong
      , oldToby
      , fierceDefense
      , heirsOfEarendil
      , openTheArmory
      , bulwarkOfTheWest
      , flightToTheSea
      , theDamBursts
      , windFromTheSea
      , elfGuide
      , drinkingSong
      , theShirefolk
      , theKingsReturn
      , theGreatHunt
      , gwaihirsDebt
      , reforged
      , trafficFromDale
      , toArms
      , valourOfTheNorth
      , bartering
      , beornsRage
      , theHiddenWay
      , flightOfTheEagles
      , southAway
      , quickerThanSight
      , manTheWalls
      , familiarLands
      , swiftAndStrong
      , theGatheringStorm
      , tomBombadillo
      , chargeIntoBattle
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

hornsHornsHorns :: CardDef
hornsHornsHorns = (event "22056" "Horns! Horns! Horns!" Leadership (StaticCost 0)) {cdKeywords = setFromList [Doomed 2]}

farSighted :: CardDef
farSighted = (event "22058" "Far-sighted" Lore (StaticCost 0)) {cdKeywords = setFromList [Doomed 1]}

recklessAttack :: CardDef
recklessAttack = (event "22087" "Reckless Attack" Tactics (StaticCost 0)) {cdKeywords = setFromList [Doomed 2]}

defiantChallenge :: CardDef
defiantChallenge = (event "22089" "Defiant Challenge" Spirit (StaticCost 0)) {cdKeywords = setFromList [Doomed 2]}

theMasterRing :: CardDef
theMasterRing = (event "21009" "The Master Ring" Neutral (StaticCost 0)) {cdCardTraits = setFromList [Master]}

theRulingRing :: CardDef
theRulingRing = (event "21010" "The Ruling Ring" Neutral (StaticCost 0)) {cdCardTraits = setFromList [Master]}

theRingOfPower :: CardDef
theRingOfPower = (event "21011" "The Ring of Power" Neutral (StaticCost 0)) {cdCardTraits = setFromList [Master]}

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

doomHangsStill :: CardDef
doomHangsStill = event "10117" "Doom Hangs Still" Leadership (StaticCost 5)

holdYourGround :: CardDef
holdYourGround = event "10119" "Hold Your Ground!" Tactics (StaticCost 1)

lordsOfTheEldar :: CardDef
lordsOfTheEldar = (event "10121" "Lords of the Eldar" Spirit (StaticCost 3)) {cdPlayableFrom = FromDiscardOnly}

quickEars :: CardDef
quickEars = event "10123" "Quick Ears" Lore (StaticCost 1)

hourOfWrath :: CardDef
hourOfWrath = event "10144" "Hour of Wrath" Tactics (StaticCost 4)

taleOfTinuviel :: CardDef
taleOfTinuviel =
  (event "10146" "Tale of Tinúviel" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

descendantsOfKings :: CardDef
descendantsOfKings = event "10030" "Descendants of Kings" Leadership (StaticCost 1)

boomedAndTrumpeted :: CardDef
boomedAndTrumpeted = (event "10032" "Boomed and Trumpeted" Tactics (StaticCost 1)) {cdCardTraits = setFromList [Ent]}

leaveNoTrace :: CardDef
leaveNoTrace = event "10035" "Leave No Trace" Lore (StaticCost 1)

distantStars :: CardDef
distantStars = event "10036" "Distant Stars" Lore (StaticCost 0)

keenAsLances :: CardDef
keenAsLances = event "10037" "Keen as Lances" Neutral (StaticCost 5)

dunedainMessage :: CardDef
dunedainMessage = (event "10056" "Dúnedain Message" Leadership (StaticCost 1)) {cdCardTraits = setFromList [Signal]}

fairAndPerilous :: CardDef
fairAndPerilous = event "10060" "Fair and Perilous" Spirit (StaticCost 1)

noneReturn :: CardDef
noneReturn = event "10062" "None Return" Lore (StaticCost 1)

hopeRekindled :: CardDef
hopeRekindled = (event "10063" "Hope Rekindled" Neutral (StaticCost 0)) {cdCardTraits = setFromList [Song]}

reinforcements :: CardDef
reinforcements = (event "10085" "Reinforcements" Leadership (StaticCost 3)) {cdDistinctPayers = Just 3}

hornsCry :: CardDef
hornsCry = event "10088" "Horn's Cry" Tactics (StaticCost 2)

theDoorIsClosed :: CardDef
theDoorIsClosed = event "10092" "The Door is Closed!" Lore (StaticCost 1)

rangerSummons :: CardDef
rangerSummons = (event "09007" "Ranger Summons" Leadership (StaticCost 1)) {cdCardTraits = setFromList [Signal]}

tirelessHunters :: CardDef
tirelessHunters = event "09008" "Tireless Hunters" Tactics (StaticCost 1)

expertTrackers :: CardDef
expertTrackers = event "09009" "Expert Trackers" Lore (StaticCost 0)

rallyingCry :: CardDef
rallyingCry = event "10003" "Rallying Cry" Leadership (StaticCost 2)

skywardVolley :: CardDef
skywardVolley = event "11004" "Skyward Volley" Tactics (StaticCost 2)

anchorWatch :: CardDef
anchorWatch = event "11007" "Anchor Watch" Leadership (StaticCost 2)

theEveningStar :: CardDef
theEveningStar = event "11010" "The Evening Star" Lore (StaticCost 2)

elwingsFlight :: CardDef
elwingsFlight = event "11013" "Elwing's Flight" Spirit (StaticCost 2)

lastStand :: CardDef
lastStand = event "12005" "Last Stand" Tactics (StaticCost 0)

tidesOfFate :: CardDef
tidesOfFate = event "12007" "Tides of Fate" Spirit (StaticCost 0)

vanishFromSight :: CardDef
vanishFromSight = event "12011" "Vanish from Sight" Neutral (StaticCost 0)

captainsWisdom :: CardDef
captainsWisdom = event "12032" "Captain's Wisdom" Leadership (StaticCost 0)

elevenses :: CardDef
elevenses = event "12036" "Elevenses" Spirit (StaticCost 1)

revealedInWrath :: CardDef
revealedInWrath = event "12061" "Revealed in Wrath" Tactics (StaticCost 1)

scoutingParty :: CardDef
scoutingParty = event "12063" "Scouting Party" Spirit (StaticCost 2)

arrowsFromTheTrees :: CardDef
arrowsFromTheTrees = event "12065" "Arrows from the Trees" Lore (StaticCost 1)

battleFury :: CardDef
battleFury = event "12086" "Battle-fury" Tactics (StaticCost 1)

interrogation :: CardDef
interrogation = event "12090" "Interrogation" Lore (StaticCost 1)

terribleToBehold :: CardDef
terribleToBehold = event "12111" "Terrible to Behold" Leadership (StaticCost 1)

knifeWork :: CardDef
knifeWork = event "12114" "Knife-work" Tactics (StaticCost 1)

theHousesOfHealing :: CardDef
theHousesOfHealing = event "12118" "The Houses of Healing" Lore (StaticCost 5)

justiceShallBeDone :: CardDef
justiceShallBeDone =
  (event "12119" "Justice Shall Be Done" Neutral (StaticCost 0))
    { cdVictoryPoints = Just 1
    , cdDeckLimit = 1
    }

holdTheLine :: CardDef
holdTheLine = event "12140" "Hold the Line" Leadership (StaticCost 0)

inspiringPresence :: CardDef
inspiringPresence = event "12143" "Inspiring Presence" Spirit (StaticCost 2)

-- * The Dream-chaser Campaign Expansion: boons, scalable, each added to the victory
-- display as it is played

galdorsCounsel :: CardCode -> Sphere -> CardDef
galdorsCounsel code sphere = (event code "Galdor's Counsel" sphere (StaticCost 0)) {cdCardTraits = setFromList [Boon], cdVictoryPoints = Just 1}

galdorsCounselLeadership :: CardDef
galdorsCounselLeadership = galdorsCounsel "DCC184" Leadership

galdorsCounselTactics :: CardDef
galdorsCounselTactics = galdorsCounsel "DCC185" Tactics

galdorsCounselSpirit :: CardDef
galdorsCounselSpirit = galdorsCounsel "DCC186" Spirit

galdorsCounselLore :: CardDef
galdorsCounselLore = galdorsCounsel "DCC187" Lore

comeTogether :: CardDef
comeTogether = (event "DCC195" "Come Together" Neutral (StaticCost 1)) {cdCardTraits = setFromList [Boon], cdVictoryPoints = Just 1}

command :: CardCode -> Sphere -> CardDef
command code sphere = (event code "Command" sphere (StaticCost 1)) {cdCardTraits = setFromList [Tactic, Boon], cdVictoryPoints = Just 1}

commandLeadership :: CardDef
commandLeadership = command "DCC201" Leadership

commandLore :: CardDef
commandLore = command "DCC202" Lore

commandSpirit :: CardDef
commandSpirit = command "DCC203" Spirit

commandTactics :: CardDef
commandTactics = command "DCC204" Tactics
-- The Angmar Awakened campaign's boons

determination :: CardDef
determination = (event "AAC172" "Determination" Neutral (StaticCost 1)) {cdCardTraits = setFromList [Boon]}

loreOfAncientArnor :: CardDef
loreOfAncientArnor = (event "AAC190" "Lore of Ancient Arnor" Neutral (StaticCost 3)) {cdCardTraits = setFromList [Boon]}

unlikelyFriendship :: CardDef
unlikelyFriendship = event "16007" "Unlikely Friendship" Leadership (StaticCost 0)

wellWarned :: CardDef
wellWarned = event "16008" "Well Warned" Spirit (StaticCost 0)

dourHanded :: CardDef
dourHanded = event "16009" "Dour-handed" Tactics (StaticCost 1)

proudHunters :: CardDef
proudHunters = event "17032" "Proud Hunters" Tactics (StaticCost 0)

backtrack :: CardDef
backtrack = event "17034" "Backtrack" Spirit (StaticCost 1)

huntingParty :: CardDef
huntingParty = event "17057" "Hunting Party" Leadership (StaticCost 1)

theFreePeoples :: CardDef
theFreePeoples = event "17064" "The Free Peoples" Neutral (StaticCost 5)

burstIntoSong :: CardDef
burstIntoSong = event "17083" "Burst into Song" Leadership (StaticCost 2)

oldToby :: CardDef
oldToby =
  (event "17089" "Old Toby" Lore (StaticCost 2))
    { cdCardTraits = setFromList [Pipeweed]
    }

fierceDefense :: CardDef
fierceDefense = event "17113" "Fierce Defense" Tactics (StaticCost 3)

heirsOfEarendil :: CardDef
heirsOfEarendil = event "17115" "Heirs of Eärendil" Spirit (StaticCost 1)

openTheArmory :: CardDef
openTheArmory = event "17118" "Open the Armory" Neutral (StaticCost 1)

bulwarkOfTheWest :: CardDef
bulwarkOfTheWest = event "17137" "Bulwark of the West" Leadership (StaticCost 0)

flightToTheSea :: CardDef
flightToTheSea = event "17140" "Flight to the Sea" Spirit (StaticCost 2)

theDamBursts :: CardDef
theDamBursts = event "17142" "The Dam Bursts" Lore (StaticCost 2)

-- * The Crossings of Poros

-- | Encounter: an encounter card back and no cost. Set aside out of play at setup, it is
-- never in a player's deck.
windFromTheSea :: CardDef
windFromTheSea =
  (event "17144" "Wind from the Sea" Neutral (StaticCost 0))
    { cdCost = Nothing
    , cdKeywords = setFromList [Encounter]
    , cdShadowEffect = True
    }

elfGuide :: CardDef
elfGuide = event "19114" "Elf Guide" Leadership (StaticCost 0)

drinkingSong :: CardDef
drinkingSong = (event "19116" "Drinking Song" Lore (StaticCost 0)) {cdCardTraits = setFromList [Song]}

theShirefolk :: CardDef
theShirefolk = event "19121" "The Shirefolk" Neutral (StaticCost 0)

theKingsReturn :: CardDef
theKingsReturn = (event "19147" "The King's Return" Leadership (StaticCost 1)) {cdCardTraits = setFromList [Song]}

theGreatHunt :: CardDef
theGreatHunt = (event "19149" "The Great Hunt" Lore (StaticCost 3)) {cdDistinctPayers = Just 3}

gwaihirsDebt :: CardDef
gwaihirsDebt = event "19151" "Gwaihir's Debt" Tactics (StaticCost 1)

reforged :: CardDef
reforged = event "19153" "Reforged" Spirit DynamicCost
trafficFromDale :: CardDef
trafficFromDale = event "18012" "Traffic from Dale" Leadership (StaticCost 1)

toArms :: CardDef
toArms = event "18013" "To Arms!" Spirit (StaticCost 0)

valourOfTheNorth :: CardDef
valourOfTheNorth = event "18014" "Valour of the North" Tactics (StaticCost 0)

bartering :: CardDef
bartering = event "18015" "Bartering" Lore (StaticCost 0)

beornsRage :: CardDef
beornsRage = event "19005" "Beorn's Rage" Tactics (StaticCost 1)

theHiddenWay :: CardDef
theHiddenWay = event "19007" "The Hidden Way" Lore (StaticCost 2)
flightOfTheEagles :: CardDef
flightOfTheEagles = event "19032" "Flight of the Eagles" Tactics (StaticCost 0)

southAway :: CardDef
southAway =
  (event "19034" "South Away!" Spirit (StaticCost 0))
    { cdCardTraits = setFromList [Song]
    }

quickerThanSight :: CardDef
quickerThanSight = event "19064" "Quicker Than Sight" Spirit (StaticCost 0)

manTheWalls :: CardDef
manTheWalls = event "19086" "Man the Walls" Leadership (StaticCost 0)

familiarLands :: CardDef
familiarLands = event "19088" "Familiar Lands" Lore (StaticCost 1)

swiftAndStrong :: CardDef
swiftAndStrong =
  (event "19090" "Swift and Strong" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

theGatheringStorm :: CardDef
theGatheringStorm = event "21007" "The Gathering Storm" Leadership (StaticCost 0)

tomBombadillo :: CardDef
tomBombadillo =
  (event "22005" "Tom Bombadillo!" Lore (StaticCost 2))
    { cdCardTraits = setFromList [Song]
    }

chargeIntoBattle :: CardDef
chargeIntoBattle = event "22007" "Charge into Battle" Tactics (StaticCost 5)
