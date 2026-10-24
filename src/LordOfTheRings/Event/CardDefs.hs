module LordOfTheRings.Event.CardDefs (module LordOfTheRings.Event.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
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
