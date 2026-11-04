module LordOfTheRings.Enemy.CardDefs (module LordOfTheRings.Enemy.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Keyword
import LordOfTheRings.Name (Name)
import LordOfTheRings.Prelude
import LordOfTheRings.Trait

allEnemyCards :: Map CardCode CardDef
allEnemyCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ kingSpider
      , cunningInformant
      , harborBrute
      , pelargirRingleader
      , mumakElite
      , haradrimMarksman
      , haradrimCaptain
      , southronMountedArcher
      , orcSaboteur
      , orcRavagerNightmare
      , ungoliantsSpawnNightmare
      , ungoliantsBrood
      , forestFlies
      , spidersOfMirkwood
      , marshlandOutlaws
      , anduinTrollSpawn
      , brownWaterRats
      , tortureMaster
      , crazedCaptive
      , spiderOfDolGuldur
      , goblinScout
      , goblinSwordsman
      , patrolLeader
      , goblinFollower
      , hummerhorns
      , ungoliantsSpawn
      , wolfRider
      , hillTroll
      , goblinSniper
      , marshAdder
      , wargs
      , dolGuldurOrcs
      , chieftanUfthak
      , dolGuldurBeastmaster
      , forestSpider
      , eastBightPatrol
      , blackForestBats
      , dungeonJailor
      , nazgulOfDolGuldur
      , cavernGuardian
      , mistyMountainGoblins
      , easternCrows
      , ungoliantsSwarm
      , goblintownScavengers
      , huntersFromMordor
      , louis
      , morris
      , stuart
      , rupert
      , muckAdder
      , mirkwoodFlock
      , blackForestBatsRhosgobel
      , orcHorseThieves
      , giantMarshWorm
      , mirkwoodBats
      , attercopAttercop
      , theNamelessFear
      , greatCaveTroll
      , orcDrummer
      , strayGoblin
      , chieftainOfThePit
      , blackUruks
      , mountainWarg
      , goblinSpearman
      , goblinArcher
      , caveTroll
      , orcHornBlower
      , caveSpider
      , goblinSneak
      , goblinWarlord
      , rockAdder
      , goblinTaskmaster
      , orcRaiders
      , crebain
      , wildBear
      , mountainGoblin
      , mountainTroll
      , snowWarg
      , namelessThing
      , elderNamelessThing
      , moriaBats
      , durinsBane
      , rangingGoblin
      , theWatcher
      , graspingTentacle
      , thrashingTentacle
      , strikingTentacle
      , orcScramblers
      , batteringRam
      , siegeRaft
      , lieutenantOfMordor
      , orcArbalesters
      , orcVanguard
      , orcRabble
      , orcArsonist
      , southronCompany
      , morgulSpider
      , forestBat
      , southronMercenaries
      , haradrimElite
      , mumak
      , harborThug
      , pickpocket
      , zealousTraitor
      , lossarnachBandit
      , umbarAssassin
      , ghulat
      , maraudingOrc
      , orcRavager
      , cravenEagle
      , druBuriDru
      , druadanDrummer
      , druadanElite
      , druadanHunter
      , druadanThief
      , morgulBodyguard
      , morgulSorcerer
      , morgulTracker
      , murzag
      , lordAlcaron
      , nazgulOfMinasMorgul
      , underworldDissident
      , daughterOfBeruthiel
      , theHandOfCastamir
      , telemnarsBane
      , urukLieutenant
      , urukSoldier
      , southronPhalanx
      , southronCommander
      , blackNumenorean
      , orcAmbusher
      , brutalUruk
      , evilCrow
      , mugashEnemy
      , mugashsGuard
      , methedrasOrc
      , darkHeartedHuorn
      , angryHuorn
      , deadlyHuorn
      , orcSkirmisher
      , orcHunter
      , orcHound
      , dunlandProwler
      , dunlendingBandit
      , dunlandRaider
      , dunlandTribesman
      , dunlandChieftain
      , dunlandBerserker
      , prowlingWolf
      , ravenChief
      , ravenWarrior
      , ravenSkirmisher
      , chiefTurchTheDunlandTrap
      , boarClanStalker
      , boarClanWarrior
      , boarClanAmbusher
      , boarClanSkirmisher
      , wolfSpirit
      , boarSpirit
      , ravenSpirit
      , bellachsHunter
      , orcSeeker
      , bloodHound
      , deadlyMarshAdder
      , swarmsOfMosquitos
      , bellachsBodyguard
      , littleSnuffler
      , dunlendingHorde
      , ravenClanElite
      , ravenClawDruid
      , bellachTroubleInTharbad
      , spyFromMordor
      , bellachsMarauder
      , bellachCelebrimborsSecret
      , bellachsScout
      , prowlingOrc
      , ancientMarshDweller
      , giantSwampAdder
      , neekerbreekers
      , boarsGuardian
      , wolfsGuardian
      , ravensGuardian
      , spiritOfTheWild
      , thaurdirCaptain
      , thaurdirChampion
      , carnDumGarrison
      , orcGrunts
      , werewolfOfAngmar
      , packLeader
      , northernWarg
      , bloodThirstyWarg
      , huntingPack
      , whiteWarg
      , wolfOfAngmar
      , jailorGornakh
      , cruelTorturer
      , dungeonGuard
      , goblinTormentor
      , orcWarParty
      , angmarOrc
      , angmarMarauder
      , angmarCaptain
      , corneredOrc
      , savageTrollspawn
      , coldfellGiant
      , cruelMountainTroll
      , ruthlessHillTroll
      , goblinPursuer
      , thaurdirDeadmensDike
      , thaurdirsDamned
      , balefulShade
      , deadLord
      , cursedDead
      , orcAbductor
      , soldierOfAngmar
      , rearguardAmbusher
      , angmarRaider
      , undeadHorde
      , cursedDeadNightmare
      , daechanar
      , dwimmerlaik
      , witchOfAngmar
      , wraithOfCarnDum
      , thaurdirTheTreacheryOfRhudaur
      , wightOfRhudaur
      , traitorousWight
      , scoutingShip
      , lightCruiser
      , corsairWarship
      , southronSailor
      , cunningPirate
      , viciousMarauder
      , umbarCaptain
      , umbarRaider
      , corsairInfiltrator
      , raidLeader
      , servantOfTheDeceiver
      , drownedDead
      , soullessCadaver
      , throngsOfUnfaithful
      , captainSahir
      , naasiyah
      , corsairArsonist
      , sahirsRavager
      , stormcaller
      , stormcallerCampaign
      , sahirsEscort
      , swiftRaider
      , corsairSkiff
      , corsairSeafarer
      , serpentsBlade
      , theThingInTheDepths
      , eyeOfTheBeast
      , thrashingArm
      , crushingArm
      , graspingArm
      , giantArm
      , seaScorpion
      , caveEel
      , templeGuardian
      , templeGuardianCampaign
      , faithlessDesecrator
      , islandWatcher
      , profaneDead
      , raiderFlagship
      , corsairSkirmisher
      , siegeShip
      , heavyCruiser
      , stormcallerTheCityOfCorsairs
      , slaveShip
      , umbarSlaver
      , captainSahirTheCityOfCorsairs
      , thunderstruck
      , crowley
      , revenant
      , graxar
      , seaSerpent
      , seaSerpentEnraged
      , recurringNightmare
      , dunlandAssassin
      , vengefulHillman
      , savageHillman
      , vigilantOrc
      , methedrasLookout
      , guardWarg
      , huornOfFangorn
      , blockingHuorn
      , wickedHuorn
      , brashCorsair
      , sahirsPursuer
      , guardianOfTheGoldenKing
      , corsairPillager
      , corsairPyromaniac
      , eliteMarauder
      , patrolSentry
      , goblinLurker
      , shadowDweller
      , overseerMaurul
      , orcTaskmaster
      , goblinSkirmisher
      , pitGoblin
      , swarmingGoblins
      , coalBlackOrc
      , raiderOfTheStorm
      , tideRunner
      , venomsBite
      , corsairDuelist
      , slitheringArm
      , crashingArm
      , kingsKnight
      , cursedSoldier
      , submergedDead
      , greatShark
      , underseaCaveTroll
      , rammingShip
      , fangOfSahir
      , patrolShip
      , corsairLoyalist
      , umbarPatrolNightmare
      , ravenousHawk
      , huntingWarg
      , savageHillTroll
      , robAndBob
      , louisNightmare
      , morrisNightmare
      , rupertNightmare
      , stuartNightmare
      , goblinEagleHunter
      , treetopBirdEater
      , tunnelingNamelessThing
      , orcInterceptor
      , thingsInThePools
      , marshWight
      , lostSoulOfLorien
      , illFatedGuard
      , ambushingOrcs
      , pursuingWargRider
      , webSpinner
      , warriorOfNurn
      , orcCaptor
      , sentryOfTheWhiteBridge
      , blackNumenoreanNightmare
      , orcTrickster
      , urukMarksman
      , druBuriDruNightmare
      , druadanWarrior
      , druadanStalker
      , orcRaidersNightmare
      , pillagingOrc
      , invadingOrc
      , haradrimWarrior
      , murderousTurncoat
      , faithlessConspirator
      , wolfpeltGoblin
      , silvertineMarauder
      , snowGiant
      , urukElfStalker
      , goblinTrapper
      , raidingParty
      , viciousTrackers
      , theWatcherNightmare
      , writhingTentacle
      , chokingTentacle
      , goblinStalker
      , trapdoorSpider
      , sentinelOfTheDeep
      , mithrilSeeker
      , giantCaveCentipede
      , unspeakableThing
      , primevalThing
      , servantOfFlame
      , captainOfMordor
      , packHunters
      , jailorGornakhNightmare
      , prisonGuard
      , orcOfThaurdir
      , hoarwellRiverSerpent
      , scavengingStoneTroll
      , giantBeetle
      , thaurdirsHunter
      , undeadThrall
      , apostateOfAngmar
      , orcWight
      , thaurdirsElite
      , silentGuardian
      , necromancerOfCarnDum
      , southronCaptain
      , southronSoldier
      , southronArcher
      , sandViper
      , giantScorpion
      , urukChieftain
      , urukOfMordor
      , urukWargRider
      , mordorWarg
      , sauronsEnforcers
      , broodMother
      , hiveGuardian
      , spiderBroodling
      , wereWorm
      , carrionBird
      , umbarSentry
      , urukPursuer
      , racingWarg
      , theBlackSerpent
      , serpentGuard
      , jailerOfCirithGurat
      , prisonGuardCirithGurat
      , dungeonWarg
      , wildMumak
      , haradTiger
      , territorialApe
      , stranglingPython
      , giantCentipede
      , theBlackSerpentCrossingsOfPoros
      , southronOutrider
      , urukProwler
      , desertWarg
      , goblinTroop
      , strayGoblinGreyMountains
      , packOfWargs
      , stoneTroll
      , hobgoblin
      , werewolf
      , giantSpider
      , blackBats
      , urdug
      , urdugsElite
      , fierceVanguard
      , wargRider
      , dagnir
      , theDragonsThrall
      , orcOfTheFlame
      , fireDrake
      , framsShade
      , cryptStalker
      , evilSpectre
      , dagnirMountGundabad
      , guardianOfGundabad
      , dagnirsSlave
      , deepDweller
      , coldDrake
      , snowTroll
      , dragonHatchling
      , greyMountainGiant
      , huntingEagle
      , wilderlandBear
      , urdugEnemy
      , tinyEnemy
      , mirkwoodPatrol
      , ravenousSpider
      , gollumASitE
      , khamulTheEasterling
      , easterlingCaptain
      , riderOfRhun
      , easterlingRaider
      , warriorOfRhun
      , messengerFromMordor
      , servantOfSauron
      , rhunBackstabber
      , treacherousEasterling
      , ulchorsGuard
      , easterlingOutrider
      , thaneUlchorTheTempleOfDoom
      , cityGuard
      , ulfastSentry
      , theKingsElite
      , ulchorsAgent
      , namelessBeast
      , mordorCaveTroll
      , ashMountainWerewolf
      , spawnOfShelob
      , torechGorgorPatrol
      , priestOfSauron
      , theGovernor
      , blackNumenoreanASitE
      , thaneUlchorWrathAndRuin
      , championOfRhun
      , easterlingSkirmisher
      , wainriderChampion
      , aggressiveRacer
      , chariotDriver
      , veteranWainrider
      , thaneUlchorTheFortressOfNurn
      , soldierOfNurn
      , easterlingMercenary
      , mordorOlogHai
      , guardDogs
      , ulwarth
      , lorgan
      , borlach
      , easterlingGuard
      , nurnSlaver
      , deadManOfDunharrow
      , faithlessDead
      , ghostsOfMen
      , shadowHost
      , armyOfTheDead
      , watcherInTheWater
      , greatWargChief
      , houndOfSauron
      , howlingWarg
      , crebainFromDunland
      , theBalrog
      , orcChieftain
      , greatCaveTrollJourneyInTheDark
      , urukFromMordor
      , moriaOrc
      , moriaArcher
      ]

namelessThing :: CardDef
namelessThing =
  (enemy "04125" "Nameless Thing" FoundationsOfStone 5 27 3 0 3 0)
    { cdCardTraits = setFromList [Nameless]
    , cdVictoryPoints = Just 1
    }

elderNamelessThing :: CardDef
elderNamelessThing =
  (enemy "04126" "Elder Nameless Thing" FoundationsOfStone 3 40 4 0 4 0)
    { cdCardTraits = setFromList [Nameless]
    , cdVictoryPoints = Just 3
    }

moriaBats :: CardDef
moriaBats =
  (enemy "04127" "Moria Bats" FoundationsOfStone 4 33 1 1 2 1)
    { cdCardTraits = setFromList [Creature]
    }

patrolLeader :: CardDef
patrolLeader =
  (enemy "03014" "Patrol Leader" IntoThePit 2 30 3 4 3 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

goblinFollower :: CardDef
goblinFollower =
  (enemy "03035" "Goblin Follower" TwistsAndTurns 4 33 1 3 2 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

goblinScout :: CardDef
goblinScout =
  (enemy "03060" "Goblin Scout" GoblinsOfTheDeep 3 37 3 1 0 2)
    { cdCardTraits = setFromList [Goblin, Orc, Scout]
    }

goblinSwordsman :: CardDef
goblinSwordsman =
  (enemy "03061" "Goblin Swordsman" GoblinsOfTheDeep 5 20 1 3 1 2)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

kingSpider :: CardDef
kingSpider =
  (enemy "01074" "King Spider" SpidersOfMirkwood 2 20 2 3 1 3)
    { cdShadowEffect = True
    , cdWhenRevealed = True
    , cdCardTraits = setFromList [Creature, Spider]
    }

hummerhorns :: CardDef
hummerhorns =
  (enemy "01075" "Hummerhorns" SpidersOfMirkwood 1 40 1 2 0 3)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Creature, Insect]
    , cdVictoryPoints = Just 5
    , cdEasyModeQuantity = Just 0
    }

ungoliantsSpawn :: CardDef
ungoliantsSpawn =
  (enemy "01076" "Ungoliant's Spawn" SpidersOfMirkwood 1 32 3 5 2 9)
    { cdShadowEffect = True
    , cdWhenRevealed = True
    , cdCardTraits = setFromList [Creature, Spider]
    }

wolfRider :: CardDef
wolfRider =
  (enemy "01081" "Wolf Rider" Wilderlands 1 10 1 2 0 2)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Goblin, Orc]
    , cdKeywords = setFromList [Surge]
    }

hillTroll :: CardDef
hillTroll =
  (enemy "01082" "Hill Troll" Wilderlands 2 30 1 6 3 9)
    { cdCardTraits = setFromList [Troll]
    , cdVictoryPoints = Just 4
    , cdEasyModeQuantity = Just 1
    }

goblinSniper :: CardDef
goblinSniper =
  (enemy "01083" "Goblin Sniper" Wilderlands 2 48 2 2 0 2)
    { cdCardTraits = setFromList [Goblin, Orc]
    }

marshAdder :: CardDef
marshAdder =
  (enemy "01084" "Marsh Adder" Wilderlands 1 40 3 4 1 7)
    { cdCardTraits = setFromList [Creature]
    , cdVictoryPoints = Just 3
    , cdEasyModeQuantity = Just 0
    }

wargs :: CardDef
wargs =
  (enemy "01085" "Wargs" Wilderlands 2 20 2 3 1 3)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Creature]
    }

dolGuldurOrcs :: CardDef
dolGuldurOrcs =
  (enemy "01089" "Dol Guldur Orcs" DolGuldurOrcs 3 10 2 2 0 3)
    { cdShadowEffect = True
    , cdWhenRevealed = True
    , cdCardTraits = setFromList [DolGuldur, Orc]
    }

chieftanUfthak :: CardDef
chieftanUfthak =
  (enemy "01090" "Chieftan Ufthak" DolGuldurOrcs 1 35 2 3 3 6)
    { cdCardTraits = setFromList [DolGuldur, Orc]
    , cdVictoryPoints = Just 4
    , cdEasyModeQuantity = Just 0
    }

dolGuldurBeastmaster :: CardDef
dolGuldurBeastmaster =
  (enemy "01091" "Dol Guldur Beastmaster" DolGuldurOrcs 2 35 2 3 1 5)
    { cdCardTraits = setFromList [DolGuldur, Orc]
    , cdEasyModeQuantity = Just 1
    }

forestSpider :: CardDef
forestSpider =
  (enemy "01096" "Forest Spider" PassageThroughMirkwood 4 25 2 2 1 4)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Creature, Spider]
    }

eastBightPatrol :: CardDef
eastBightPatrol =
  (enemy "01097" "East Bight Patrol" PassageThroughMirkwood 1 5 3 3 1 2)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Goblin, Orc]
    }

blackForestBats :: CardDef
blackForestBats =
  (enemy "01098" "Black Forest Bats" PassageThroughMirkwood 1 15 1 1 0 2)
    { cdCardTraits = setFromList [Creature]
    , cdWhenRevealed = True
    }

dungeonJailor :: CardDef
dungeonJailor =
  (enemy "01101" "Dungeon Jailor" EscapeFromDolGuldur 2 38 1 2 3 5)
    { cdCardTraits = setFromList [DolGuldur, Orc]
    , cdVictoryPoints = Just 5
    , cdEasyModeQuantity = Just 0
    }

nazgulOfDolGuldur :: CardDef
nazgulOfDolGuldur =
  (enemy "01102" "Nazgûl of Dol Guldur" EscapeFromDolGuldur 1 40 5 4 3 9)
    { cdCardTraits = setFromList [Nazgul]
    }

cavernGuardian :: CardDef
cavernGuardian =
  (enemy "01103" "Cavern Guardian" EscapeFromDolGuldur 2 8 2 2 1 2)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Undead]
    , cdKeywords = setFromList [Doomed 1]
    }

mistyMountainGoblins :: CardDef
mistyMountainGoblins =
  (enemy "01111" "Misty Mountain Goblins" JourneyAlongTheAnduin 3 15 2 2 1 3)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Goblin, Orc]
    }

easternCrows :: CardDef
easternCrows =
  (enemy "01115" "Eastern Crows" SauronsReach 3 30 1 1 0 1)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

ungoliantsSwarm :: CardDef
ungoliantsSwarm =
  (enemy "RC004" "Ungoliant's Swarm" PassageThroughMirkwoodCampaign 1 33 3 2 2 5)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Creature, Spider, Burden]
    }

goblintownScavengers :: CardDef
goblintownScavengers =
  (enemy "02020" "Goblintown Scavengers" TheHuntForGollum 2 12 1 1 0 3)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 1
    }

huntersFromMordor :: CardDef
huntersFromMordor =
  (enemy "02021" "Hunters from Mordor" TheHuntForGollum 5 34 2 2 2 6)
    { cdCardTraits = setFromList [Mordor]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

louis :: CardDef
louis =
  (enemy "02038" "Louis" ConflictAtTheCarrock 1 34 2 4 2 10)
    { cdUnique = True
    , cdCardTraits = setFromList [Troll]
    }

morris :: CardDef
morris =
  (enemy "02039" "Morris" ConflictAtTheCarrock 1 34 2 4 2 10)
    { cdUnique = True
    , cdCardTraits = setFromList [Troll]
    }

stuart :: CardDef
stuart =
  (enemy "02040" "Stuart" ConflictAtTheCarrock 1 34 2 4 2 10)
    { cdUnique = True
    , cdCardTraits = setFromList [Troll]
    }

rupert :: CardDef
rupert =
  (enemy "02041" "Rupert" ConflictAtTheCarrock 1 34 2 4 2 10)
    { cdUnique = True
    , cdCardTraits = setFromList [Troll]
    }

muckAdder :: CardDef
muckAdder =
  (enemy "02042" "Muck Adder" ConflictAtTheCarrock 4 20 1 2 0 4)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

mirkwoodFlock :: CardDef
mirkwoodFlock =
  (enemy "02070" "Mirkwood Flock" AJourneyToRhosgobel 4 32 1 2 1 3)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

blackForestBatsRhosgobel :: CardDef
blackForestBatsRhosgobel =
  (enemy "02071" "Black Forest Bats" AJourneyToRhosgobel 5 26 1 1 0 2)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

orcHorseThieves :: CardDef
orcHorseThieves =
  (enemy "02094" "Orc Horse Thieves" TheHillsOfEmynMuil 3 35 3 1 2 6)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdKeywords = setFromList [Doomed 2]
    , cdEasyModeQuantity = Just 2
    }

giantMarshWorm :: CardDef
giantMarshWorm =
  (enemy "02112" "Giant Marsh Worm" TheDeadMarshes 4 36 1 3 2 6)
    { cdCardTraits = setFromList [Creature]
    , cdEscape = Just 2
    , cdEasyModeQuantity = Just 2
    }

mirkwoodBats :: CardDef
mirkwoodBats =
  (enemy "02138" "Mirkwood Bats" ReturnToMirkwood 4 22 1 1 1 1)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 2
    }

attercopAttercop :: CardDef
attercopAttercop =
  (enemy "02139" "Attercop, Attercop" ReturnToMirkwood 3 44 2 8 4 6)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdEasyModeQuantity = Just 0
    }

blackUruks :: CardDef
blackUruks =
  (enemy "03054" "Black Uruks" MistyMountains 4 32 2 3 3 2)
    { cdCardTraits = setFromList [Uruk, Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

mountainWarg :: CardDef
mountainWarg =
  (enemy "03055" "Mountain Warg" MistyMountains 3 30 2 4 2 4)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

chieftainOfThePit :: CardDef
chieftainOfThePit =
  (enemy "03030" "Chieftain of the Pit" PlunderingGoblins 1 27 2 5 2 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

goblinSpearman :: CardDef
goblinSpearman =
  (enemy "03031" "Goblin Spearman" PlunderingGoblins 5 15 2 2 2 2)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

goblinArcher :: CardDef
goblinArcher =
  (enemy "03032" "Goblin Archer" PlunderingGoblins 2 48 2 1 3 1)
    { cdCardTraits = setFromList [Goblin, Orc]
    }

caveTroll :: CardDef
caveTroll =
  (enemy "03020" "Cave-troll" TheSeventhLevel 2 33 4 6 4 7)
    { cdCardTraits = setFromList [Troll]
    , cdVictoryPoints = Just 2
    , cdEasyModeQuantity = Just 1
    }

orcHornBlower :: CardDef
orcHornBlower =
  (enemy "03021" "Orc Horn Blower" TheSeventhLevel 1 45 2 1 1 3)
    { cdCardTraits = setFromList [Orc, Summoner]
    , cdKeywords = setFromList [Surge]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 0
    }

greatCaveTroll :: CardDef
greatCaveTroll =
  (enemy "03042" "Great Cave-troll" DeepsOfMoria 2 38 2 7 3 10)
    { cdCardTraits = setFromList [Troll]
    , cdVictoryPoints = Just 3
    , cdEasyModeQuantity = Just 0
    }

orcDrummer :: CardDef
orcDrummer =
  (enemy "03043" "Orc Drummer" DeepsOfMoria 1 50 1 1 3 1)
    { cdCardTraits = setFromList [Orc, Summoner]
    , cdEasyModeQuantity = Just 0
    }

-- | X is the number of players in the game.
strayGoblin :: CardDef
strayGoblin =
  (enemy "03044" "Stray Goblin" DeepsOfMoria 3 29 0 0 2 2)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

-- | X is the number of victory points in the victory display.
theNamelessFear :: CardDef
theNamelessFear =
  (enemy "03025" "The Nameless Fear" FlightFromMoria 1 50 0 0 0 27)
    { cdCardTraits = setFromList [Flame, Shadow]
    }

caveSpider :: CardDef
caveSpider =
  (enemy "04097" "Cave Spider" TheLongDark 3 31 3 2 1 4)
    { cdCardTraits = setFromList [Spider, Creature]
    , cdWhenRevealed = True
    }

goblinSneak :: CardDef
goblinSneak =
  (enemy "04098" "Goblin Sneak" TheLongDark 4 15 2 1 1 2)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

goblinWarlord :: CardDef
goblinWarlord =
  (enemy "04099" "Goblin Warlord" TheLongDark 1 39 4 3 3 5)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

rockAdder :: CardDef
rockAdder =
  (enemy "04100" "Rock Adder" TheLongDark 3 20 1 3 0 3)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }


goblinTaskmaster :: CardDef
goblinTaskmaster =
  (enemy "04049" "Goblin Taskmaster" RoadToRivendell 4 27 2 2 2 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdKeywords = setFromList [Ambush]
    }

orcRaiders :: CardDef
orcRaiders =
  (enemy "04050" "Orc Raiders" RoadToRivendell 3 21 1 3 1 3)
    { cdCardTraits = setFromList [Orc]
    , cdKeywords = setFromList [Ambush]
    }

crebain :: CardDef
crebain =
  (enemy "04051" "Crebain" RoadToRivendell 3 35 2 0 0 3)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

wildBear :: CardDef
wildBear =
  (enemy "04052" "Wild Bear" RoadToRivendell 3 34 0 2 3 5)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Ambush]
    }


mountainGoblin :: CardDef
mountainGoblin =
  (enemy "04025" "Mountain Goblin" TheRedhornGate 3 25 1 2 2 3)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

mountainTroll :: CardDef
mountainTroll =
  (enemy "04026" "Mountain Troll" TheRedhornGate 2 35 2 5 5 7)
    { cdCardTraits = setFromList [Troll]
    , cdShadowEffect = True
    }

snowWarg :: CardDef
snowWarg =
  (enemy "04027" "Snow Warg" TheRedhornGate 3 28 3 3 1 4)
    { cdCardTraits = setFromList [Creature, Snow]
    }


durinsBane :: CardDef
durinsBane =
  (enemy "04150" "Durin's Bane" ShadowAndFlame 1 1 4 6 3 27)
    { cdCardTraits = setFromList [Balrog, Flame, Shadow]
    , cdKeywords = setFromList [Regenerate 3, Indestructible]
    , cdUnique = True
    }

rangingGoblin :: CardDef
rangingGoblin =
  (enemy "04151" "Ranging Goblin" ShadowAndFlame 3 35 2 1 1 2)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdWhenRevealed = True
    }


theWatcher :: CardDef
theWatcher =
  (enemy "04072" "The Watcher" TheWatcherInTheWater 1 48 4 5 7 9)
    { cdUnique = True
    , cdCardTraits = setFromList [Creature, Tentacle]
    , cdKeywords = setFromList [Regenerate 2]
    , cdVictoryPoints = Just 3
    }

graspingTentacle :: CardDef
graspingTentacle =
  (enemy "04073" "Grasping Tentacle" TheWatcherInTheWater 4 12 2 3 0 3)
    { cdCardTraits = setFromList [Tentacle]
    }

thrashingTentacle :: CardDef
thrashingTentacle =
  (enemy "04074" "Thrashing Tentacle" TheWatcherInTheWater 4 12 2 3 0 3)
    { cdCardTraits = setFromList [Tentacle]
    }

strikingTentacle :: CardDef
strikingTentacle =
  (enemy "04075" "Striking Tentacle" TheWatcherInTheWater 4 18 2 4 1 3)
    { cdCardTraits = setFromList [Tentacle]
    }

orcScramblers :: CardDef
orcScramblers =
  (enemy "05030" "Orc Scramblers" TheSiegeOfCairAndros 3 15 1 2 1 2)
    { cdCardTraits = setFromList [Orc, Besieger]
    , cdKeywords = setFromList [Surge]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

batteringRam :: CardDef
batteringRam =
  (enemy "05031" "Battering Ram" TheSiegeOfCairAndros 3 33 2 7 3 5)
    { cdCardTraits = setFromList [Besieger]
    , cdKeywords = setFromList [Doomed 2]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

siegeRaft :: CardDef
siegeRaft =
  (enemy "05032" "Siege Raft" TheSiegeOfCairAndros 2 30 3 4 1 6)
    { cdCardTraits = setFromList [Besieger]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

lieutenantOfMordor :: CardDef
lieutenantOfMordor =
  (enemy "05056" "Lieutenant of Mordor" MordorElite 1 33 2 5 2 5)
    { cdCardTraits = setFromList [Mordor]
    , cdVictoryPoints = Just 3
    , cdWhenRevealed = True
    }

orcArbalesters :: CardDef
orcArbalesters =
  (enemy "05057" "Orc Arbalesters" MordorElite 3 35 2 1 2 5)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdShadowEffect = True
    }

orcVanguard :: CardDef
orcVanguard =
  (enemy "05058" "Orc Vanguard" MordorElite 2 40 2 8 3 5)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

orcRabble :: CardDef
orcRabble =
  (enemy "05062" "Orc Rabble" RavagingOrcs 3 28 2 1 1 3)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

orcArsonist :: CardDef
orcArsonist =
  (enemy "05063" "Orc Arsonist" RavagingOrcs 3 30 3 3 2 3)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdShadowEffect = True
    }

southronCompany :: CardDef
southronCompany =
  (enemy "05027" "Southron Company" IntoIthilien 4 34 1 3 1 5)
    { cdCardTraits = setFromList [Harad]
    , cdShadowEffect = True
    }

morgulSpider :: CardDef
morgulSpider =
  (enemy "05049" "Morgul Spider" CreaturesOfTheForest 3 25 3 4 1 5)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 1
    }

forestBat :: CardDef
forestBat =
  (enemy "05050" "Forest Bat" CreaturesOfTheForest 3 18 1 1 1 1)
    { cdCardTraits = setFromList [Creature]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

-- | Archery X, X the number of players: gained from its own modifiers.
southronMercenaries :: CardDef
southronMercenaries =
  (enemy "05052" "Southron Mercenaries" Southrons 3 35 2 3 2 4)
    { cdCardTraits = setFromList [Harad]
    , cdShadowEffect = True
    }

haradrimElite :: CardDef
haradrimElite =
  (enemy "05053" "Haradrim Elite" Southrons 2 27 3 4 3 3)
    { cdCardTraits = setFromList [Harad]
    , cdShadowEffect = True
    }

mumak :: CardDef
mumak =
  (enemy "05054" "Mumak" Southrons 1 38 4 7 3 12)
    { cdCardTraits = setFromList [Harad, Creature]
    , cdEasyModeQuantity = Just 0
    }

harborThug :: CardDef
harborThug =
  (enemy "05020" "Harbor Thug" PerilInPelargir 5 25 2 3 1 3)
    { cdCardTraits = setFromList [Thug]
    , cdShadowEffect = True
    }

pickpocket :: CardDef
pickpocket =
  (enemy "05037" "Pickpocket" StreetsOfGondor 3 28 3 1 0 2)
    { cdCardTraits = setFromList [Brigand]
    , cdShadowEffect = True
    }

zealousTraitor :: CardDef
zealousTraitor =
  (enemy "05042" "Zealous Traitor" Brigands 3 17 2 3 2 2)
    { cdCardTraits = setFromList [Brigand]
    }

lossarnachBandit :: CardDef
lossarnachBandit =
  (enemy "05043" "Lossarnach Bandit" Brigands 2 24 3 3 3 3)
    { cdCardTraits = setFromList [Brigand]
    }

umbarAssassin :: CardDef
umbarAssassin =
  (enemy "05044" "Umbar Assassin" Brigands 1 40 4 5 1 5)
    { cdCardTraits = setFromList [Brigand]
    , cdKeywords = setFromList [Archery 2]
    , cdEasyModeQuantity = Just 0
    }

-- | X attack, X the damage on Dead Villagers: gained from its own modifiers.
ghulat :: CardDef
ghulat =
  unique
    (enemy "06071" "Ghulat" EncounterAtAmonDin 1 30 3 0 3 7)
      { cdCardTraits = setFromList [Orc, Uruk]
      , cdVictoryPoints = Just 2
      }

maraudingOrc :: CardDef
maraudingOrc =
  (enemy "06072" "Marauding Orc" EncounterAtAmonDin 4 25 2 4 1 4)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

orcRavager :: CardDef
orcRavager =
  (enemy "06073" "Orc Ravager" EncounterAtAmonDin 3 35 2 3 1 3)
    { cdCardTraits = setFromList [Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

cravenEagle :: CardDef
cravenEagle =
  (enemy "06074" "Craven Eagle" EncounterAtAmonDin 2 40 3 5 2 3)
    { cdCardTraits = setFromList [Creature, Eagle]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 0
    }

druBuriDru :: CardDef
druBuriDru =
  (enemy "06045" "Drû-buri-Drû" TheDruadanForest 1 1 4 5 3 6)
    { cdCardTraits = setFromList [Wose]
    , cdUnique = True
    }

druadanDrummer :: CardDef
druadanDrummer =
  (enemy "06046" "Drúadan Drummer" TheDruadanForest 3 40 0 1 2 2)
    { cdCardTraits = setFromList [Wose]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | Prowl X, X the number of players: resolved by the card as it is revealed.
druadanElite :: CardDef
druadanElite =
  (enemy "06047" "Drúadan Elite" TheDruadanForest 3 35 2 3 2 4)
    { cdCardTraits = setFromList [Wose]
    , cdKeywords = setFromList [Archery 2]
    , cdShadowEffect = True
    }

-- | Archery X, X the number of heroes with no resources: gained from its own modifiers.
druadanHunter :: CardDef
druadanHunter =
  (enemy "06048" "Drúadan Hunter" TheDruadanForest 4 25 2 4 3 3)
    { cdCardTraits = setFromList [Wose]
    , cdKeywords = setFromList [Prowl 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

druadanThief :: CardDef
druadanThief =
  (enemy "06049" "Drúadan Thief" TheDruadanForest 3 5 2 2 1 2)
    { cdCardTraits = setFromList [Wose]
    , cdKeywords = setFromList [Surge, Prowl 1]
    , cdShadowEffect = True
    }

morgulBodyguard :: CardDef
morgulBodyguard =
  (enemy "06148" "Morgul Bodyguard" TheMorgulVale 4 35 1 2 2 6)
    { cdCardTraits = setFromList [Mordor]
    , cdShadowEffect = True
    }

morgulSorcerer :: CardDef
morgulSorcerer =
  (enemy "06149" "Morgul Sorcerer" TheMorgulVale 2 5 1 3 1 3)
    { cdCardTraits = setFromList [Mordor]
    , cdShadowEffect = True
    }

morgulTracker :: CardDef
morgulTracker =
  (enemy "06150" "Morgul Tracker" TheMorgulVale 3 40 4 1 0 3)
    { cdCardTraits = setFromList [Orc, Mordor]
    , cdShadowEffect = True
    }

murzag :: CardDef
murzag =
  unique
    (enemy "06151" "Murzag" TheMorgulVale 1 30 2 5 2 7)
      { cdCardTraits = setFromList [Orc, Uruk, Captain]
      }

lordAlcaron :: CardDef
lordAlcaron =
  unique
    (enemy "06152" "Lord Alcaron" TheMorgulVale 1 10 3 4 3 5)
      { cdCardTraits = setFromList [Gondor, Traitor, Captain]
      }

nazgulOfMinasMorgul :: CardDef
nazgulOfMinasMorgul =
  unique
    (enemy "06153" "Nazgûl of Minas Morgul" TheMorgulVale 1 1 4 7 4 5)
      { cdCardTraits = setFromList [Nazgul, Captain]
      }

underworldDissident :: CardDef
underworldDissident =
  (enemy "06014" "Underworld Dissident" TheStewardsFear 5 25 3 2 2 2)
    { cdCardTraits = setFromList [Brigand]
    , cdShadowEffect = True
    }

daughterOfBeruthiel :: CardDef
daughterOfBeruthiel =
  unique
    (enemy "06015" "Daughter of Berúthiel" TheStewardsFear 1 10 3 5 3 5)
      { cdCardTraits = setFromList [Gondor, Villain]
      }

theHandOfCastamir :: CardDef
theHandOfCastamir =
  unique
    (enemy "06016" "The Hand of Castamir" TheStewardsFear 1 30 3 4 3 6)
      { cdCardTraits = setFromList [Gondor, Villain]
      }

telemnarsBane :: CardDef
telemnarsBane =
  unique
    (enemy "06017" "Telemnar's Bane" TheStewardsFear 1 20 3 3 2 7)
      { cdCardTraits = setFromList [Gondor, Villain]
      }

urukLieutenant :: CardDef
urukLieutenant =
  (enemy "06092" "Uruk Lieutenant" AssaultOnOsgiliath 2 40 2 6 2 3)
    { cdCardTraits = setFromList [Orc, Uruk, Mordor]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

urukSoldier :: CardDef
urukSoldier =
  (enemy "06093" "Uruk Soldier" AssaultOnOsgiliath 4 30 1 4 1 2)
    { cdCardTraits = setFromList [Orc, Uruk, Mordor]
    , cdShadowEffect = True
    }

southronPhalanx :: CardDef
southronPhalanx =
  (enemy "06094" "Southron Phalanx" AssaultOnOsgiliath 3 25 1 1 2 3)
    { cdCardTraits = setFromList [Harad]
    , cdShadowEffect = True
    }

southronCommander :: CardDef
southronCommander =
  (enemy "06095" "Southron Commander" AssaultOnOsgiliath 2 37 3 5 3 6)
    { cdCardTraits = setFromList [Harad]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 0
    }

-- | X threat, X the total number of hidden cards in the game: given by its own modifiers.
blackNumenorean :: CardDef
blackNumenorean =
  (enemy "06119" "Black Númenorean" TheBloodOfGondor 1 35 0 5 3 5)
    { cdCardTraits = setFromList [Mordor]
    }

orcAmbusher :: CardDef
orcAmbusher =
  (enemy "06120" "Orc Ambusher" TheBloodOfGondor 4 10 2 3 1 3)
    { cdCardTraits = setFromList [Orc, Mordor]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

brutalUruk :: CardDef
brutalUruk =
  (enemy "06121" "Brutal Uruk" TheBloodOfGondor 3 20 3 4 1 5)
    { cdCardTraits = setFromList [Orc, Uruk, Mordor]
    , cdEasyModeQuantity = Just 1
    }

evilCrow :: CardDef
evilCrow =
  (enemy "06122" "Evil Crow" TheBloodOfGondor 2 5 1 1 0 2)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

-- | The players' Mugash: shuffled into an out-of-play deck at setup, never the encounter
-- deck. Into Fangorn has an objective of the name.
mugashEnemy :: CardDef
mugashEnemy =
  unique
    (enemy "07024" "Mugash" ToCatchAnOrc 1 1 4 7 4 8)
      { cdCardTraits = setFromList [Orc, Uruk]
      }

-- | Shuffled into the out-of-play decks at setup, the copies left over set aside.
mugashsGuard :: CardDef
mugashsGuard =
  (enemy "07025" "Mugash's Guard" ToCatchAnOrc 3 40 3 6 3 6)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdVictoryPoints = Just 3
    }

methedrasOrc :: CardDef
methedrasOrc =
  (enemy "07026" "Methedras Orc" ToCatchAnOrc 3 30 3 5 3 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

darkHeartedHuorn :: CardDef
darkHeartedHuorn =
  (enemy "07033" "Dark-Hearted Huorn" IntoFangorn 4 1 2 4 4 5)
    { cdCardTraits = setFromList [Huorn]
    , cdKeywords = setFromList [Hinder]
    , cdEasyModeQuantity = Just 3
    }

angryHuorn :: CardDef
angryHuorn =
  (enemy "07034" "Angry Huorn" IntoFangorn 3 38 3 5 4 8)
    { cdCardTraits = setFromList [Huorn]
    , cdKeywords = setFromList [Hinder]
    , cdEasyModeQuantity = Just 2
    }

deadlyHuorn :: CardDef
deadlyHuorn =
  (enemy "07035" "Deadly Huorn" IntoFangorn 3 34 2 3 4 6)
    { cdCardTraits = setFromList [Huorn]
    , cdKeywords = setFromList [Hinder]
    , cdEasyModeQuantity = Just 2
    }

orcSkirmisher :: CardDef
orcSkirmisher =
  (enemy "07038" "Orc Skirmisher" MistyMountainOrcs 3 33 3 4 2 4)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

orcHunter :: CardDef
orcHunter =
  (enemy "07039" "Orc Hunter" MistyMountainOrcs 3 35 2 3 1 3)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 1
    }

orcHound :: CardDef
orcHound =
  (enemy "07040" "Orc Hound" MistyMountainOrcs 2 20 1 2 1 2)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

-- | Surge and +1 threat while players hold enough cards: given by its own modifiers.
dunlandProwler :: CardDef
dunlandProwler = (enemy "07042" "Dunland Prowler" DunlandRaiders 3 30 1 2 3 3) {cdCardTraits = setFromList [Dunland]}

dunlendingBandit :: CardDef
dunlendingBandit =
  (enemy "07043" "Dunlending Bandit" DunlandRaiders 2 33 2 1 2 4)
    { cdCardTraits = setFromList [Dunland]
    , cdShadowEffect = True
    }

dunlandRaider :: CardDef
dunlandRaider =
  (enemy "07044" "Dunland Raider" DunlandRaiders 2 35 3 5 3 5)
    { cdCardTraits = setFromList [Dunland]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

dunlandTribesman :: CardDef
dunlandTribesman =
  (enemy "07046" "Dunland Tribesman" DunlandWarriors 2 37 0 4 2 3)
    { cdCardTraits = setFromList [Dunland]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

dunlandChieftain :: CardDef
dunlandChieftain =
  (enemy "07047" "Dunland Chieftain" DunlandWarriors 2 35 3 5 3 6)
    { cdCardTraits = setFromList [Dunland]
    , cdEasyModeQuantity = Just 0
    }

dunlandBerserker :: CardDef
dunlandBerserker =
  (enemy "07048" "Dunland Berserker" DunlandWarriors 3 25 2 2 1 4)
    { cdCardTraits = setFromList [Dunland]
    , cdShadowEffect = True
    }

prowlingWolf :: CardDef
prowlingWolf =
  (enemy "07053" "Prowling Wolf" BrokenLands 3 28 3 1 1 3)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

ravenChief :: CardDef
ravenChief =
  unique
    (enemy "08151" "Raven Chief" TheAntleredCrown 1 15 4 5 5 9)
      { cdCardTraits = setFromList [Dunland, RavenClan]
      , cdVictoryPoints = Just 3
      }

ravenWarrior :: CardDef
ravenWarrior =
  (enemy "08153" "Raven Warrior" TheAntleredCrown 4 36 3 4 2 5)
    { cdCardTraits = setFromList [Dunland, RavenClan]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 3
    }

ravenSkirmisher :: CardDef
ravenSkirmisher =
  (enemy "08154" "Raven Skirmisher" TheAntleredCrown 3 26 2 3 2 2)
    { cdCardTraits = setFromList [Dunland, RavenClan]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 2
    }

-- | Cannot leave play: not even destroyed, so it is indestructible.
chiefTurchTheDunlandTrap :: CardDef
chiefTurchTheDunlandTrap =
  unique
    (enemy "08014" "Chief Turch" TheDunlandTrap 1 50 3 5 3 5)
      { cdCardTraits = setFromList [Dunland, BoarClan]
      , cdKeywords = setFromList [Indestructible]
      }

boarClanStalker :: CardDef
boarClanStalker =
  (enemy "08017" "Boar Clan Stalker" TheDunlandTrap 3 28 3 0 0 5)
    { cdCardTraits = setFromList [Dunland, BoarClan]
    , cdEasyModeQuantity = Just 2
    }

boarClanWarrior :: CardDef
boarClanWarrior =
  (enemy "08018" "Boar Clan Warrior" TheDunlandTrap 4 20 2 3 1 4)
    { cdCardTraits = setFromList [Dunland, BoarClan]
    , cdShadowEffect = True
    }

-- | X threat, X the number of players.
bellachTroubleInTharbad :: CardDef
bellachTroubleInTharbad =
  unique
    (enemy "08070" "Bellach" TroubleInTharbad 1 50 0 5 4 7)
      { cdCardTraits = setFromList [Mordor, Spy]
      }

spyFromMordor :: CardDef
spyFromMordor =
  (enemy "08072" "Spy from Mordor" TroubleInTharbad 4 40 2 3 2 4)
    { cdCardTraits = setFromList [Mordor, Orc, Spy]
    }

bellachsMarauder :: CardDef
bellachsMarauder =
  (enemy "08073" "Bellach's Marauder" TroubleInTharbad 2 30 3 5 4 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | X threat, X the cards underneath The Orcs' Search: given by its own modifiers.
bellachCelebrimborsSecret :: CardDef
bellachCelebrimborsSecret =
  unique
    (enemy "08124" "Bellach" CelebrimborsSecret 1 50 0 5 4 7)
      { cdCardTraits = setFromList [Mordor, Spy]
      , cdKeywords = setFromList [Scour]
      , cdVictoryPoints = Just 5
      }

bellachsScout :: CardDef
bellachsScout =
  (enemy "08128" "Bellach's Scout" CelebrimborsSecret 4 40 2 3 2 4)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdKeywords = setFromList [Scour]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 3
    }

prowlingOrc :: CardDef
prowlingOrc =
  (enemy "08129" "Prowling Orc" CelebrimborsSecret 2 45 3 5 3 5)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdKeywords = setFromList [Scour]
    , cdEasyModeQuantity = Just 1
    }

-- | Set aside at setup, out of play, until a stage adds it to the staging area.
ancientMarshDweller :: CardDef
ancientMarshDweller =
  (enemy "08103" "Ancient Marsh-dweller" TheNinInEilph 1 45 3 6 4 9)
    { cdCardTraits = setFromList [Creature]
    , cdVictoryPoints = Just 5
    }

giantSwampAdder :: CardDef
giantSwampAdder =
  (enemy "08104" "Giant Swamp Adder" TheNinInEilph 4 35 3 3 3 6)
    { cdCardTraits = setFromList [Creature]
    , cdEasyModeQuantity = Just 3
    }

neekerbreekers :: CardDef
neekerbreekers =
  (enemy "08105" "Neekerbreekers" TheNinInEilph 3 20 2 1 1 6)
    { cdCardTraits = setFromList [Creature, Insect]
    , cdShadowEffect = True
    }

boarsGuardian :: CardDef
boarsGuardian =
  (enemy "08040" "Boar's Guardian" TheThreeTrials 1 50 1 4 4 10)
    { cdCardTraits = setFromList [Guardian, Boar]
    , cdKeywords = setFromList [Time 2]
    , cdVictoryPoints = Just 3
    }

wolfsGuardian :: CardDef
wolfsGuardian =
  (enemy "08041" "Wolf's Guardian" TheThreeTrials 1 50 2 5 3 12)
    { cdCardTraits = setFromList [Guardian, Wolf]
    , cdKeywords = setFromList [Time 3]
    , cdVictoryPoints = Just 3
    }

ravensGuardian :: CardDef
ravensGuardian =
  (enemy "08042" "Raven's Guardian" TheThreeTrials 1 50 3 3 5 8)
    { cdCardTraits = setFromList [Guardian, Raven]
    , cdKeywords = setFromList [Time 4]
    , cdVictoryPoints = Just 3
    }

spiritOfTheWild :: CardDef
spiritOfTheWild =
  (enemy "08050" "Spirit of the Wild" TheThreeTrials 5 28 1 1 2 4)
    { cdCardTraits = setFromList [Undead, SpiritTrait]
    , cdShadowEffect = True
    }

corneredOrc :: CardDef
corneredOrc =
  (enemy "09025" "Cornered Orc" TheWeatherHills 5 20 2 2 1 2)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 4
    }

-- | Indestructible. Cannot have attachments. Its Captain side; it starts in the staging
-- area as the scenario sets up, so it is never shuffled into the encounter deck.
thaurdirCaptain :: CardDef
thaurdirCaptain =
  unique
    (enemy "10127" "Thaurdir" TheBattleOfCarnDum 1 50 4 6 4 9)
      { cdCardTraits = setFromList [Undead, Wraith, Captain]
      , cdKeywords = setFromList [Indestructible]
      }

-- | Thaurdir's Champion side, the same card flipped: no copy of its own in the encounter
-- deck.
thaurdirChampion :: CardDef
thaurdirChampion =
  unique
    (enemy "10128" "Thaurdir" TheBattleOfCarnDum 0 50 4 6 4 9)
      { cdCardTraits = setFromList [Undead, Wraith, Champion]
      , cdKeywords = setFromList [Indestructible]
      }

carnDumGarrison :: CardDef
carnDumGarrison =
  (enemy "10129" "Carn Dûm Garrison" TheBattleOfCarnDum 4 40 1 4 3 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 3
    }

orcGrunts :: CardDef
orcGrunts =
  (enemy "10130" "Orc Grunts" TheBattleOfCarnDum 4 8 1 2 1 3)
    { cdCardTraits = setFromList [Orc]
    , cdKeywords = setFromList [Doomed 1, Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

werewolfOfAngmar :: CardDef
werewolfOfAngmar =
  (enemy "10131" "Werewolf of Angmar" TheBattleOfCarnDum 2 35 2 5 4 6)
    { cdCardTraits = setFromList [Creature, Werewolf]
    , cdEasyModeQuantity = Just 0
    }

-- | Set aside at setup, out of play, until Battle with the Pack adds it to the staging area.
packLeader :: CardDef
packLeader =
  unique
    (enemy "10017" "Pack Leader" TheWastesOfEriador 1 1 4 5 4 8)
      { cdCardTraits = setFromList [Creature, Warg]
      , cdVictoryPoints = Just 3
      }

northernWarg :: CardDef
northernWarg =
  (enemy "10018" "Northern Warg" TheWastesOfEriador 4 30 2 3 1 4)
    { cdCardTraits = setFromList [Creature, Warg]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 3
    }

bloodThirstyWarg :: CardDef
bloodThirstyWarg = (enemy "10019" "Blood-thirsty Warg" TheWastesOfEriador 2 35 2 4 2 5) {cdCardTraits = setFromList [Creature, Warg], cdEasyModeQuantity = Just 1}

huntingPack :: CardDef
huntingPack = (enemy "10020" "Hunting Pack" TheWastesOfEriador 2 40 3 6 3 6) {cdCardTraits = setFromList [Creature, Warg], cdEasyModeQuantity = Just 1}

whiteWarg :: CardDef
whiteWarg =
  (enemy "10021" "White Warg" TheWastesOfEriador 3 20 2 2 3 3)
    { cdCardTraits = setFromList [Creature, Warg]
    , cdShadowEffect = True
    }

wolfOfAngmar :: CardDef
wolfOfAngmar =
  (enemy "10022" "Wolf of Angmar" TheWastesOfEriador 3 25 1 2 1 3)
    { cdCardTraits = setFromList [Creature, Warg]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

jailorGornakh :: CardDef
jailorGornakh =
  unique
    (enemy "10041" "Jailor Gornákh" EscapeFromMountGram 1 44 3 5 3 8)
      { cdCardTraits = setFromList [Goblin, Orc]
      , cdVictoryPoints = Just 5
      }

cruelTorturer :: CardDef
cruelTorturer =
  (enemy "10043" "Cruel Torturer" EscapeFromMountGram 2 36 2 3 2 3)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdKeywords = setFromList [Capture 1]
    }

dungeonGuard :: CardDef
dungeonGuard =
  (enemy "10044" "Dungeon Guard" EscapeFromMountGram 3 16 1 2 1 2)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdKeywords = setFromList [Capture 1]
    }

-- | X attack, X the number of cards in play with captured cards underneath them.
goblinTormentor :: CardDef
goblinTormentor =
  (enemy "10045" "Goblin Tormentor" EscapeFromMountGram 2 28 2 0 1 3)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

orcWarParty :: CardDef
orcWarParty =
  (enemy "09017" "Orc War Party" IntrudersInChetwood 3 40 3 6 3 6)
    { cdCardTraits = setFromList [Orc]
    , cdEasyModeQuantity = Just 1
    }

angmarOrc :: CardDef
angmarOrc =
  (enemy "09043" "Angmar Orc" AngmarOrcs 3 25 2 2 3 3)
    { cdCardTraits = setFromList [Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

angmarMarauder :: CardDef
angmarMarauder =
  (enemy "09044" "Angmar Marauder" AngmarOrcs 3 30 3 3 2 4)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

angmarCaptain :: CardDef
angmarCaptain =
  (enemy "09045" "Angmar Captain" AngmarOrcs 2 35 3 5 2 5)
    { cdCardTraits = setFromList [Orc]
    , cdEasyModeQuantity = Just 1
    }

-- | It gets +1 attack for each damaged character the engaged player controls: from its
-- own modifiers.
savageTrollspawn :: CardDef
savageTrollspawn =
  (enemy "10072" "Savage Trollspawn" AcrossTheEttenmoors 4 36 1 2 3 6)
    { cdCardTraits = setFromList [Troll]
    , cdShadowEffect = True
    }

coldfellGiant :: CardDef
coldfellGiant =
  (enemy "10073" "Coldfell Giant" AcrossTheEttenmoors 2 41 3 5 4 7)
    { cdCardTraits = setFromList [Giant]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

cruelMountainTroll :: CardDef
cruelMountainTroll =
  (enemy "10074" "Cruel Mountain-troll" AcrossTheEttenmoors 2 46 3 6 2 9)
    { cdCardTraits = setFromList [Troll]
    , cdEasyModeQuantity = Just 0
    }

ruthlessHillTroll :: CardDef
ruthlessHillTroll =
  (enemy "10075" "Ruthless Hill-troll" AcrossTheEttenmoors 2 31 2 4 1 12)
    { cdCardTraits = setFromList [Troll]
    , cdShadowEffect = True
    }

goblinPursuer :: CardDef
goblinPursuer =
  (enemy "10076" "Goblin Pursuer" AcrossTheEttenmoors 2 16 2 2 2 3)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

thaurdirDeadmensDike :: CardDef
thaurdirDeadmensDike =
  unique
    (enemy "09033" "Thaurdir" DeadmensDike 1 1 4 6 4 9)
      { cdCardTraits = setFromList [Undead, Wraith]
      , cdKeywords = setFromList [Indestructible]
      }

thaurdirsDamned :: CardDef
thaurdirsDamned = (enemy "09034" "Thaurdir's Damned" DeadmensDike 3 35 3 5 2 6) {cdCardTraits = setFromList [Undead, Wraith], cdEasyModeQuantity = Just 1}

balefulShade :: CardDef
balefulShade =
  (enemy "09035" "Baleful Shade" DeadmensDike 4 30 2 2 1 5)
    { cdCardTraits = setFromList [Undead, Wraith]
    , cdShadowEffect = True
    }

deadLord :: CardDef
deadLord =
  (enemy "09060" "Dead Lord" CursedDead 2 33 3 4 2 3)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

cursedDead :: CardDef
cursedDead =
  (enemy "09061" "Cursed Dead" CursedDead 5 27 1 3 1 2)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 4
    }

daechanar :: CardDef
daechanar =
  (enemy "10153" "Daechanar" TheDreadRealm 1 1 5 4 5 12)
    { cdUnique = True
    , cdCardTraits = setFromList [Undead, Dunedain, Sorcerer]
    , cdVictoryPoints = Just 10
    }

dwimmerlaik :: CardDef
dwimmerlaik =
  (enemy "10155" "Dwimmerlaik" TheDreadRealm 3 34 4 5 0 9)
    { cdCardTraits = setFromList [Undead, Wraith]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

witchOfAngmar :: CardDef
witchOfAngmar =
  (enemy "10156" "Witch of Angmar" TheDreadRealm 3 26 3 2 2 4)
    { cdCardTraits = setFromList [Undead, Sorcerer]
    , cdEasyModeQuantity = Just 2
    }

wraithOfCarnDum :: CardDef
wraithOfCarnDum =
  (enemy "10157" "Wraith of Carn Dûm" TheDreadRealm 2 18 2 4 0 5)
    { cdCardTraits = setFromList [Undead, Wraith]
    }

-- | Indestructible. Cannot have attachments, from its own modifiers.
thaurdirTheTreacheryOfRhudaur :: CardDef
thaurdirTheTreacheryOfRhudaur =
  unique
    (enemy "10098" "Thaurdir" TheTreacheryOfRhudaur 1 1 4 6 4 9)
      { cdCardTraits = setFromList [Undead, Wraith]
      , cdKeywords = setFromList [Indestructible]
      }

-- | Surge or Doomed 2, by the current quest's progress, from its own card.
wightOfRhudaur :: CardDef
wightOfRhudaur =
  (enemy "10099" "Wight of Rhudaur" TheTreacheryOfRhudaur 2 23 2 3 3 5)
    { cdCardTraits = setFromList [Undead, Wight]
    }

traitorousWight :: CardDef
traitorousWight =
  (enemy "10100" "Traitorous Wight" TheTreacheryOfRhudaur 3 32 2 4 2 4)
    { cdCardTraits = setFromList [Undead, Wight]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

-- * The Dream-chaser cycle: Voyage Across Belegaer, Umbar Navy, Corsair Pirates, Corsair Raiders

scoutingShip :: CardDef
scoutingShip =
  (enemy "11022" "Scouting Ship" VoyageAcrossBelegaer 3 35 2 5 2 7)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 1]
    }

lightCruiser :: CardDef
lightCruiser =
  (enemy "11057" "Light Cruiser" UmbarNavy 2 37 4 6 5 7)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 2]
    }

corsairWarship :: CardDef
corsairWarship =
  (enemy "11058" "Corsair Warship" UmbarNavy 2 44 3 8 4 9)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 3]
    , cdVictoryPoints = Just 6
    , cdEasyModeQuantity = Just 0
    }

southronSailor :: CardDef
southronSailor =
  (enemy "11060" "Southron Sailor" CorsairPirates 2 28 0 2 1 4)
    { cdCardTraits = setFromList [Corsair]
    }

cunningPirate :: CardDef
cunningPirate =
  (enemy "11061" "Cunning Pirate" CorsairPirates 3 36 1 2 2 4)
    { cdCardTraits = setFromList [Corsair, Raider]
    }

viciousMarauder :: CardDef
viciousMarauder =
  (enemy "11062" "Vicious Marauder" CorsairPirates 2 12 2 3 3 3)
    { cdCardTraits = setFromList [Corsair, Raider]
    }

umbarCaptain :: CardDef
umbarCaptain =
  (enemy "11063" "Umbar Captain" CorsairPirates 2 40 4 5 3 5)
    { cdCardTraits = setFromList [Corsair]
    , cdEasyModeQuantity = Just 0
    }

umbarRaider :: CardDef
umbarRaider =
  (enemy "11064" "Umbar Raider" CorsairRaiders 3 24 2 3 2 4)
    { cdCardTraits = setFromList [Corsair, Raider]
    }

corsairInfiltrator :: CardDef
corsairInfiltrator =
  (enemy "11065" "Corsair Infiltrator" CorsairRaiders 2 16 2 3 3 2)
    { cdCardTraits = setFromList [Corsair, Raider]
    }

raidLeader :: CardDef
raidLeader =
  (enemy "11066" "Raid Leader" CorsairRaiders 2 40 3 5 1 5)
    { cdCardTraits = setFromList [Corsair, Raider]
    , cdEasyModeQuantity = Just 0
    }

servantOfTheDeceiver :: CardDef
servantOfTheDeceiver = (enemy "11029" "Servant of the Deceiver" TheFateOfNumenor 3 35 3 3 4 3) {cdCardTraits = setFromList [Undead], cdEasyModeQuantity = Just 2}

drownedDead :: CardDef
drownedDead =
  (enemy "11068" "Drowned Dead" DrownedDead 3 32 2 4 3 3)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    }

soullessCadaver :: CardDef
soullessCadaver = (enemy "11069" "Soulless Cadaver" DrownedDead 2 38 3 3 2 4) {cdCardTraits = setFromList [Undead], cdEasyModeQuantity = Just 1}

throngsOfUnfaithful :: CardDef
throngsOfUnfaithful =
  (enemy "11070" "Throngs of Unfaithful" DrownedDead 2 26 1 2 0 4)
    { cdCardTraits = setFromList [Undead]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

-- | The enemy side of The Stormcaller Elite's Captain Sahír: never shuffled into the
-- encounter deck, his scenarios put him into play. Flipped, he is 'captainSahirObjective'.
captainSahir :: CardDef
captainSahir =
  unique
    (enemy "11076" "Captain Sahír" TheStormcallerElite 1 4 6 6 2 5)
      { cdCardTraits = setFromList [Corsair, Raider]
      , cdVictoryPoints = Just 6
      , cdOtherSide = Just "11077"
      }

-- | The enemy side of Na'asiyah: flipped, she is 'naasiyahObjective'.
naasiyah :: CardDef
naasiyah =
  unique
    (enemy "11078" "Na'asiyah" TheStormcallerElite 1 8 1 2 2 4)
      { cdCardTraits = setFromList [Corsair, Raider]
      , cdVictoryPoints = Just 4
      , cdOtherSide = Just "11079"
      }

corsairArsonist :: CardDef
corsairArsonist =
  (enemy "11040" "Corsair Arsonist" RaidOnTheGreyHavens 3 28 2 2 1 3)
    { cdCardTraits = setFromList [Corsair]
    , cdWhenRevealed = True
    }

sahirsRavager :: CardDef
sahirsRavager = (enemy "11041" "Sahír's Ravager" RaidOnTheGreyHavens 3 36 3 4 3 5) {cdCardTraits = setFromList [Corsair, Raider], cdEasyModeQuantity = Just 1}

-- * Flight of the Stormcaller

-- | Never in the encounter deck: the first stage's setup puts it in the Stormcaller's area.
stormcaller :: CardDef
stormcaller =
  unique
    (enemy "12019" "Stormcaller" FlightOfTheStormcaller 1 50 2 6 6 18)
      { cdCardTraits = setFromList [Corsair, Ship]
      , cdKeywords = setFromList [Boarding 2]
      , cdVictoryPoints = Just 10
      }

-- | The Stormcaller's Campaign Mode only side (Dream-chaser Campaign Expansion 42), which
-- the Dream-chaser campaign plays in its place.
stormcallerCampaign :: CardDef
stormcallerCampaign =
  unique
    (enemy "DCC042" "Stormcaller" DreamChaserCampaign 0 50 2 7 6 20)
      { cdCardTraits = setFromList [Corsair, Ship]
      , cdKeywords = setFromList [Boarding 2]
      , cdVictoryPoints = Just 15
      }

sahirsEscort :: CardDef
sahirsEscort =
  (enemy "12020" "Sahír's Escort" FlightOfTheStormcaller 3 46 2 4 3 6)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 1]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 2
    }

swiftRaider :: CardDef
swiftRaider =
  (enemy "12021" "Swift Raider" FlightOfTheStormcaller 2 38 3 5 4 8)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 2]
    , cdEasyModeQuantity = Just 1
    }

corsairSkiff :: CardDef
corsairSkiff =
  (enemy "12022" "Corsair Skiff" FlightOfTheStormcaller 2 24 1 2 2 5)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Surge]
    , cdSailingIcons = 1
    }

corsairSeafarer :: CardDef
corsairSeafarer =
  (enemy "12023" "Corsair Seafarer" FlightOfTheStormcaller 2 26 2 2 2 3)
    { cdCardTraits = setFromList [Corsair, Raider]
    }

serpentsBlade :: CardDef
serpentsBlade =
  (enemy "12024" "Serpent's Blade" FlightOfTheStormcaller 2 10 3 3 3 4)
    { cdCardTraits = setFromList [Corsair]
    , cdEasyModeQuantity = Just 1
    }

-- | Immune to player card effects, and cannot be engaged: its module says so.
theThingInTheDepths :: CardDef
theThingInTheDepths =
  unique
    (enemy "12049" "The Thing in the Depths" TheThingInTheDepths 1 50 2 5 0 8)
      { cdCardTraits = setFromList [Creature]
      , cdVictoryPoints = Just 10
      }

eyeOfTheBeast :: CardDef
eyeOfTheBeast =
  (enemy "12050" "Eye of the Beast" TheThingInTheDepths 2 0 2 0 5 5)
    { cdCardTraits = setFromList [Creature, Body]
    , cdKeywords = setFromList [Surge]
    }

thrashingArm :: CardDef
thrashingArm =
  (enemy "12051" "Thrashing Arm" TheThingInTheDepths 2 23 3 5 1 7)
    { cdCardTraits = setFromList [Creature, Tentacle]
    , cdKeywords = setFromList [Grapple LowestPrintedThreat]
    }

crushingArm :: CardDef
crushingArm =
  (enemy "12052" "Crushing Arm" TheThingInTheDepths 3 17 1 3 1 5)
    { cdCardTraits = setFromList [Creature, Tentacle]
    , cdKeywords = setFromList [Surge, Grapple HighestPrintedQuestPoints]
    }

graspingArm :: CardDef
graspingArm =
  (enemy "12053" "Grasping Arm" TheThingInTheDepths 3 20 2 4 1 4)
    { cdCardTraits = setFromList [Creature, Tentacle]
    , cdKeywords = setFromList [Grapple HighestPrintedThreat]
    , cdShadowEffect = True
    }

giantArm :: CardDef
giantArm =
  (enemy "12054" "Giant Arm" TheThingInTheDepths 3 13 1 4 1 6)
    { cdCardTraits = setFromList [Creature, Tentacle]
    , cdKeywords = setFromList [Grapple LowestPrintedQuestPoints]
    , cdShadowEffect = True
    }

seaScorpion :: CardDef
seaScorpion =
  (enemy "12096" "Sea-scorpion" TheDrownedRuins 3 30 2 2 2 4)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

caveEel :: CardDef
caveEel = (enemy "12097" "Cave Eel" TheDrownedRuins 3 20 3 3 4 2) {cdCardTraits = setFromList [Creature], cdEasyModeQuantity = Just 2}

-- | X threat, X the number of players: its card gives itself the rest.
templeGuardian :: CardDef
templeGuardian =
  unique
    (enemy "12070" "Temple Guardian" TempleOfTheDeceived 1 50 0 6 4 10)
      { cdCardTraits = setFromList [Undead]
      , cdVictoryPoints = Just 4
      }

-- | Temple Guardian's Campaign Mode only side (Dream-chaser Campaign Expansion 77), which
-- the Dream-chaser campaign plays in its place. X threat, X defense and archery X, X the
-- number of players: its card gives itself those.
templeGuardianCampaign :: CardDef
templeGuardianCampaign =
  unique
    (enemy "DCC077" "Temple Guardian" DreamChaserCampaign 0 50 0 5 0 10)
      { cdCardTraits = setFromList [Undead]
      , cdVictoryPoints = Just 5
      }

faithlessDesecrator :: CardDef
faithlessDesecrator =
  (enemy "12075" "Faithless Desecrator" TempleOfTheDeceived 3 25 2 2 2 4)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    }

islandWatcher :: CardDef
islandWatcher = (enemy "12076" "Island Watcher" TempleOfTheDeceived 2 46 3 4 1 5) {cdCardTraits = setFromList [Undead], cdEasyModeQuantity = Just 0}

profaneDead :: CardDef
profaneDead = (enemy "12077" "Profane Dead" TempleOfTheDeceived 2 35 1 2 3 3) {cdCardTraits = setFromList [Undead]}

-- * A Storm on Cobas Haven

raiderFlagship :: CardDef
raiderFlagship =
  (enemy "12124" "Raider Flagship" AStormOnCobasHaven 1 50 3 5 3 18)
    { cdUnique = True
    , cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 1]
    , cdVictoryPoints = Just 20
    }

-- | Its other side is the Ship objective the players may take it as, once destroyed.
corsairSkirmisher :: CardDef
corsairSkirmisher =
  (enemy "12125" "Corsair Skirmisher" AStormOnCobasHaven 2 28 3 4 4 6)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 1]
    , cdOtherSide = Just "12125-ship"
    }

siegeShip :: CardDef
siegeShip =
  (enemy "12126" "Siege Ship" AStormOnCobasHaven 3 46 2 5 5 7)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Archery 2]
    , cdEasyModeQuantity = Just 2
    }

heavyCruiser :: CardDef
heavyCruiser =
  (enemy "12127" "Heavy Cruiser" AStormOnCobasHaven 1 42 5 7 5 8)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 2]
    , cdSailingIcons = 1
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 0
    }

-- * The City of Corsairs: Coast of Umbar and The City of Corsairs

-- | Not the Stormcaller of The Thing in the Depths: this one is a Ship enemy.
stormcallerTheCityOfCorsairs :: CardDef
stormcallerTheCityOfCorsairs =
  unique
    (enemy "12151" "Stormcaller" CoastOfUmbar 1 20 2 6 6 16)
      { cdCardTraits = setFromList [Corsair, Ship]
      , cdKeywords = setFromList [Boarding 1]
      , cdVictoryPoints = Just 10
      }

slaveShip :: CardDef
slaveShip =
  (enemy "12152" "Slave Ship" CoastOfUmbar 2 32 3 7 5 5)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 1]
    , cdEasyModeQuantity = Just 1
    }

umbarSlaver :: CardDef
umbarSlaver = (enemy "12153" "Umbar Slaver" CoastOfUmbar 3 28 2 2 3 5) {cdCardTraits = setFromList [Corsair, Raider], cdEasyModeQuantity = Just 1}

-- | Not The Stormcaller Elite's Captain Sahír: this one is single-sided, and shuffled in
-- with The City of Corsairs.
captainSahirTheCityOfCorsairs :: CardDef
captainSahirTheCityOfCorsairs =
  unique (enemy "12158" "Captain Sahír" TheCityOfCorsairs 1 4 6 6 2 8) {cdCardTraits = setFromList [Corsair, Raider, Corrupt]}

-- Dream-chaser Nightmare Decks

raiderOfTheStorm :: CardDef
raiderOfTheStorm =
  (enemy "flight-of-the-stormcaller-nightmare-flight-of-the-stormcaller-002" "Raider of the Storm" FlightOfTheStormcallerNightmare 4 28 0 3 3 4)
    { cdCardTraits = setFromList [Corsair]
    }

tideRunner :: CardDef
tideRunner =
  (enemy "flight-of-the-stormcaller-nightmare-flight-of-the-stormcaller-003" "Tide-runner" FlightOfTheStormcallerNightmare 2 44 4 4 2 9)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 1]
    , cdWhenRevealed = True
    }

venomsBite :: CardDef
venomsBite =
  unique
    (enemy "flight-of-the-stormcaller-nightmare-flight-of-the-stormcaller-004" "Venom's Bite" FlightOfTheStormcallerNightmare 1 34 5 8 5 14)
      { cdCardTraits = setFromList [Corsair, Ship]
      , cdKeywords = setFromList [Boarding 2]
      , cdVictoryPoints = Just 5
      , cdSailingIcons = 1
      }

corsairDuelist :: CardDef
corsairDuelist =
  (enemy "the-thing-in-the-depths-nightmare-the-thing-in-the-depths-005" "Corsair Duelist" TheThingInTheDepthsNightmare 3 36 3 3 1 3)
    { cdCardTraits = setFromList [Corsair, Raider]
    , cdShadowEffect = True
    }

slitheringArm :: CardDef
slitheringArm =
  (enemy "the-thing-in-the-depths-nightmare-the-thing-in-the-depths-006" "Slithering Arm" TheThingInTheDepthsNightmare 3 0 0 4 1 5)
    { cdCardTraits = setFromList [Creature, Tentacle]
    , cdKeywords = setFromList [Surge, Grapple MostEnemiesGrappled]
    , cdShadowEffect = True
    }

crashingArm :: CardDef
crashingArm =
  (enemy "the-thing-in-the-depths-nightmare-the-thing-in-the-depths-007" "Crashing Arm" TheThingInTheDepthsNightmare 3 27 2 6 1 7)
    { cdCardTraits = setFromList [Creature, Tentacle]
    , cdKeywords = setFromList [Grapple FewestEnemiesGrappled]
    , cdShadowEffect = True
    }

kingsKnight :: CardDef
kingsKnight =
  (enemy "temple-of-the-deceived-nightmare-temple-of-the-deceived-005" "King's Knight" TempleOfTheDeceivedNightmare 2 23 4 4 4 6)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    }

cursedSoldier :: CardDef
cursedSoldier =
  (enemy "temple-of-the-deceived-nightmare-temple-of-the-deceived-006" "Cursed Soldier" TempleOfTheDeceivedNightmare 3 28 3 3 3 4)
    { cdCardTraits = setFromList [Undead]
    }

submergedDead :: CardDef
submergedDead =
  (enemy "the-drowned-ruins-nightmare-the-drowned-ruins-006" "Submerged Dead" TheDrownedRuinsNightmare 4 26 2 3 2 3)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

greatShark :: CardDef
greatShark =
  (enemy "the-drowned-ruins-nightmare-the-drowned-ruins-007" "Great Shark" TheDrownedRuinsNightmare 2 34 4 4 4 6)
    { cdCardTraits = setFromList [Creature]
    }

underseaCaveTroll :: CardDef
underseaCaveTroll =
  (enemy "the-drowned-ruins-nightmare-the-drowned-ruins-008" "Undersea Cave Troll" TheDrownedRuinsNightmare 2 36 3 7 1 10)
    { cdCardTraits = setFromList [Troll]
    , cdVictoryPoints = Just 1
    }

rammingShip :: CardDef
rammingShip =
  (enemy "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-004" "Ramming Ship" AStormOnCobasHavenNightmare 2 28 4 4 3 8)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdShadowEffect = True
    }

fangOfSahir :: CardDef
fangOfSahir =
  (enemy "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-005" "Fang of Sahír" AStormOnCobasHavenNightmare 3 42 2 5 3 3)
    { cdCardTraits = setFromList [Corsair]
    }

patrolShip :: CardDef
patrolShip =
  (enemy "the-city-of-corsairs-nightmare-the-city-of-corsairs-002" "Patrol Ship" CoastOfUmbarNightmare 2 41 2 5 2 7)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 2]
    , cdSailingIcons = 1
    }

corsairLoyalist :: CardDef
corsairLoyalist =
  (enemy "the-city-of-corsairs-nightmare-the-city-of-corsairs-003" "Corsair Loyalist" CoastOfUmbarNightmare 2 28 4 4 2 4)
    { cdCardTraits = setFromList [Corsair, Raider]
    , cdShadowEffect = True
    }

umbarPatrolNightmare :: CardDef
umbarPatrolNightmare =
  (enemy "the-city-of-corsairs-nightmare-the-city-of-corsairs-008" "Umbar Patrol" TheCityOfCorsairsNightmare 3 48 3 3 4 6)
    { cdCardTraits = setFromList [Corsair]
    , cdShadowEffect = True
    }

-- Core Set Nightmare Decks

ungoliantsSpawnNightmare :: CardDef
ungoliantsSpawnNightmare =
  (enemy "core-set-nightmare-passage-through-mirkwood-002" "Ungoliant's Spawn" PassageThroughMirkwoodNightmare 1 32 3 5 3 9)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

ungoliantsBrood :: CardDef
ungoliantsBrood =
  (enemy "core-set-nightmare-passage-through-mirkwood-003" "Ungoliant's Brood" PassageThroughMirkwoodNightmare 3 31 2 3 2 5)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdShadowEffect = True
    }

forestFlies :: CardDef
forestFlies =
  (enemy "core-set-nightmare-passage-through-mirkwood-004" "Forest Flies" PassageThroughMirkwoodNightmare 3 27 4 1 0 3)
    { cdCardTraits = setFromList [Creature, Insect]
    , cdShadowEffect = True
    }

spidersOfMirkwood :: CardDef
spidersOfMirkwood =
  (enemy "core-set-nightmare-passage-through-mirkwood-005" "Spiders of Mirkwood" PassageThroughMirkwoodNightmare 2 18 3 2 2 4)
    { cdCardTraits = setFromList [Spider]
    , cdShadowEffect = True
    }

marshlandOutlaws :: CardDef
marshlandOutlaws =
  (enemy "core-set-nightmare-journey-along-the-anduin-002" "Marshland Outlaws" JourneyAlongTheAnduinNightmare 2 21 2 2 2 8)
    { cdCardTraits = setFromList [Marshland]
    , cdShadowEffect = True
    }

anduinTrollSpawn :: CardDef
anduinTrollSpawn =
  (enemy "core-set-nightmare-journey-along-the-anduin-003" "Anduin Troll Spawn" JourneyAlongTheAnduinNightmare 3 26 2 4 3 5)
    { cdCardTraits = setFromList [Troll]
    }

brownWaterRats :: CardDef
brownWaterRats =
  (enemy "core-set-nightmare-journey-along-the-anduin-004" "Brown Water Rats" JourneyAlongTheAnduinNightmare 3 1 1 1 1 1)
    { cdCardTraits = setFromList [Creature, Rat]
    , cdShadowEffect = True
    }

tortureMaster :: CardDef
tortureMaster =
  (enemy "core-set-nightmare-escape-from-dol-guldur-002" "Torture Master" EscapeFromDolGuldurNightmare 3 45 5 1 2 4)
    { cdCardTraits = setFromList [DolGuldur, Orc]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

crazedCaptive :: CardDef
crazedCaptive =
  (enemy "core-set-nightmare-escape-from-dol-guldur-003" "Crazed Captive" EscapeFromDolGuldurNightmare 2 13 2 1 3 1)
    { cdCardTraits = setFromList [Captive]
    , cdShadowEffect = True
    , cdKeywords = setFromList [Doomed 1]
    }

spiderOfDolGuldur :: CardDef
spiderOfDolGuldur =
  (enemy "core-set-nightmare-escape-from-dol-guldur-004" "Spider of Dol Guldur" EscapeFromDolGuldurNightmare 3 28 3 3 3 6)
    { cdCardTraits = setFromList [Creature, Spider, DolGuldur]
    , cdShadowEffect = True
    }

-- * The Dream-chaser Campaign Expansion: burdens

thunderstruck :: CardDef
thunderstruck =
  unique
    (enemy "DCC177" "Thunderstruck" DreamChaserCampaign 1 46 3 6 5 4)
      { cdCardTraits = setFromList [Corsair, Ship, Elite, Burden]
      , cdKeywords = setFromList [Archery 1, Boarding 2]
      , cdVictoryPoints = Just 7
      , cdShadowEffect = True
      }

-- | Shuffled into the Corsair deck.
crowley :: CardDef
crowley = unique (enemy "DCC178" "Crowley" DreamChaserCampaign 1 35 2 2 2 2) {cdCardTraits = setFromList [Corsair, Raider, Burden]}

revenant :: CardDef
revenant = unique (enemy "DCC181" "Revenant" DreamChaserCampaign 1 43 3 3 3 3) {cdCardTraits = setFromList [Undead, Burden]}

graxar :: CardDef
graxar =
  unique (enemy "DCC188" "Graxar" DreamChaserCampaign 1 47 3 3 0 10)
    { cdCardTraits = setFromList [Corsair, Raider, Burden]
    , cdVictoryPoints = Just 8
    }

-- | Its Menacing side, X threat and defense the number of players: its card gives itself
-- those.
seaSerpent :: CardDef
seaSerpent =
  unique (enemy "DCC196" "Sea Serpent" DreamChaserCampaign 0 40 0 6 0 6)
    { cdCardTraits = setFromList [Creature, Burden]
    , cdOtherSide = Just "DCC196B"
    }

seaSerpentEnraged :: CardDef
seaSerpentEnraged =
  unique (enemy "DCC196B" "Sea Serpent" DreamChaserCampaign 0 0 0 8 0 8)
    { cdCardTraits = setFromList [Creature, Burden]
    , cdVictoryPoints = Just 8
    , cdOtherSide = Just "DCC196"
    }

-- | Defense X, the number of players.
recurringNightmare :: CardDef
recurringNightmare = (enemy "DCC199" "Recurring Nightmare" DreamChaserCampaign 0 35 3 5 0 6) {cdCardTraits = setFromList [Undead, Burden]}
-- Heirs of Númenor Nightmare Decks

cunningInformant :: CardDef
cunningInformant =
  (enemy "heirs-of-numenor-nightmare-peril-in-pelargir-002" "Cunning Informant" PerilInPelargirNightmare 3 21 3 2 1 3) {cdCardTraits = setFromList [Brigand]}

harborBrute :: CardDef
harborBrute =
  (enemy "heirs-of-numenor-nightmare-peril-in-pelargir-003" "Harbor Brute" PerilInPelargirNightmare 2 38 4 4 4 5)
    { cdCardTraits = setFromList [Brigand, Thug]
    , cdWhenRevealed = True
    }

pelargirRingleader :: CardDef
pelargirRingleader =
  (enemy "heirs-of-numenor-nightmare-peril-in-pelargir-004" "Pelargir Ringleader" PerilInPelargirNightmare 2 35 2 3 3 4)
    { cdCardTraits = setFromList [Brigand]
    , cdShadowEffect = True
    }

-- | Archery X, X the number of players: gained from its own modifiers.
mumakElite :: CardDef
mumakElite =
  (enemy "heirs-of-numenor-nightmare-into-ithilien-002" "Mumak Elite" IntoIthilienNightmare 2 38 5 7 5 12) {cdCardTraits = setFromList [Harad, Creature]}

haradrimMarksman :: CardDef
haradrimMarksman =
  (enemy "heirs-of-numenor-nightmare-into-ithilien-003" "Haradrim Marksman" IntoIthilienNightmare 3 36 2 2 2 5) {cdCardTraits = setFromList [Harad]}

haradrimCaptain :: CardDef
haradrimCaptain =
  (enemy "heirs-of-numenor-nightmare-into-ithilien-004" "Haradrim Captain" IntoIthilienNightmare 2 44 4 4 3 6)
    { cdCardTraits = setFromList [Harad]
    , cdShadowEffect = True
    }

southronMountedArcher :: CardDef
southronMountedArcher =
  (enemy "heirs-of-numenor-nightmare-the-siege-of-cair-andros-002" "Southron Mounted Archer" TheSiegeOfCairAndrosNightmare 3 45 3 4 2 4)
    { cdCardTraits = setFromList [Harad]
    , cdKeywords = setFromList [Archery 5]
    }

orcSaboteur :: CardDef
orcSaboteur =
  (enemy "heirs-of-numenor-nightmare-the-siege-of-cair-andros-003" "Orc Saboteur" TheSiegeOfCairAndrosNightmare 3 21 2 5 2 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

-- The Voice of Isengard Nightmare Decks

dunlandAssassin :: CardDef
dunlandAssassin =
  (enemy "the-voice-of-isengard-nightmare-the-fords-of-isen-002" "Dunland Assassin" TheFordsOfIsenNightmare 2 45 3 3 3 5)
    { cdCardTraits = setFromList [Dunland]
    }

vengefulHillman :: CardDef
vengefulHillman =
  (enemy "the-voice-of-isengard-nightmare-the-fords-of-isen-003" "Vengeful Hillman" TheFordsOfIsenNightmare 3 27 2 4 2 4)
    { cdCardTraits = setFromList [Dunland]
    }

savageHillman :: CardDef
savageHillman =
  (enemy "the-voice-of-isengard-nightmare-the-fords-of-isen-004" "Savage Hillman" TheFordsOfIsenNightmare 3 20 1 2 1 3)
    { cdCardTraits = setFromList [Dunland]
    , cdKeywords = setFromList [Surge]
    }

vigilantOrc :: CardDef
vigilantOrc =
  (enemy "the-voice-of-isengard-nightmare-to-catch-an-orc-002" "Vigilant Orc" ToCatchAnOrcNightmare 4 1 2 4 3 4)
    { cdCardTraits = setFromList [Orc]
    }

methedrasLookout :: CardDef
methedrasLookout =
  (enemy "the-voice-of-isengard-nightmare-to-catch-an-orc-003" "Methedras Lookout" ToCatchAnOrcNightmare 3 44 3 5 4 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

-- The Lost Realm Nightmare Decks

orcAbductor :: CardDef
orcAbductor =
  (enemy "the-lost-realm-nightmare-invaders-in-chetwood-002" "Orc Abductor" IntrudersInChetwoodNightmare 3 45 2 4 4 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

wolfpeltGoblin :: CardDef
wolfpeltGoblin =
  (enemy "the-redhorn-gate-nightmare-the-redhorn-gate-002" "Wolfpelt Goblin" TheRedhornGateNightmare 3 20 2 3 2 4)
    { cdCardTraits = setFromList [Goblin, Orc, Snow]
    , cdKeywords = setFromList [Surge]
    }

silvertineMarauder :: CardDef
silvertineMarauder =
  (enemy "the-redhorn-gate-nightmare-the-redhorn-gate-003" "Silvertine Marauder" TheRedhornGateNightmare 4 28 2 3 3 6)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

orcRavagerNightmare :: CardDef
orcRavagerNightmare =
  (enemy "heirs-of-numenor-nightmare-the-siege-of-cair-andros-004" "Orc Ravager" TheSiegeOfCairAndrosNightmare 4 30 3 3 1 4)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

guardWarg :: CardDef
guardWarg =
  (enemy "the-voice-of-isengard-nightmare-to-catch-an-orc-004" "Guard Warg" ToCatchAnOrcNightmare 2 25 4 3 3 3)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

-- The Ring-maker Nightmare Decks

boarClanAmbusher :: CardDef
boarClanAmbusher =
  (enemy "the-dunland-trap-nightmare-the-dunland-trap-002" "Boar Clan Ambusher" TheDunlandTrapNightmare 3 30 3 5 2 5)
    { cdCardTraits = setFromList [Dunland, BoarClan]
    , cdShadowEffect = True
    }

boarClanSkirmisher :: CardDef
boarClanSkirmisher =
  (enemy "the-dunland-trap-nightmare-the-dunland-trap-003" "Boar Clan Skirmisher" TheDunlandTrapNightmare 4 20 2 3 2 4)
    { cdCardTraits = setFromList [Dunland, BoarClan]
    , cdShadowEffect = True
    }

wolfSpirit :: CardDef
wolfSpirit =
  (enemy "the-three-trials-nightmare-the-three-trials-002" "Wolf Spirit" TheThreeTrialsNightmare 3 30 2 5 1 6)
    { cdCardTraits = setFromList [Undead, SpiritTrait, Wolf]
    }

boarSpirit :: CardDef
boarSpirit =
  (enemy "the-three-trials-nightmare-the-three-trials-003" "Boar Spirit" TheThreeTrialsNightmare 3 20 1 4 2 5)
    { cdCardTraits = setFromList [Undead, SpiritTrait, Boar]
    }

ravenSpirit :: CardDef
ravenSpirit =
  (enemy "the-three-trials-nightmare-the-three-trials-004" "Raven Spirit" TheThreeTrialsNightmare 3 40 3 3 3 4)
    { cdCardTraits = setFromList [Undead, SpiritTrait, Raven]
    }

bellachsHunter :: CardDef
bellachsHunter =
  (enemy "trouble-in-tharbad-nightmare-trouble-in-tharbad-002" "Bellach's Hunter" TroubleInTharbadNightmare 2 50 3 6 4 6)
    { cdCardTraits = setFromList [Orc, Spy]
    }

orcSeeker :: CardDef
orcSeeker =
  (enemy "trouble-in-tharbad-nightmare-trouble-in-tharbad-003" "Orc Seeker" TroubleInTharbadNightmare 3 20 2 4 3 4)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

bloodHound :: CardDef
bloodHound =
  (enemy "trouble-in-tharbad-nightmare-trouble-in-tharbad-004" "Blood Hound" TroubleInTharbadNightmare 3 0 2 2 2 3)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

huornOfFangorn :: CardDef
huornOfFangorn =
  (enemy "the-voice-of-isengard-nightmare-into-fangorn-002" "Huorn of Fangorn" IntoFangornNightmare 3 23 3 4 3 6)
    { cdCardTraits = setFromList [Huorn]
    , cdKeywords = setFromList [Hinder]
    , cdShadowEffect = True
    }

blockingHuorn :: CardDef
blockingHuorn =
  (enemy "the-voice-of-isengard-nightmare-into-fangorn-003" "Blocking Huorn" IntoFangornNightmare 2 28 2 3 3 8)
    { cdCardTraits = setFromList [Huorn]
    , cdKeywords = setFromList [Hinder]
    , cdShadowEffect = True
    }

wickedHuorn :: CardDef
wickedHuorn =
  (enemy "the-voice-of-isengard-nightmare-into-fangorn-004" "Wicked Huorn" IntoFangornNightmare 2 33 3 6 4 7)
    { cdCardTraits = setFromList [Huorn]
    , cdKeywords = setFromList [Hinder]
    , cdShadowEffect = True
    }

-- The Grey Havens Nightmare Decks

brashCorsair :: CardDef
brashCorsair =
  (enemy "the-grey-havens-nightmare-voyage-across-belegaer-004" "Brash Corsair" VoyageAcrossBelegaerNightmare 3 16 3 3 4 3)
    { cdCardTraits = setFromList [Corsair]
    }

sahirsPursuer :: CardDef
sahirsPursuer =
  (enemy "the-grey-havens-nightmare-voyage-across-belegaer-005" "Sahír's Pursuer" VoyageAcrossBelegaerNightmare 2 40 3 7 3 8)
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdKeywords = setFromList [Boarding 1]
    }

guardianOfTheGoldenKing :: CardDef
guardianOfTheGoldenKing =
  (enemy "the-grey-havens-nightmare-the-fate-of-numenor-009" "Guardian of the Golden King" TheFateOfNumenorNightmare 2 41 4 6 4 10)
    { cdCardTraits = setFromList [Undead]
    }

corsairPillager :: CardDef
corsairPillager =
  (enemy "the-grey-havens-nightmare-raid-on-the-grey-havens-003" "Corsair Pillager" RaidOnTheGreyHavensNightmare 3 16 3 2 3 3)
    { cdCardTraits = setFromList [Corsair, Raider]
    }

corsairPyromaniac :: CardDef
corsairPyromaniac =
  (enemy "the-grey-havens-nightmare-raid-on-the-grey-havens-004" "Corsair Pyromaniac" RaidOnTheGreyHavensNightmare 3 44 2 2 2 4)
    { cdCardTraits = setFromList [Corsair, Raider]
    }

eliteMarauder :: CardDef
eliteMarauder =
  (enemy "the-grey-havens-nightmare-raid-on-the-grey-havens-005" "Elite Marauder" RaidOnTheGreyHavensNightmare 1 32 4 6 2 5)
    { cdCardTraits = setFromList [Corsair]
    }

soldierOfAngmar :: CardDef
soldierOfAngmar =
  (enemy "the-lost-realm-nightmare-invaders-in-chetwood-003" "Soldier of Angmar" IntrudersInChetwoodNightmare 2 28 2 5 3 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

rearguardAmbusher :: CardDef
rearguardAmbusher =
  (enemy "the-lost-realm-nightmare-the-weather-hills-013" "Rearguard Ambusher" TheWeatherHillsNightmare 3 17 1 4 2 4)
    { cdCardTraits = setFromList [Orc]
    }

angmarRaider :: CardDef
angmarRaider =
  (enemy "the-lost-realm-nightmare-the-weather-hills-014" "Angmar Raider" TheWeatherHillsNightmare 4 44 3 5 3 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

undeadHorde :: CardDef
undeadHorde =
  (enemy "the-lost-realm-nightmare-deadmens-dike-020" "Undead Horde" DeadmensDikeNightmare 2 34 4 6 3 8)
    { cdCardTraits = setFromList [Undead]
    }

cursedDeadNightmare :: CardDef
cursedDeadNightmare =
  (enemy "the-lost-realm-nightmare-deadmens-dike-021" "Cursed Dead" DeadmensDikeNightmare 2 27 1 3 1 2)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

-- Khazad-dûm Nightmare Decks

patrolSentry :: CardDef
patrolSentry =
  (enemy "khazad-dum-nightmare-into-the-pit-003" "Patrol Sentry" IntoThePitNightmare 4 20 1 4 3 5)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

goblinLurker :: CardDef
goblinLurker =
  (enemy "khazad-dum-nightmare-into-the-pit-004" "Goblin Lurker" IntoThePitNightmare 3 50 3 2 2 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

shadowDweller :: CardDef
shadowDweller =
  (enemy "khazad-dum-nightmare-into-the-pit-005" "Shadow Dweller" IntoThePitNightmare 2 0 1 7 7 8)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

overseerMaurul :: CardDef
overseerMaurul =
  (enemy "khazad-dum-nightmare-the-seventh-level-002" "Overseer Maurûl" TheSeventhLevelNightmare 1 20 3 3 3 12)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdUnique = True
    , cdVictoryPoints = Just 5
    }

orcTaskmaster :: CardDef
orcTaskmaster =
  (enemy "khazad-dum-nightmare-the-seventh-level-004" "Orc Taskmaster" TheSeventhLevelNightmare 2 44 1 4 4 6)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdWhenRevealed = True
    }

goblinSkirmisher :: CardDef
goblinSkirmisher =
  (enemy "khazad-dum-nightmare-the-seventh-level-005" "Goblin Skirmisher" TheSeventhLevelNightmare 3 15 2 4 2 3)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

pitGoblin :: CardDef
pitGoblin =
  (enemy "khazad-dum-nightmare-the-seventh-level-006" "Pit Goblin" TheSeventhLevelNightmare 2 36 3 2 1 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

swarmingGoblins :: CardDef
swarmingGoblins =
  (enemy "khazad-dum-nightmare-flight-from-moria-005" "Swarming Goblins" FlightFromMoriaNightmare 3 10 1 3 1 6)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdKeywords = setFromList [Surge]
    }

-- | X is the number of victory points in the victory display.
coalBlackOrc :: CardDef
coalBlackOrc =
  (enemy "khazad-dum-nightmare-flight-from-moria-006" "Coal-Black Orc" FlightFromMoriaNightmare 3 34 0 0 3 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

deadlyMarshAdder :: CardDef
deadlyMarshAdder =
  (enemy "the-nin-in-eilph-nightmare-the-nin-in-eilph-002" "Deadly Marsh Adder" TheNinInEilphNightmare 3 30 3 4 2 5)
    { cdCardTraits = setFromList [Creature]
    }

swarmsOfMosquitos :: CardDef
swarmsOfMosquitos =
  (enemy "the-nin-in-eilph-nightmare-the-nin-in-eilph-003" "Swarms of Mosquitoes" TheNinInEilphNightmare 3 25 2 3 1 7)
    { cdCardTraits = setFromList [Creature, Insect]
    , cdVictoryPoints = Just 1
    }

bellachsBodyguard :: CardDef
bellachsBodyguard =
  (enemy "celebrimbors-secret-nightmare-celebrimbors-secret-002" "Bellach's Bodyguard" CelebrimborsSecretNightmare 3 45 4 4 3 6)
    { cdCardTraits = setFromList [Mordor, Orc]
    , cdShadowEffect = True
    , cdKeywords = setFromList [Scour]
    }

littleSnuffler :: CardDef
littleSnuffler =
  (enemy "celebrimbors-secret-nightmare-celebrimbors-secret-003" "Little Snuffler" CelebrimborsSecretNightmare 3 15 3 3 2 3)
    { cdCardTraits = setFromList [Orc, Snaga]
    , cdWhenRevealed = True
    , cdKeywords = setFromList [Scour]
    }

dunlendingHorde :: CardDef
dunlendingHorde =
  (enemy "the-antlered-crown-nightmare-the-antlered-crown-002" "Dunlending Horde" TheAntleredCrownNightmare 2 32 4 6 3 8)
    { cdCardTraits = setFromList [Dunland]
    }

ravenClanElite :: CardDef
ravenClanElite =
  (enemy "the-antlered-crown-nightmare-the-antlered-crown-003" "Raven Clan Elite" TheAntleredCrownNightmare 4 28 2 4 2 4)
    { cdCardTraits = setFromList [Dunland, RavenClan]
    , cdWhenRevealed = True
    }

ravenClawDruid :: CardDef
ravenClawDruid =
  (enemy "the-antlered-crown-nightmare-the-antlered-crown-004" "Raven Claw Druid" TheAntleredCrownNightmare 3 26 3 2 3 5)
    { cdCardTraits = setFromList [Dunland, RavenClan]
    , cdShadowEffect = True
    }

-- Shadows of Mirkwood Nightmare Decks

ravenousHawk :: CardDef
ravenousHawk =
  (enemy "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-002" "Ravenous Hawk" TheHuntForGollumNightmare 3 42 2 3 3 3)
    { cdCardTraits = setFromList [Creature]
    }

huntingWarg :: CardDef
huntingWarg =
  (enemy "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-003" "Hunting Warg" TheHuntForGollumNightmare 2 15 1 3 2 3)
    { cdCardTraits = setFromList [Creature, Mordor]
    }

savageHillTroll :: CardDef
savageHillTroll =
  (enemy "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-002" "Savage Hill-troll" ConflictAtTheCarrockNightmare 4 21 1 3 2 6)
    { cdCardTraits = setFromList [Troll]
    , cdShadowEffect = True
    , cdVictoryPoints = Just 1
    }

robAndBob :: CardDef
robAndBob =
  (unique (enemy "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-003" "Rob & Bob" ConflictAtTheCarrockNightmare 1 34 4 5 4 10))
    { cdCardTraits = setFromList [Troll]
    }

louisNightmare :: CardDef
louisNightmare =
  (unique (enemy "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-004" "Louis" ConflictAtTheCarrockNightmare 1 34 4 4 2 10))
    { cdCardTraits = setFromList [Troll]
    , cdVictoryPoints = Just 2
    }

morrisNightmare :: CardDef
morrisNightmare =
  (unique (enemy "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-005" "Morris" ConflictAtTheCarrockNightmare 1 34 3 5 2 10))
    { cdCardTraits = setFromList [Troll]
    , cdVictoryPoints = Just 2
    }

rupertNightmare :: CardDef
rupertNightmare =
  (unique (enemy "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-006" "Rupert" ConflictAtTheCarrockNightmare 1 34 2 5 3 10))
    { cdCardTraits = setFromList [Troll]
    , cdVictoryPoints = Just 2
    }

stuartNightmare :: CardDef
stuartNightmare =
  (unique (enemy "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-007" "Stuart" ConflictAtTheCarrockNightmare 1 34 2 4 4 10))
    { cdCardTraits = setFromList [Troll]
    , cdVictoryPoints = Just 2
    }

goblinEagleHunter :: CardDef
goblinEagleHunter =
  (enemy "a-journey-to-rhosgobel-nightmare-a-journey-to-rhosgobel-002" "Goblin Eagle-hunter" AJourneyToRhosgobelNightmare 3 48 2 5 3 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    }

treetopBirdEater :: CardDef
treetopBirdEater =
  (enemy "a-journey-to-rhosgobel-nightmare-a-journey-to-rhosgobel-003" "Treetop Bird-eater" AJourneyToRhosgobelNightmare 2 11 2 4 3 5)
    { cdCardTraits = setFromList [Creature, Spider]
    }

tunnelingNamelessThing :: CardDef
tunnelingNamelessThing =
  (enemy "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-002" "Tunneling Nameless Thing" TheHillsOfEmynMuilNightmare 3 50 4 6 4 10)
    { cdCardTraits = setFromList [Nameless]
    }

orcInterceptor :: CardDef
orcInterceptor =
  (enemy "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-003" "Orc Interceptor" TheHillsOfEmynMuilNightmare 2 39 2 2 3 5)
    { cdCardTraits = setFromList [Mordor, Orc]
    }

thingsInThePools :: CardDef
thingsInThePools =
  (enemy "the-dead-marshes-nightmare-the-dead-marshes-004" "Things in the Pools" TheDeadMarshesNightmare 3 50 3 4 2 8)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    }

-- The Wastes of Eriador Nightmare

packHunters :: CardDef
packHunters =
  (enemy "the-wastes-of-eriador-nightmare-the-wastes-of-eriador-002" "Pack Hunters" TheWastesOfEriadorNightmare 5 45 3 2 1 3)
    { cdCardTraits = setFromList [Creature, Warg]
    }

-- Escape from Mount Gram Nightmare

jailorGornakhNightmare :: CardDef
jailorGornakhNightmare =
  unique
    (enemy "escape-from-mount-gram-nightmare-escape-from-mount-gram-002" "Jailor Gornákh" EscapeFromMountGramNightmare 1 45 4 5 5 14)
      { cdCardTraits = setFromList [Goblin, Orc]
      , cdVictoryPoints = Just 5
      }

prisonGuard :: CardDef
prisonGuard =
  (enemy "escape-from-mount-gram-nightmare-escape-from-mount-gram-003" "Prison Guard" EscapeFromMountGramNightmare 3 26 2 2 1 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdKeywords = setFromList [Capture 1]
    }

-- | Of the Angmar Orcs encounter set, as its icon shows: set aside with it.
orcOfThaurdir :: CardDef
orcOfThaurdir =
  (enemy "escape-from-mount-gram-nightmare-escape-from-mount-gram-009" "Orc of Thaurdir" EscapeFromMountGramNightmare 3 35 3 4 3 5)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

-- Across the Ettenmoors Nightmare

hoarwellRiverSerpent :: CardDef
hoarwellRiverSerpent =
  (enemy "across-the-ettenmoors-nightmare-across-the-ettenmoors-003" "Hoarwell River Serpent" AcrossTheEttenmoorsNightmare 2 38 3 4 3 5)
    { cdCardTraits = setFromList [Creature]
    }

scavengingStoneTroll :: CardDef
scavengingStoneTroll =
  (enemy "across-the-ettenmoors-nightmare-across-the-ettenmoors-004" "Scavenging Stone-troll" AcrossTheEttenmoorsNightmare 2 42 1 7 2 14)
    { cdCardTraits = setFromList [Troll]
    }

giantBeetle :: CardDef
giantBeetle =
  (enemy "across-the-ettenmoors-nightmare-across-the-ettenmoors-005" "Giant Beetle" AcrossTheEttenmoorsNightmare 2 14 2 3 3 3)
    { cdCardTraits = setFromList [Creature, Insect]
    }

-- The Treachery of Rhudaur Nightmare

thaurdirsHunter :: CardDef
thaurdirsHunter =
  (enemy "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-002" "Thaurdir's Hunter" TheTreacheryOfRhudaurNightmare 2 16 2 2 1 6)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    }

marshWight :: CardDef
marshWight =
  (enemy "the-dead-marshes-nightmare-the-dead-marshes-005" "Marsh-wight" TheDeadMarshesNightmare 2 40 4 3 2 5)
    { cdCardTraits = setFromList [Undead, Wight]
    }

lostSoulOfLorien :: CardDef
lostSoulOfLorien =
  (enemy "the-dead-marshes-nightmare-the-dead-marshes-006" "Lost Soul of Lórien" TheDeadMarshesNightmare 3 30 1 5 2 3)
    { cdCardTraits = setFromList [SpiritTrait, Undead]
    }

illFatedGuard :: CardDef
illFatedGuard =
  (enemy "the-dead-marshes-nightmare-the-dead-marshes-007" "Ill-fated Guard" TheDeadMarshesNightmare 3 18 2 2 3 7)
    { cdCardTraits = setFromList [SpiritTrait, Undead]
    , cdShadowEffect = True
    }

ambushingOrcs :: CardDef
ambushingOrcs =
  (enemy "return-to-mirkwood-nightmare-return-to-mirkwood-002" "Ambushing Orcs" ReturnToMirkwoodNightmare 3 28 2 4 3 7)
    { cdCardTraits = setFromList [Mordor, Orc]
    }

pursuingWargRider :: CardDef
pursuingWargRider =
  (enemy "return-to-mirkwood-nightmare-return-to-mirkwood-003" "Pursuing Warg-rider" ReturnToMirkwoodNightmare 2 48 3 4 2 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    }

webSpinner :: CardDef
webSpinner =
  (enemy "return-to-mirkwood-nightmare-return-to-mirkwood-004" "Web-spinner" ReturnToMirkwoodNightmare 3 33 3 3 2 5)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

-- The Morgul Vale Nightmare

warriorOfNurn :: CardDef
warriorOfNurn =
  (enemy "the-morgul-vale-nightmare-the-morgul-vale-002" "Warrior of Nurn" TheMorgulValeNightmare 3 18 2 4 3 5)
    { cdCardTraits = setFromList [Traitor]
    , cdShadowEffect = True
    }

orcCaptor :: CardDef
orcCaptor =
  (enemy "the-morgul-vale-nightmare-the-morgul-vale-003" "Orc Captor" TheMorgulValeNightmare 3 50 4 3 3 4)
    { cdCardTraits = setFromList [Orc, Mordor]
    , cdShadowEffect = True
    }

-- | X threat and X attack, X the progress on To the Tower.
sentryOfTheWhiteBridge :: CardDef
sentryOfTheWhiteBridge =
  (enemy "the-morgul-vale-nightmare-the-morgul-vale-004" "Sentry of the White Bridge" TheMorgulValeNightmare 2 37 0 0 4 5)
    { cdCardTraits = setFromList [Orc, Mordor]
    , cdShadowEffect = True
    }

-- The Blood of Gondor Nightmare

-- | X threat, X the total number of hidden cards in the game.
blackNumenoreanNightmare :: CardDef
blackNumenoreanNightmare =
  (enemy "the-blood-of-gondor-nightmare-the-blood-of-gondor-002" "Black Númenórean" TheBloodOfGondorNightmare 1 35 0 5 4 9)
    { cdCardTraits = setFromList [Mordor]
    , cdVictoryPoints = Just 3
    }

orcTrickster :: CardDef
orcTrickster =
  (enemy "the-blood-of-gondor-nightmare-the-blood-of-gondor-003" "Orc Trickster" TheBloodOfGondorNightmare 4 20 3 3 2 2)
    { cdCardTraits = setFromList [Orc]
    }

urukMarksman :: CardDef
urukMarksman =
  (enemy "the-blood-of-gondor-nightmare-the-blood-of-gondor-004" "Uruk Marksman" TheBloodOfGondorNightmare 3 40 4 2 2 4)
    { cdCardTraits = setFromList [Orc, Uruk, Mordor]
    , cdKeywords = setFromList [Archery 3]
    }

-- The Drúadan Forest Nightmare

druBuriDruNightmare :: CardDef
druBuriDruNightmare =
  (enemy "the-druadan-forest-nightmare-the-druadan-forest-002" "Drû-buri-Drû" TheDruadanForestNightmare 1 1 4 5 4 3)
    { cdCardTraits = setFromList [Wose]
    , cdUnique = True
    }

druadanWarrior :: CardDef
druadanWarrior =
  (enemy "the-druadan-forest-nightmare-the-druadan-forest-003" "Drúadan Warrior" TheDruadanForestNightmare 3 20 2 5 3 5)
    { cdCardTraits = setFromList [Wose]
    , cdKeywords = setFromList [Prowl 2]
    }

druadanStalker :: CardDef
druadanStalker =
  (enemy "the-druadan-forest-nightmare-the-druadan-forest-004" "Drúadan Stalker" TheDruadanForestNightmare 3 43 4 3 2 3)
    { cdCardTraits = setFromList [Wose]
    , cdKeywords = setFromList [Archery 3]
    , cdShadowEffect = True
    }

-- Encounter at Amon Dîn Nightmare

orcRaidersNightmare :: CardDef
orcRaidersNightmare =
  (enemy "encounter-at-amon-din-nightmare-encounter-at-amon-din-002" "Orc Raiders" EncounterAtAmonDinNightmare 2 32 4 5 2 6)
    { cdCardTraits = setFromList [Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

pillagingOrc :: CardDef
pillagingOrc =
  (enemy "encounter-at-amon-din-nightmare-encounter-at-amon-din-003" "Pillaging Orc" EncounterAtAmonDinNightmare 3 34 2 4 3 5)
    { cdCardTraits = setFromList [Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

-- Assault on Osgiliath Nightmare

invadingOrc :: CardDef
invadingOrc =
  (enemy "assault-on-osgiliath-nightmare-assault-on-osgiliath-002" "Invading Orc" AssaultOnOsgiliathNightmare 3 20 2 5 3 3)
    { cdCardTraits = setFromList [Orc, Mordor]
    , cdShadowEffect = True
    }

-- | X threat and archery X, X the number of locations the first player controls.
haradrimWarrior :: CardDef
haradrimWarrior =
  (enemy "assault-on-osgiliath-nightmare-assault-on-osgiliath-003" "Haradrim Warrior" AssaultOnOsgiliathNightmare 3 30 0 3 3 4)
    { cdCardTraits = setFromList [Harad]
    , cdShadowEffect = True
    }

-- The Steward's Fear Nightmare

murderousTurncoat :: CardDef
murderousTurncoat =
  (enemy "the-stewards-fear-nightmare-the-stewards-fear-004" "Murderous Turncoat" TheStewardsFearNightmare 3 20 1 5 3 4)
    { cdCardTraits = setFromList [Brigand]
    , cdWhenRevealed = True
    }

faithlessConspirator :: CardDef
faithlessConspirator =
  (enemy "the-stewards-fear-nightmare-the-stewards-fear-005" "Faithless Conspirator" TheStewardsFearNightmare 3 40 4 4 2 4)
    { cdCardTraits = setFromList [Brigand]
    , cdShadowEffect = True
    }

snowGiant :: CardDef
snowGiant =
  (enemy "the-redhorn-gate-nightmare-the-redhorn-gate-004" "Snow-Giant" TheRedhornGateNightmare 2 42 4 6 4 10)
    { cdCardTraits = setFromList [Giant, Snow]
    , cdVictoryPoints = Just 1
    }

urukElfStalker :: CardDef
urukElfStalker =
  (enemy "road-to-rivendell-nightmare-road-to-rivendell-003" "Uruk Elf-stalker" RoadToRivendellNightmare 3 5 3 5 3 5)
    { cdCardTraits = setFromList [Uruk, Orc]
    , cdKeywords = setFromList [Ambush]
    , cdShadowEffect = True
    }

goblinTrapper :: CardDef
goblinTrapper =
  (enemy "road-to-rivendell-nightmare-road-to-rivendell-004" "Goblin Trapper" RoadToRivendellNightmare 3 30 3 3 2 3)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdKeywords = setFromList [Ambush]
    , cdShadowEffect = True
    }

raidingParty :: CardDef
raidingParty =
  (enemy "road-to-rivendell-nightmare-road-to-rivendell-005" "Raiding Party" RoadToRivendellNightmare 2 34 2 5 1 7)
    { cdCardTraits = setFromList [Mountain, Orc]
    , cdKeywords = setFromList [Ambush]
    }

viciousTrackers :: CardDef
viciousTrackers =
  (enemy "road-to-rivendell-nightmare-road-to-rivendell-006" "Vicious Trackers" RoadToRivendellNightmare 2 16 1 3 2 5)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdKeywords = setFromList [Surge]
    }

theWatcherNightmare :: CardDef
theWatcherNightmare =
  (enemy "the-watcher-in-the-water-nightmare-the-watcher-in-the-water-002" "The Watcher" TheWatcherInTheWaterNightmare 1 50 4 7 7 21)
    { cdUnique = True
    , cdCardTraits = setFromList [Creature, Tentacle]
    , cdVictoryPoints = Just 3
    }

writhingTentacle :: CardDef
writhingTentacle =
  (enemy "the-watcher-in-the-water-nightmare-the-watcher-in-the-water-003" "Writhing Tentacle" TheWatcherInTheWaterNightmare 3 46 4 4 0 3)
    { cdCardTraits = setFromList [Tentacle]
    }

chokingTentacle :: CardDef
chokingTentacle =
  (enemy "the-watcher-in-the-water-nightmare-the-watcher-in-the-water-004" "Choking Tentacle" TheWatcherInTheWaterNightmare 4 10 2 5 1 4)
    { cdCardTraits = setFromList [Tentacle]
    }

goblinStalker :: CardDef
goblinStalker =
  (enemy "the-long-dark-nightmare-the-long-dark-002" "Goblin Stalker" TheLongDarkNightmare 4 28 3 2 2 3)
    { cdCardTraits = setFromList [Goblin, Orc]
    }

trapdoorSpider :: CardDef
trapdoorSpider =
  (enemy "the-long-dark-nightmare-the-long-dark-003" "Trapdoor Spider" TheLongDarkNightmare 3 42 4 5 3 6)
    { cdCardTraits = setFromList [Creature, Spider]
    }

sentinelOfTheDeep :: CardDef
sentinelOfTheDeep =
  (enemy "the-long-dark-nightmare-the-long-dark-004" "Sentinel of the Deep" TheLongDarkNightmare 2 12 2 4 2 5)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdShadowEffect = True
    }

mithrilSeeker :: CardDef
mithrilSeeker =
  (enemy "foundations-of-stone-nightmare-foundations-of-stone-002" "Mithril Seeker" FoundationsOfStoneNightmare 3 18 2 2 2 5)
    { cdCardTraits = setFromList [Goblin, Orc]
    }

giantCaveCentipede :: CardDef
giantCaveCentipede =
  (enemy "foundations-of-stone-nightmare-foundations-of-stone-003" "Giant Cave Centipede" FoundationsOfStoneNightmare 2 28 3 3 5 4)
    { cdCardTraits = setFromList [Creature, Insect]
    , cdWhenRevealed = True
    }

-- | X attack and hit points, 5 with nothing attached: see its card module.
unspeakableThing :: CardDef
unspeakableThing =
  (enemy "foundations-of-stone-nightmare-foundations-of-stone-004" "Unspeakable Thing" FoundationsOfStoneNightmare 3 15 5 0 5 0)
    { cdCardTraits = setFromList [Nameless]
    }

-- | X attack and hit points, 6 with nothing attached: see its card module.
primevalThing :: CardDef
primevalThing =
  (enemy "foundations-of-stone-nightmare-foundations-of-stone-005" "Primeval Thing" FoundationsOfStoneNightmare 2 45 6 0 6 0)
    { cdCardTraits = setFromList [Nameless]
    }

servantOfFlame :: CardDef
servantOfFlame =
  (enemy "shadow-and-flame-nightmare-shadow-and-flame-007" "Servant of Flame" ShadowAndFlameNightmare 3 12 3 3 2 5)
    { cdCardTraits = setFromList [Mordor, Orc, Uruk]
    , cdShadowEffect = True
    }

captainOfMordor :: CardDef
captainOfMordor =
  (enemy "shadow-and-flame-nightmare-shadow-and-flame-008" "Captain of Mordor" ShadowAndFlameNightmare 2 42 4 4 3 7)
    { cdCardTraits = setFromList [Mordor, Orc, Uruk]
    , cdShadowEffect = True
    }

undeadThrall :: CardDef
undeadThrall =
  (enemy "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-003" "Undead Thrall" TheTreacheryOfRhudaurNightmare 2 1 2 2 2 2)
    { cdCardTraits = setFromList [Undead]
    , cdKeywords = setFromList [Indestructible, Surge]
    , cdShadowEffect = True
    }

-- | Of the Quiet the Spirits encounter set, as its icon shows (cards.json has it as a
-- treachery; the card is an enemy).
apostateOfAngmar :: CardDef
apostateOfAngmar =
  (enemy "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-007" "Apostate of Angmar" TheTreacheryOfRhudaurNightmare 3 38 5 2 2 12)
    { cdCardTraits = setFromList [Undead]
    , cdShadowEffect = True
    }

-- The Battle of Carn Dûm Nightmare

orcWight :: CardDef
orcWight =
  (enemy "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-002" "Orc Wight" TheBattleOfCarnDumNightmare 3 30 2 3 2 4)
    { cdCardTraits = setFromList [Orc, Undead]
    , cdShadowEffect = True
    }

thaurdirsElite :: CardDef
thaurdirsElite =
  (enemy "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-003" "Thaurdir's Elite" TheBattleOfCarnDumNightmare 2 45 2 5 4 6)
    { cdCardTraits = setFromList [Orc]
    , cdShadowEffect = True
    }

-- The Dread Realm Nightmare

-- | 10 hit points, as the card shows (cards.json has 1).
silentGuardian :: CardDef
silentGuardian =
  (enemy "the-dread-realm-nightmare-the-dread-realm-002" "Silent Guardian" TheDreadRealmNightmare 2 36 4 2 4 10)
    { cdCardTraits = setFromList [Undead]
    }

necromancerOfCarnDum :: CardDef
necromancerOfCarnDum =
  (enemy "the-dread-realm-nightmare-the-dread-realm-003" "Necromancer of Carn Dûm" TheDreadRealmNightmare 3 28 2 0 3 3)
    { cdCardTraits = setFromList [Sorcerer]
    }

-- * The Sands of Harad: the shared encounter sets (Harad Soldiers, Desert Creatures, Mordor
-- Orcs). Easy mode copy counts from Hall of Beorn's scenario pages.

southronCaptain :: CardDef
southronCaptain = unique (enemy "16041" "Southron Captain" HaradSoldiers 1 40 2 5 3 6) {cdCardTraits = setFromList [Harad]}

southronSoldier :: CardDef
southronSoldier =
  (enemy "16042" "Southron Soldier" HaradSoldiers 3 32 3 3 2 5)
    { cdCardTraits = setFromList [Harad]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

southronArcher :: CardDef
southronArcher =
  (enemy "16043" "Southron Archer" HaradSoldiers 4 36 2 4 3 4)
    { cdCardTraits = setFromList [Harad]
    , cdKeywords = setFromList [Archery 2]
    , cdEasyModeQuantity = Just 3
    }

sandViper :: CardDef
sandViper =
  (enemy "16054" "Sand Viper" DesertCreatures 2 44 4 1 1 3)
    { cdCardTraits = setFromList [Creature]
    , cdEasyModeQuantity = Just 1
    }

giantScorpion :: CardDef
giantScorpion =
  (enemy "16055" "Giant Scorpion" DesertCreatures 3 33 3 3 2 4)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

urukChieftain :: CardDef
urukChieftain =
  unique
    (enemy "16061" "Uruk Chieftain" MordorOrcs 1 50 4 5 3 6)
      { cdCardTraits = setFromList [Orc, Uruk]
      , cdVictoryPoints = Just 3
      }

urukOfMordor :: CardDef
urukOfMordor =
  (enemy "16062" "Uruk of Mordor" MordorOrcs 3 35 2 4 2 4)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdShadowEffect = True
    }

urukWargRider :: CardDef
urukWargRider =
  (enemy "16063" "Uruk Warg-Rider" MordorOrcs 2 40 3 3 1 5)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | "Mordor Warg" as printed, and as the card names itself; cards.json has "Mordor Wargs".
mordorWarg :: CardDef
mordorWarg =
  (enemy "16064" "Mordor Warg" MordorOrcs 2 20 2 2 2 2)
    { cdCardTraits = setFromList [Creature]
    , cdWhenRevealed = True
    }

-- | X attack, defense and threat, X the main quest's stage number: its card gives them.
sauronsEnforcers :: CardDef
sauronsEnforcers =
  (enemy "16035" "Sauron's Enforcers" TheLongArmOfMordor 4 30 0 0 0 3)
    { cdCardTraits = setFromList [Orc, Uruk]
    }
-- * Beneath the Sands

broodMother :: CardDef
broodMother =
  (enemy "17065" "Brood Mother" BeneathTheSands 1 50 4 6 4 12)
    { cdUnique = True
    , cdCardTraits = setFromList [Creature, Spider]
    , cdVictoryPoints = Just 4
    }

-- | On Track printed in its lower right corner.
hiveGuardian :: CardDef
hiveGuardian =
  (enemy "17066" "Hive Guardian" BeneathTheSands 3 40 3 3 2 5)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdShadowEffect = True
    }

-- | On Track printed in its lower right corner.
spiderBroodling :: CardDef
spiderBroodling =
  (enemy "17067" "Spider Broodling" BeneathTheSands 5 20 1 2 1 2)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdShadowEffect = True
    }

-- * Desert Crossing (The Sands of Harad). Easy mode copy counts from Hall of Beorn's
-- scenario page.

-- | X attack and X defense: the tens digit of the temperature, which the card adds.
wereWorm :: CardDef
wereWorm =
  (enemy "16023" "Were-worm" DesertCrossing 4 22 0 0 0 5)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    }

carrionBird :: CardDef
carrionBird =
  (enemy "16024" "Carrion Bird" DesertCrossing 2 40 2 2 1 3)
    { cdCardTraits = setFromList [Creature]
    , cdEasyModeQuantity = Just 1
    }

-- * Escape from Umbar. Easy mode copy counts from Hall of Beorn's scenario page.

umbarSentry :: CardDef
umbarSentry =
  (enemy "16015" "Umbar Sentry" EscapeFromUmbar 5 28 1 2 1 3)
    { cdCardTraits = setFromList [Harad]
    , cdKeywords = setFromList [Surge, Archery 1]
    , cdEasyModeQuantity = Just 4
    }

-- * Race Across Harad

urukPursuer :: CardDef
urukPursuer =
  (enemy "17043" "Uruk Pursuer" RaceAcrossHarad 4 20 2 3 2 3)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

racingWarg :: CardDef
racingWarg =
  (enemy "17044" "Racing Warg" RaceAcrossHarad 3 10 1 1 1 1)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdWhenRevealed = True
    }

-- * The Black Serpent. Easy mode copy counts from Hall of Beorn's scenario page.

-- | The Black Serpent's, not The Crossings of Poros'.
theBlackSerpent :: CardDef
theBlackSerpent = unique (enemy "17094" "The Black Serpent" TheBlackSerpent 1 50 0 2 1 8) {cdCardTraits = setFromList [Harad]}

serpentGuard :: CardDef
serpentGuard =
  (enemy "17095" "Serpent Guard" TheBlackSerpent 4 28 2 3 1 4)
    { cdCardTraits = setFromList [Harad]
    , cdShadowEffect = True
    }

-- The Dungeons of Cirith Gurat (17121-17123). Easy mode copy counts from Hall of Beorn's
-- scenario page.

jailerOfCirithGurat :: CardDef
jailerOfCirithGurat =
  (enemy "17121" "Jailer of Cirith Gurat" TheDungeonsOfCirithGurat 2 45 4 3 2 5)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | The Dungeons of Cirith Gurat's Prison Guard; Escape from Mount Gram's is 'prisonGuard'.
prisonGuardCirithGurat :: CardDef
prisonGuardCirithGurat =
  (enemy "17122" "Prison Guard" TheDungeonsOfCirithGurat 3 30 3 4 1 4)
    { cdCardTraits = setFromList [Orc, Uruk]
    }

dungeonWarg :: CardDef
dungeonWarg =
  (enemy "17123" "Dungeon Warg" TheDungeonsOfCirithGurat 3 25 1 3 1 3)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

-- The Mûmakil

-- | Indestructible. Immune to player card effects. Cannot take more than 3 damage each
-- round.
wildMumak :: CardDef
wildMumak =
  (enemy "17015" "Wild Mûmak" TheMumakil 4 40 3 6 3 9)
    { cdCardTraits = setFromList [Creature, Mumak]
    , cdKeywords = setFromList [Indestructible]
    , cdVictoryPoints = Just 3
    }

haradTiger :: CardDef
haradTiger = (enemy "17016" "Harad Tiger" TheMumakil 1 0 4 4 3 5) {cdCardTraits = setFromList [Creature], cdEasyModeQuantity = Just 0}

territorialApe :: CardDef
territorialApe = (enemy "17017" "Territorial Ape" TheMumakil 4 35 2 2 2 4) {cdCardTraits = setFromList [Creature], cdShadowEffect = True}

stranglingPython :: CardDef
stranglingPython =
  (enemy "17018" "Strangling Python" TheMumakil 2 25 1 3 1 3)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

giantCentipede :: CardDef
giantCentipede = (enemy "17019" "Giant Centipede" TheMumakil 2 30 3 3 1 2) {cdCardTraits = setFromList [Creature], cdShadowEffect = True}

-- * The Crossings of Poros: its two encounter sets, The Crossings of Poros and Mountains of
-- Shadow. Easy mode copy counts from Hall of Beorn's scenario page.

-- | The Crossings of Poros' copy of The Black Serpent, not The Black Serpent scenario's.
theBlackSerpentCrossingsOfPoros :: CardDef
theBlackSerpentCrossingsOfPoros = unique (enemy "17146" "The Black Serpent" TheCrossingsOfPoros 1 50 0 2 1 8) {cdCardTraits = setFromList [Harad]}

southronOutrider :: CardDef
southronOutrider = (enemy "17147" "Southron Outrider" TheCrossingsOfPoros 2 28 2 3 1 4) {cdCardTraits = setFromList [Harad], cdShadowEffect = True}

urukProwler :: CardDef
urukProwler =
  (enemy "17148" "Uruk Prowler" TheCrossingsOfPoros 2 45 3 4 2 5)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

desertWarg :: CardDef
desertWarg =
  (enemy "17149" "Desert Warg" TheCrossingsOfPoros 2 34 2 5 2 3)
    { cdCardTraits = setFromList [Creature]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }

-- * The Wilds of Rhovanion's shared encounter sets

goblinTroop :: CardDef
goblinTroop =
  (enemy "18020" "Goblin Troop" GreyMountainGoblins 2 35 3 5 4 6)
    { cdCardTraits = setFromList [Orc, Goblin]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | Grey Mountain Goblins' Stray Goblin; Shadow and Flame's is 'strayGoblin'.
strayGoblinGreyMountains :: CardDef
strayGoblinGreyMountains =
  (enemy "18021" "Stray Goblin" GreyMountainGoblins 3 50 1 2 0 3)
    { cdCardTraits = setFromList [Orc, Goblin]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

packOfWargs :: CardDef
packOfWargs =
  (enemy "18022" "Pack of Wargs" HillsOfWilderland 2 40 4 4 3 5)
    { cdCardTraits = setFromList [Creature, Warg]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | Printed "Stone-troll", as The Withered Heath's Snow-troll (Hall of Beorn).
stoneTroll :: CardDef
stoneTroll =
  (enemy "18049" "Stone-troll" FellBeasts 2 36 3 6 3 9)
    { cdCardTraits = setFromList [Troll]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

hobgoblin :: CardDef
hobgoblin =
  (enemy "18050" "Hobgoblin" FellBeasts 2 38 1 1 1 5)
    { cdCardTraits = setFromList [Orc]
    , cdEasyModeQuantity = Just 1
    }

werewolf :: CardDef
werewolf = (enemy "18051" "Werewolf" WildCreatures 2 33 3 2 1 8) {cdCardTraits = setFromList [Creature], cdShadowEffect = True, cdEasyModeQuantity = Just 1}

giantSpider :: CardDef
giantSpider = (enemy "18052" "Giant Spider" WildCreatures 3 28 2 3 2 3) {cdCardTraits = setFromList [Creature, Spider], cdShadowEffect = True}

blackBats :: CardDef
blackBats =
  (enemy "18053" "Black Bats" WildCreatures 3 18 1 2 1 2)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

-- * The Fate of Wilderland

urdug :: CardDef
urdug =
  unique
    (enemy "19157" "Urdug" TheFateOfWilderland 1 50 2 3 2 5)
      { cdCardTraits = setFromList [Orc, Goblin]
      , cdVictoryPoints = Just 5
      }

urdugsElite :: CardDef
urdugsElite =
  (enemy "19158" "Urdug's Elite" TheFateOfWilderland 3 25 2 3 2 5)
    { cdCardTraits = setFromList [Orc, Goblin]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

-- * Roam Across Rhovanion

greyMountainGiant :: CardDef
greyMountainGiant = (enemy "19038" "Grey Mountain Giant" RoamAcrossRhovanion 2 44 2 5 2 11) {cdCardTraits = setFromList [Giant], cdEasyModeQuantity = Just 1}

huntingEagle :: CardDef
huntingEagle =
  (enemy "19039" "Hunting Eagle" RoamAcrossRhovanion 3 50 1 3 1 3)
    { cdCardTraits = setFromList [Creature, Eagle]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

fierceVanguard :: CardDef
fierceVanguard =
  (enemy "19159" "Fierce Vanguard" TheFateOfWilderland 3 20 2 2 2 3)
    { cdCardTraits = setFromList [Orc, Goblin]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

wargRider :: CardDef
wargRider = (enemy "19160" "Warg-rider" TheFateOfWilderland 2 30 3 4 1 4) {cdCardTraits = setFromList [Orc, Goblin]}
-- * Fire in the Night

-- | The first player takes it from the encounter deck to the staging area at setup.
dagnir :: CardDef
dagnir =
  unique
    $ (enemy "19074" "Dagnir" FireInTheNight 1 50 0 7 5 20)
      { cdCardTraits = setFromList [Dragon]
      , cdVictoryPoints = Just 5
      }

theDragonsThrall :: CardDef
theDragonsThrall =
  (enemy "19075" "The Dragon's Thrall" FireInTheNight 3 30 3 3 3 4)
    { cdCardTraits = setFromList [Goblin, Orc]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- * The Withered Heath

coldDrake :: CardDef
coldDrake =
  unique
    (enemy "19011" "Cold-Drake" TheWitheredHeath 1 10 1 6 1 18)
      { cdCardTraits = setFromList [Dragon]
      , cdVictoryPoints = Just 6
      }

snowTroll :: CardDef
snowTroll =
  (enemy "19013" "Snow-troll" TheWitheredHeath 2 35 3 5 3 9)
    { cdCardTraits = setFromList [Troll]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- * The Ghost of Framsburg (19094-19111)

framsShade :: CardDef
framsShade =
  unique
    (enemy "19098" "Fram's Shade" TheGhostOfFramsburg 1 30 3 3 3 6)
      { cdCardTraits = setFromList [Undead, Wight, Hazard]
      }

-- | X threat, attack and defense: its card gives itself X.
cryptStalker :: CardDef
cryptStalker =
  (enemy "19099" "Crypt Stalker" TheGhostOfFramsburg 2 40 0 0 0 5)
    { cdCardTraits = setFromList [Undead, Hazard]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

orcOfTheFlame :: CardDef
orcOfTheFlame =
  (enemy "19076" "Orc of the Flame" FireInTheNight 4 25 2 2 2 3)
    { cdCardTraits = setFromList [Orc, Goblin]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    }
-- | X, its threat and defense, is 1 more than the number of players: printed as 0, the
-- card adds it.
fireDrake :: CardDef
fireDrake = unique (enemy "18042" "Fire-drake" TheKingsQuest 1 35 0 7 0 17) {cdCardTraits = setFromList [Dragon]}
-- | Its engagement cost is Hall of Beorn's 35, read from the card.
evilSpectre :: CardDef
evilSpectre =
  (enemy "19100" "Evil Spectre" TheGhostOfFramsburg 3 35 2 4 2 4)
    { cdCardTraits = setFromList [Undead, Hazard]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }
-- * Mount Gundabad. Easy mode copy counts from Hall of Beorn.

-- | Indestructible. Its threat is X, printed 0 here: see its card.
dagnirMountGundabad :: CardDef
dagnirMountGundabad =
  unique
    (enemy "19125" "Dagnir" MountGundabad 1 50 0 7 5 20)
      { cdCardTraits = setFromList [Dragon]
      , cdKeywords = setFromList [Indestructible]
      }

guardianOfGundabad :: CardDef
guardianOfGundabad = (enemy "19126" "Guardian of Gundabad" MountGundabad 3 39 3 4 1 5) {cdCardTraits = setFromList [Orc, Goblin], cdShadowEffect = True}

dagnirsSlave :: CardDef
dagnirsSlave =
  (enemy "19127" "Dagnir's Slave" MountGundabad 3 26 2 3 2 3)
    { cdCardTraits = setFromList [Orc, Goblin]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

deepDweller :: CardDef
deepDweller = (enemy "19128" "Deep-dweller" MountGundabad 2 32 2 3 1 4) {cdCardTraits = setFromList [Creature, Spider], cdEasyModeQuantity = Just 1}
dragonHatchling :: CardDef
dragonHatchling =
  (enemy "19014" "Dragon Hatchling" TheWitheredHeath 3 20 2 2 0 4)
    { cdCardTraits = setFromList [Dragon]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }
wilderlandBear :: CardDef
wilderlandBear =
  (enemy "19040" "Wilderland Bear" RoamAcrossRhovanion 2 30 3 4 1 5)
    { cdCardTraits = setFromList [Creature]
    , cdWhenRevealed = True
    }

-- | The enemy side of Urdug (Hall of Beorn's 19053): never gathered, Urdug's Gambit flips
-- his objective-ally side to it.
urdugEnemy :: CardDef
urdugEnemy =
  unique
    (enemy "19053" "Urdug" RoamAcrossRhovanion 0 15 2 3 2 5)
      { cdCardTraits = setFromList [Goblin, Orc]
      , cdKeywords = setFromList [Indestructible]
      , cdOtherSide = Just "19052"
      }

-- | The enemy side of Tiny (Hall of Beorn's 19055).
tinyEnemy :: CardDef
tinyEnemy =
  unique
    (enemy "19055" "Tiny" RoamAcrossRhovanion 0 50 1 6 3 9)
      { cdCardTraits = setFromList [Troll]
      , cdVictoryPoints = Just 6
      , cdOtherSide = Just "19054"
      }
mirkwoodPatrol :: CardDef
mirkwoodPatrol = (enemy "18034" "Mirkwood Patrol" LostInMirkwood 2 40 4 5 3 6) {cdCardTraits = setFromList [Orc], cdShadowEffect = True, cdEasyModeQuantity = Just 1}

ravenousSpider :: CardDef
ravenousSpider = (enemy "18035" "Ravenous Spider" LostInMirkwood 2 36 3 4 3 5) {cdCardTraits = setFromList [Creature, Spider], cdShadowEffect = True, cdEasyModeQuantity = Just 1}

-- * A Shadow in the East

-- | Sméagol's other side ('LordOfTheRings.Hero.CardDefs.smeagol'): a player card, never
-- gathered into an encounter deck.
gollumASitE :: CardDef
gollumASitE =
  unique
    (enemy "21073" "Gollum" GollumAndSmeagol 1 50 2 2 2 3)
      { cdCardTraits = setFromList [Gollum]
      , cdOtherSide = Just "21072"
      }

-- | The Power of Mordor deck's, not the encounter deck's.
khamulTheEasterling :: CardDef
khamulTheEasterling =
  unique
    (enemy "21066" "Khamûl the Easterling" ThePowerOfMordor 1 49 4 6 4 9)
      { cdCardTraits = setFromList [Nazgul, Easterling, Mordor]
      }

-- * A Shadow in the East's shared encounter sets

easterlingCaptain :: CardDef
easterlingCaptain =
  unique
    (enemy "21017" "Easterling Captain" RidersOfRhun 1 36 3 6 3 6)
      { cdCardTraits = setFromList [Easterling]
      , cdVictoryPoints = Just 6
      }

riderOfRhun :: CardDef
riderOfRhun =
  (enemy "21018" "Rider of Rhûn" RidersOfRhun 2 40 2 5 2 5)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Archery 2]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 1
    }

easterlingRaider :: CardDef
easterlingRaider =
  (enemy "21020" "Easterling Raider" EasterlingRaiders 3 28 2 2 2 3)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Archery 1]
    , cdShadowEffect = True
    }

warriorOfRhun :: CardDef
warriorOfRhun =
  (enemy "21021" "Warrior of Rhûn" EasterlingRaiders 3 34 3 3 3 4)
    { cdCardTraits = setFromList [Easterling]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

messengerFromMordor :: CardDef
messengerFromMordor =
  (enemy "21040" "Messenger from Mordor" ServantsOfSauron 3 34 2 4 2 4)
    { cdCardTraits = setFromList [Mordor]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

servantOfSauron :: CardDef
servantOfSauron =
  (enemy "21041" "Servant of Sauron" ServantsOfSauron 2 36 1 2 1 3)
    { cdCardTraits = setFromList [Mordor]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

rhunBackstabber :: CardDef
rhunBackstabber = (enemy "21044" "Rhûn Backstabber" CityOfRhun 2 28 2 3 2 3) {cdCardTraits = setFromList [Easterling], cdShadowEffect = True}

treacherousEasterling :: CardDef
treacherousEasterling = (enemy "21045" "Treacherous Easterling" CityOfRhun 2 32 3 3 1 5) {cdCardTraits = setFromList [Easterling], cdEasyModeQuantity = Just 1}

ulchorsGuard :: CardDef
ulchorsGuard =
  (enemy "21058" "Ulchor's Guard" UlchorsGuard 4 25 2 4 1 5)
    { cdCardTraits = setFromList [Easterling]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 3
    }

-- * The River Running

easterlingOutrider :: CardDef
easterlingOutrider =
  (enemy "21013" "Easterling Outrider" TheRiverRunning 3 24 1 2 1 2)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }
-- | The Temple of Doom's Thane Ulchor.
thaneUlchorTheTempleOfDoom :: CardDef
thaneUlchorTheTempleOfDoom =
  unique
    (enemy "21054" "Thane Ulchor" TheTempleOfDoom 1 50 0 6 4 9)
      { cdCardTraits = setFromList [Mordor]
      , cdKeywords = setFromList [Indestructible]
      , cdVictoryPoints = Just 6
      }
-- * The City of Ulfast. Easy mode copy counts from Hall of Beorn.

-- | Indestructible. Its threat is X, printed 0 here: see its card.
cityGuard :: CardDef
cityGuard =
  unique
    (enemy "22037" "City Guard" TheCityOfUlfast 1 45 0 9 4 20)
      { cdCardTraits = setFromList [Easterling]
      , cdKeywords = setFromList [Indestructible]
      , cdVictoryPoints = Just 5
      }

-- | Its attack is X, printed 0 here: see its card.
ulfastSentry :: CardDef
ulfastSentry =
  (enemy "22038" "Ulfast Sentry" TheCityOfUlfast 3 25 2 0 1 3)
    { cdCardTraits = setFromList [Easterling]
    , cdEasyModeQuantity = Just 2
    }

theKingsElite :: CardDef
theKingsElite =
  (enemy "22039" "The King's Elite" TheCityOfUlfast 2 30 3 5 2 5)
    { cdCardTraits = setFromList [Easterling]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

ulchorsAgent :: CardDef
ulchorsAgent =
  (enemy "22040" "Ulchor's Agent" TheCityOfUlfast 2 32 3 3 2 4)
    { cdCardTraits = setFromList [Mordor]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }
-- * Under the Ash Mountains

namelessBeast :: CardDef
namelessBeast = (enemy "22095" "Nameless Beast" UnderTheAshMountains 2 30 3 3 3 6) {cdCardTraits = setFromList [Nameless], cdShadowEffect = True}

mordorCaveTroll :: CardDef
mordorCaveTroll = (enemy "22096" "Mordor Cave Troll" UnderTheAshMountains 2 33 3 6 3 8) {cdCardTraits = setFromList [Troll], cdEasyModeQuantity = Just 1}

ashMountainWerewolf :: CardDef
ashMountainWerewolf = (enemy "22097" "Ash Mountain Werewolf" UnderTheAshMountains 3 28 2 4 1 5) {cdCardTraits = setFromList [Creature, Werewolf], cdShadowEffect = True}

spawnOfShelob :: CardDef
spawnOfShelob =
  (enemy "22098" "Spawn of Shelob" UnderTheAshMountains 3 12 1 2 1 2)
    { cdCardTraits = setFromList [Creature, Spider]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

-- | X threat, X 1 more than the number of quest cards in play: given by its own modifiers.
torechGorgorPatrol :: CardDef
torechGorgorPatrol = (enemy "22099" "Torech Gorgor Patrol" UnderTheAshMountains 2 45 0 5 4 9) {cdCardTraits = setFromList [Orc, Uruk], cdEasyModeQuantity = Just 1}
-- * Danger in Dorwinion

priestOfSauron :: CardDef
priestOfSauron =
  unique
    (enemy "21030" "Priest of Sauron" DangerInDorwinion 1 38 3 6 2 8)
      { cdCardTraits = setFromList [Cultist]
      , cdVictoryPoints = Just 6
      }

theGovernor :: CardDef
theGovernor =
  unique
    (enemy "21031" "The Governor" DangerInDorwinion 1 44 3 4 3 8)
      { cdCardTraits = setFromList [Cultist]
      , cdVictoryPoints = Just 6
      }

blackNumenoreanASitE :: CardDef
blackNumenoreanASitE =
  unique
    (enemy "21032" "Black Númenórean" DangerInDorwinion 1 40 3 5 1 9)
      { cdCardTraits = setFromList [Cultist]
      , cdVictoryPoints = Just 6
      }
-- | Wrath and Ruin's Thane Ulchor: the other versions of him have names of their own.
thaneUlchorWrathAndRuin :: CardDef
thaneUlchorWrathAndRuin =
  unique
    $ (enemy "22011" "Thane Ulchor" WrathAndRuin 1 50 1 6 4 9)
      { cdCardTraits = setFromList [Mordor]
      , cdVictoryPoints = Just 6
      }

-- * The Fortress of Nurn

-- | His threat is X, 1 more than the Castle side quests in the victory display.
thaneUlchorTheFortressOfNurn :: CardDef
thaneUlchorTheFortressOfNurn =
  unique
    (enemy "22148" "Thane Ulchor" TheFortressOfNurn 1 0 0 6 4 12)
      { cdCardTraits = setFromList [Mordor]
      , cdVictoryPoints = Just 6
      }

championOfRhun :: CardDef
championOfRhun = (enemy "22012" "Champion of Rhûn" WrathAndRuin 2 36 2 5 2 5) {cdCardTraits = setFromList [Easterling], cdShadowEffect = True}

easterlingSkirmisher :: CardDef
easterlingSkirmisher =
  (enemy "22013" "Easterling Skirmisher" WrathAndRuin 4 30 1 4 1 3)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Surge, Archery 1]
    , cdEasyModeQuantity = Just 2
    }

-- * Challenge of the Wainriders

-- | X is 2 per player: its threat and defense are raised by its card.
wainriderChampion :: CardDef
wainriderChampion =
  unique
    (enemy "22066" "Wainrider Champion" ChallengeOfTheWainriders 1 50 0 4 0 5)
      { cdCardTraits = setFromList [Easterling]
      , cdKeywords = setFromList [Indestructible]
      , cdVictoryPoints = Just 4
      }

aggressiveRacer :: CardDef
aggressiveRacer =
  (enemy "22067" "Aggressive Racer" ChallengeOfTheWainriders 4 28 1 4 2 3)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Archery 1]
    , cdEasyModeQuantity = Just 3
    , cdRacingResult = Just Turn
    }

chariotDriver :: CardDef
chariotDriver =
  (enemy "22068" "Chariot Driver" ChallengeOfTheWainriders 3 38 3 5 1 6)
    { cdCardTraits = setFromList [Easterling]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    , cdRacingResult = Just Straight
    }

veteranWainrider :: CardDef
veteranWainrider =
  (enemy "22069" "Veteran Wainrider" ChallengeOfTheWainriders 3 45 2 3 1 4)
    { cdCardTraits = setFromList [Easterling]
    , cdShadowEffect = True
    , cdRacingResult = Just Straight
    }

soldierOfNurn :: CardDef
soldierOfNurn = (enemy "22149" "Soldier of Nurn" TheFortressOfNurn 3 28 2 3 1 4) {cdCardTraits = setFromList [Mordor], cdShadowEffect = True}

easterlingMercenary :: CardDef
easterlingMercenary =
  (enemy "22150" "Easterling Mercenary" TheFortressOfNurn 2 32 3 4 2 3)
    { cdCardTraits = setFromList [Easterling]
    , cdShadowEffect = True
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 1
    }

mordorOlogHai :: CardDef
mordorOlogHai =
  (enemy "22151" "Mordor Olog-hai" TheFortressOfNurn 2 34 4 6 3 9)
    { cdCardTraits = setFromList [Troll]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

guardDogs :: CardDef
guardDogs =
  (enemy "22152" "Guard Dogs" TheFortressOfNurn 3 12 1 2 1 2)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }


-- * The Land of Sorrow

-- | A Warden: set up faceup, but not in play, under a Camp location (stage 1A).
warden :: CardCode -> Name -> Int -> Int -> Int -> Int -> Int -> CardDef
warden code name engagementCost threat attack defense hitPoints =
  unique
    (enemy code name TheLandOfSorrow 1 engagementCost threat attack defense hitPoints)
      { cdCardTraits = setFromList [Easterling, Warden]
      , cdVictoryPoints = Just 1
      }

ulwarth :: CardDef
ulwarth = warden "22118" "Ulwarth" 34 3 4 1 6

lorgan :: CardDef
lorgan = warden "22119" "Lorgan" 29 1 5 3 8

borlach :: CardDef
borlach = warden "22120" "Borlach" 33 2 6 2 9

easterlingGuard :: CardDef
easterlingGuard =
  (enemy "22127" "Easterling Guard" TheLandOfSorrow 3 32 3 4 2 5)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Archery 2]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

nurnSlaver :: CardDef
nurnSlaver =
  (enemy "22128" "Nurn Slaver" TheLandOfSorrow 4 23 2 3 2 6)
    { cdCardTraits = setFromList [Easterling]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 3
    }

-- * The Passing of the Grey Company

deadManOfDunharrow :: CardDef
deadManOfDunharrow =
  (enemy "145022" "Dead Man of Dunharrow" ThePassingOfTheGreyCompany 4 30 2 2 1 3)
    { cdCardTraits = setFromList [Undead, Oathbreaker]
    , cdKeywords = setFromList [Phantom]
    , cdShadowEffect = True
    }

faithlessDead :: CardDef
faithlessDead =
  (enemy "145023" "Faithless Dead" ThePassingOfTheGreyCompany 3 35 2 3 1 4)
    { cdCardTraits = setFromList [Undead, Oathbreaker]
    , cdKeywords = setFromList [Peril, Phantom]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

ghostsOfMen :: CardDef
ghostsOfMen =
  (enemy "145024" "Ghosts of Men" ThePassingOfTheGreyCompany 2 40 3 3 1 5)
    { cdCardTraits = setFromList [Undead, Oathbreaker]
    , cdKeywords = setFromList [Peril, Phantom]
    , cdWhenRevealed = True
    }

shadowHost :: CardDef
shadowHost =
  (enemy "145025" "Shadow Host" ThePassingOfTheGreyCompany 2 45 4 4 2 6)
    { cdCardTraits = setFromList [Undead, Oathbreaker]
    , cdKeywords = setFromList [Phantom]
    , cdEasyModeQuantity = Just 1
    }

-- | X threat, twice the number of players: given by its own modifiers. Set aside by stage
-- 1A, added to the staging area by stage 3A; its other side is the boon
-- 'LordOfTheRings.Objective.CardDefs.armyOfTheDeadBoon'.
armyOfTheDead :: CardDef
armyOfTheDead =
  unique
    (enemy "145067" "Army of the Dead" ThePassingOfTheGreyCompany 1 50 0 6 2 8)
      { cdCardTraits = setFromList [Undead, Oathbreaker]
      , cdKeywords = setFromList [Phantom]
      , cdVictoryPoints = Just 6
      , cdOtherSide = Just "145068"
      }

-- * The Ring Goes South

-- | Indestructible. Immune to player card effects (its card gives it the modifier).
watcherInTheWater :: CardDef
watcherInTheWater =
  unique
    (enemy "142024" "Watcher in the Water" TheRingGoesSouth 1 50 5 6 5 12)
      { cdCardTraits = setFromList [Creature]
      , cdKeywords = setFromList [Indestructible]
      }

-- | Cannot have attachments (its card gives it the modifier).
greatWargChief :: CardDef
greatWargChief =
  unique
    (enemy "142025" "Great Warg Chief" TheRingGoesSouth 1 30 4 5 3 6)
      { cdCardTraits = setFromList [Creature, Warg]
      }

houndOfSauron :: CardDef
houndOfSauron =
  (enemy "142026" "Hound of Sauron" TheRingGoesSouth 4 50 1 4 1 3)
    { cdCardTraits = setFromList [Creature, Warg]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

howlingWarg :: CardDef
howlingWarg =
  (enemy "142027" "Howling Warg" TheRingGoesSouth 4 38 2 3 2 4)
    { cdCardTraits = setFromList [Creature, Warg]
    }

crebainFromDunland :: CardDef
crebainFromDunland =
  (enemy "142028" "Crebain from Dunland" TheRingGoesSouth 2 28 3 1 1 2)
    { cdCardTraits = setFromList [Creature]
    , cdKeywords = setFromList [Peril]
    }

-- * Journey in the Dark

-- | Indestructible. Cannot be optionally engaged, immune to player card effects, and
-- engaged with the first player from the staging area (its card gives it the modifiers).
theBalrog :: CardDef
theBalrog =
  unique
    (enemy "142044" "The Balrog" JourneyInTheDark 1 50 5 8 9 25)
      { cdCardTraits = setFromList [Balrog, Shadow, Flame]
      , cdKeywords = setFromList [Indestructible]
      , cdVictoryPoints = Just 50
      }

-- | Cannot have attachments. Allies cannot defend against it (its card gives it the
-- modifiers).
orcChieftain :: CardDef
orcChieftain =
  (enemy "142045" "Orc-chieftain" JourneyInTheDark 1 40 4 4 3 6)
    { cdCardTraits = setFromList [Orc, Uruk]
    }

-- | Cannot have attachments (its card gives it the modifier).
greatCaveTrollJourneyInTheDark :: CardDef
greatCaveTrollJourneyInTheDark =
  (enemy "142046" "Great Cave-troll" JourneyInTheDark 2 36 3 6 4 6)
    { cdCardTraits = setFromList [Troll]
    , cdEasyModeQuantity = Just 1
    }

urukFromMordor :: CardDef
urukFromMordor =
  (enemy "142047" "Uruk from Mordor" JourneyInTheDark 3 34 2 4 2 5)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

moriaOrc :: CardDef
moriaOrc =
  (enemy "142048" "Moria Orc" JourneyInTheDark 3 38 2 3 1 3)
    { cdCardTraits = setFromList [Orc]
    , cdWhenRevealed = True
    , cdShadowEffect = True
    }

-- | Its X threat, the number of players, comes from its card's modifiers.
moriaArcher :: CardDef
moriaArcher =
  (enemy "142049" "Moria Archer" JourneyInTheDark 3 42 0 3 3 3)
    { cdCardTraits = setFromList [Orc]
    , cdKeywords = setFromList [Peril, Archery 2]
    , cdWhenRevealed = True
    , cdEasyModeQuantity = Just 2
    }
