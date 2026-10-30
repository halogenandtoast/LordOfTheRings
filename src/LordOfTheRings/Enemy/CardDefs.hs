module LordOfTheRings.Enemy.CardDefs (module LordOfTheRings.Enemy.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Trait

allEnemyCards :: Map CardCode CardDef
allEnemyCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ kingSpider
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
      , daechanar
      , dwimmerlaik
      , witchOfAngmar
      , wraithOfCarnDum
      , thaurdirTheTreacheryOfRhudaur
      , wightOfRhudaur
      , traitorousWight
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
    , cdCardTraits = setFromList [Creature, Spider]
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
