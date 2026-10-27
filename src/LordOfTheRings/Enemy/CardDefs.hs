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
      , goblinSpearman
      , goblinArcher
      ]

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
