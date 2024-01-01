module LordOfTheRings.Enemy.Cards (module LordOfTheRings.Enemy.Cards) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Card.EncounterCard qualified as EC
import LordOfTheRings.EncounterSet
import LordOfTheRings.Name
import LordOfTheRings.Prelude

allEnemies :: Map CardCode (EncounterCardDef 'EnemyKind)
allEnemies =
  Map.fromList
    [ ("01074", kingSpider)
    , ("01075", hummerhorns)
    , ("01076", ungoliantsSpawn)
    , ("01089", dolGuldurOrcs)
    , ("01090", chieftanUfthak)
    , ("01091", dolGuldurBeastmaster)
    , ("01096", forestSpider)
    , ("01097", eastBightPatrol)
    , ("01098", blackForestBats)
    ]

enemy
  :: CardCode -> Name -> EncounterSet -> (Int, Int, Int) -> EnemyDetails -> EncounterCardDef 'EnemyKind
enemy code name encounterSet encounterSetCount details =
  EncounterCardDef
    { code
    , name
    , EC.unique = False
    , kind = SEnemyKind
    , details
    , encounterSet
    , encounterSetCount
    }

kingSpider :: EncounterCardDef 'EnemyKind
kingSpider =
  enemy "01074" "King Spider" SpidersOfMirkwood (2, 2, 2) $
    EnemyDetails
      { engagementCost = 20
      , threat = 2
      , attack = 3
      , defense = 1
      , hitPoints = 3
      }

hummerhorns :: EncounterCardDef 'EnemyKind
hummerhorns =
  enemy "01075" "Hummerhorns" SpidersOfMirkwood (0, 1, 1) $
    EnemyDetails
      { engagementCost = 40
      , threat = 1
      , attack = 2
      , defense = 0
      , hitPoints = 3
      }

ungoliantsSpawn :: EncounterCardDef 'EnemyKind
ungoliantsSpawn =
  enemy "01076" "Ungoliant's Spawn" SpidersOfMirkwood (1, 1, 0) $
    EnemyDetails
      { engagementCost = 32
      , threat = 3
      , attack = 5
      , defense = 2
      , hitPoints = 9
      }

dolGuldurOrcs :: EncounterCardDef 'EnemyKind
dolGuldurOrcs =
  enemy "01089" "Dol Guldur Orcs" DolGuldurOrcs (3, 3, 0) $
    EnemyDetails
      { engagementCost = 10
      , threat = 2
      , attack = 2
      , defense = 0
      , hitPoints = 3
      }

chieftanUfthak :: EncounterCardDef 'EnemyKind
chieftanUfthak =
  enemy "01090" "Chieftan Ufthak" DolGuldurOrcs (0, 1, 1) $
    EnemyDetails
      { engagementCost = 35
      , threat = 2
      , attack = 3
      , defense = 3
      , hitPoints = 6
      }

dolGuldurBeastmaster :: EncounterCardDef 'EnemyKind
dolGuldurBeastmaster =
  enemy "01091" "Dol Guldur Beastmaster" DolGuldurOrcs (1, 2, 2) $
    EnemyDetails
      { engagementCost = 35
      , threat = 2
      , attack = 3
      , defense = 1
      , hitPoints = 5
      }

forestSpider :: EncounterCardDef 'EnemyKind
forestSpider =
  enemy "01096" "Forest Spider" PassageThroughMirkwood (4, 4, 1) $
    EnemyDetails
      { engagementCost = 25
      , threat = 2
      , attack = 2
      , defense = 1
      , hitPoints = 4
      }

eastBightPatrol :: EncounterCardDef 'EnemyKind
eastBightPatrol =
  enemy "01097" "East Bight Patrol" PassageThroughMirkwood (1, 1, 1) $
    EnemyDetails
      { engagementCost = 5
      , threat = 3
      , attack = 3
      , defense = 1
      , hitPoints = 2
      }

blackForestBats :: EncounterCardDef 'EnemyKind
blackForestBats =
  enemy "01098" "Black Forest Bats" PassageThroughMirkwood (1, 1, 0) $
    EnemyDetails
      { engagementCost = 15
      , threat = 1
      , attack = 1
      , defense = 0
      , hitPoints = 2
      }
