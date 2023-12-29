module LordOfTheRings.Enemy.Cards where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.EncounterSet
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

enemy :: CardCode -> Name -> EncounterSet -> (Int, Int, Int) -> EncounterCardDef 'EnemyKind
enemy code name encounterSet encounterSetCount =
  EncounterCardDef
    { code
    , name
    , unique = False
    , kind = SEnemyKind
    , details = EnemyDetails
    , encounterSet
    , encounterSetCount
    }

kingSpider :: EncounterCardDef 'EnemyKind
kingSpider = enemy "01074" "King Spider" SpidersOfMirkwood (2, 2, 2)

hummerhorns :: EncounterCardDef 'EnemyKind
hummerhorns = enemy "01075" "Hummerhorns" SpidersOfMirkwood (0, 1, 1)

ungoliantsSpawn :: EncounterCardDef 'EnemyKind
ungoliantsSpawn = enemy "01076" "Ungoliant's Spawn" SpidersOfMirkwood (1, 1, 0)

dolGuldurOrcs :: EncounterCardDef 'EnemyKind
dolGuldurOrcs = enemy "01089" "Dol Guldur Orcs" DolGuldurOrcs (3, 3, 0)

chieftanUfthak :: EncounterCardDef 'EnemyKind
chieftanUfthak = enemy "01090" "Chieftan Ufthak" DolGuldurOrcs (0, 1, 1)

dolGuldurBeastmaster :: EncounterCardDef 'EnemyKind
dolGuldurBeastmaster = enemy "01091" "Dol Guldur Beastmaster" DolGuldurOrcs (1, 2, 2)

forestSpider :: EncounterCardDef 'EnemyKind
forestSpider = enemy "01096" "Forest Spider" PassageThroughMirkwood (4, 4, 1)

eastBightPatrol :: EncounterCardDef 'EnemyKind
eastBightPatrol = enemy "01097" "East Bight Patrol" PassageThroughMirkwood (1, 1, 1)

blackForestBats :: EncounterCardDef 'EnemyKind
blackForestBats = enemy "01098" "Black Forest Bats" PassageThroughMirkwood (1, 1, 0)
