module LordOfTheRings.Location.Cards where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.EncounterSet
import LordOfTheRings.Prelude

allLocations :: Map CardCode (EncounterCardDef 'LocationKind)
allLocations =
  Map.fromList
    [ ("01077", greatForestWeb)
    , ("01078", mountainsOfMirkwood)
    , ("01094", necromancersPass)
    , ("01095", enchantedStream)
    , ("01099", oldForestRoad)
    , ("01100", forestGate)
    ]

location :: CardCode -> Name -> EncounterSet -> (Int, Int, Int) -> EncounterCardDef 'LocationKind
location code name encounterSet encounterSetCount =
  EncounterCardDef
    { code
    , name
    , unique = False
    , kind = SLocationKind
    , details = LocationDetails
    , encounterSet
    , encounterSetCount
    }

greatForestWeb :: EncounterCardDef 'LocationKind
greatForestWeb = location "01077" "Great Forest Web" SpidersOfMirkwood (2, 2, 2)

mountainsOfMirkwood :: EncounterCardDef 'LocationKind
mountainsOfMirkwood = location "01078" "Mountains of Mirkwood" SpidersOfMirkwood (3, 3, 0)

necromancersPass :: EncounterCardDef 'LocationKind
necromancersPass = location "01094" "Necromancer's Pass" DolGuldurOrcs (1, 2, 2)

enchantedStream :: EncounterCardDef 'LocationKind
enchantedStream = location "01095" "Enchanted Stream" DolGuldurOrcs (2, 2, 2)

oldForestRoad :: EncounterCardDef 'LocationKind
oldForestRoad = location "01099" "Old Forest Road" PassageThroughMirkwood (1, 2, 2)

forestGate :: EncounterCardDef 'LocationKind
forestGate = location "01100" "Forest Gate" PassageThroughMirkwood (2, 2, 0)
