module LordOfTheRings.Treachery.Cards where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.EncounterSet
import LordOfTheRings.Prelude

allTreacheries :: Map CardCode (EncounterCardDef 'TreacheryKind)
allTreacheries =
  Map.fromList
    [ ("01079", eyesOfTheForest)
    , ("01080", caughtInAWeb)
    , ("01092", drivenByShadow)
    , ("01093", theNecromancersReach)
    ]

treachery :: CardCode -> Name -> EncounterSet -> (Int, Int, Int) -> EncounterCardDef 'TreacheryKind
treachery code name encounterSet encounterSetCount =
  EncounterCardDef
    { code
    , name
    , unique = False
    , kind = STreacheryKind
    , details = TreacheryDetails
    , encounterSet
    , encounterSetCount
    }

eyesOfTheForest :: EncounterCardDef 'TreacheryKind
eyesOfTheForest = treachery "01076" "Ungoliant's Spawn" SpidersOfMirkwood (0, 1, 1)

caughtInAWeb :: EncounterCardDef 'TreacheryKind
caughtInAWeb = treachery "01089" "Dol Guldur Orcs" SpidersOfMirkwood (0, 2, 0)

drivenByShadow :: EncounterCardDef 'TreacheryKind
drivenByShadow = treachery "01074" "King Spider" DolGuldurOrcs (1, 1, 1)

theNecromancersReach :: EncounterCardDef 'TreacheryKind
theNecromancersReach = treachery "01075" "Hummerhorns" DolGuldurOrcs (1, 3, 3)
