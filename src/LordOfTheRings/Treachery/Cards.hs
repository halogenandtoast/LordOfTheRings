module LordOfTheRings.Treachery.Cards (module LordOfTheRings.Treachery.Cards) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Card.EncounterCard qualified as EC
import LordOfTheRings.EncounterSet
import LordOfTheRings.Name
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
    , EC.unique = False
    , kind = STreacheryKind
    , details = TreacheryDetails
    , encounterSet
    , encounterSetCount
    }

eyesOfTheForest :: EncounterCardDef 'TreacheryKind
eyesOfTheForest = treachery "01079" "Eyes of the Forest" SpidersOfMirkwood (0, 1, 1)

caughtInAWeb :: EncounterCardDef 'TreacheryKind
caughtInAWeb = treachery "01080" "Caught in a Web" SpidersOfMirkwood (0, 2, 0)

drivenByShadow :: EncounterCardDef 'TreacheryKind
drivenByShadow = treachery "01092" "Driven by Shadow" DolGuldurOrcs (1, 1, 1)

theNecromancersReach :: EncounterCardDef 'TreacheryKind
theNecromancersReach = treachery "01093" "The Necromancer's Reach" DolGuldurOrcs (1, 3, 3)
