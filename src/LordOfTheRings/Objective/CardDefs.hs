module LordOfTheRings.Objective.CardDefs (module LordOfTheRings.Objective.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Trait

allObjectiveCards :: Map CardCode CardDef
allObjectiveCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ gandalfsMap
      , caveTorch
      , dungeonTorch
      , shadowKey
      , mendor
      , signsOfGollum
      , grimbeornTheOld
      , athelas
      , wilyador
      , gollumTheDeadMarshes
      , gollumReturnToMirkwood
      , abandonedTools
      , bookOfMazarbul
      , arwenUndomielTheRedhornGate
      , arwenUndomielRoadToRivendell
      , durinsAxe
      , durinsHelm
      ]

durinsAxe :: CardDef
durinsAxe =
  unique
    (objective "04119" "Durin's Axe" FoundationsOfStone 1)
      { cdCardTraits = setFromList [Weapon, Artifact]
      , cdKeywords = setFromList [Surge, Restricted]
      }

durinsHelm :: CardDef
durinsHelm =
  unique
    (objective "04120" "Durin's Helm" FoundationsOfStone 1)
      { cdCardTraits = setFromList [Armor, Artifact]
      , cdKeywords = setFromList [Surge]
      }

caveTorch :: CardDef
caveTorch =
  (objective "03041" "Cave Torch" TwistsAndTurns 1)
    { cdCardTraits = setFromList [Light]
    , cdKeywords = setFromList [Restricted]
    }

gandalfsMap :: CardDef
gandalfsMap =
  (objective "01108" "Gandalf's Map" EscapeFromDolGuldur 1)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Guarded, Restricted]
    }

dungeonTorch :: CardDef
dungeonTorch =
  (objective "01109" "Dungeon Torch" EscapeFromDolGuldur 1)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Guarded, Restricted]
    }

shadowKey :: CardDef
shadowKey =
  (objective "01110" "Shadow Key" EscapeFromDolGuldur 1)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Guarded, Restricted]
    }

mendor :: CardDef
mendor =
  ((objective "RC002" "Mendor" PassageThroughMirkwoodCampaign 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 1, cdDefense = Just 1, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Scout, Silvan]
    , cdKeywords = setFromList [Ranged]
    }

signsOfGollum :: CardDef
signsOfGollum =
  (objective "02014" "Signs of Gollum" TheHuntForGollum 4)
    { cdCardTraits = setFromList [Clue]
    , cdKeywords = setFromList [Guarded]
    }

grimbeornTheOld :: CardDef
grimbeornTheOld =
  ((objective "02037" "Grimbeorn the Old" ConflictAtTheCarrock 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 4, cdDefense = Just 3, cdHitPoints = Just 10 })
    { cdUnique = True
    }

athelas :: CardDef
athelas =
  (objective "02063" "Athelas" AJourneyToRhosgobel 4)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Guarded]
    }

wilyador :: CardDef
wilyador =
  ((objective "02064" "Wilyador" AJourneyToRhosgobel 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 1, cdDefense = Just 1, cdHitPoints = Just 20 })
    { cdUnique = True
    , cdCardTraits = setFromList [Creature, Eagle]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

gollumTheDeadMarshes :: CardDef
gollumTheDeadMarshes =
  (objective "02107" "Gollum" TheDeadMarshes 1)
    { cdUnique = True
    , cdCardTraits = setFromList [Gollum]
    }

gollumReturnToMirkwood :: CardDef
gollumReturnToMirkwood =
  ((objective "02130" "Gollum" ReturnToMirkwood 1) {cdHitPoints = Just 5})
    { cdUnique = True
    , cdCardTraits = setFromList [Creature]
    }

bookOfMazarbul :: CardDef
bookOfMazarbul =
  (objective "03024" "Book of Mazarbul" TheSeventhLevel 1)
    { cdCardTraits = setFromList [Item, Artifact]
    , cdKeywords = setFromList [Restricted]
    }

abandonedTools :: CardDef
abandonedTools =
  (objective "03029" "Abandoned Tools" FlightFromMoria 1)
    { cdCardTraits = setFromList [Tools]
    , cdKeywords = setFromList [Guarded, Restricted]
    }

-- | The Redhorn Gate's and Road to Rivendell's Arwen, the same card in each set.
arwenUndomiel :: CardCode -> EncounterSet -> CardDef
arwenUndomiel code set =
  ((objective code "Arwen Undómiel" set 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 0, cdDefense = Just 1, cdHitPoints = Just 2 })
    { cdUnique = True
    , cdCardTraits = setFromList [Noldor, Noble]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

arwenUndomielTheRedhornGate :: CardDef
arwenUndomielTheRedhornGate = arwenUndomiel "04014" TheRedhornGate

arwenUndomielRoadToRivendell :: CardDef
arwenUndomielRoadToRivendell = arwenUndomiel "04041" RoadToRivendell
