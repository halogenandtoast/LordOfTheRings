module LordOfTheRings.Quest.Cards where

import LordOfTheRings.Card
import LordOfTheRings.EncounterSet
import LordOfTheRings.Prelude

quest :: CardCode -> Name -> EncounterSet -> EncounterCardDef 'QuestKind
quest code name encounterSet =
  EncounterCardDef
    { code
    , name
    , unique = False
    , kind = SQuestKind
    , details = QuestDetails
    , encounterSet
    , encounterSetCount = (1, 1, 1)
    }

fliesAndSpiders :: EncounterCardDef 'QuestKind
fliesAndSpiders = quest "01119" "Flies and Spiders" PassageThroughMirkwood

aForkInTheRoad :: EncounterCardDef 'QuestKind
aForkInTheRoad = quest "01120" "A Fork in the Road" PassageThroughMirkwood

aChosenPathDontLeaveThePath :: EncounterCardDef 'QuestKind
aChosenPathDontLeaveThePath = quest "01121" "A Chosen Path" PassageThroughMirkwood

aChosenPathBeornsPath :: EncounterCardDef 'QuestKind
aChosenPathBeornsPath = quest "01122" "A Chosen Path" PassageThroughMirkwood
