module LordOfTheRings.Scenario.CardDefs (module LordOfTheRings.Scenario.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Prelude

-- | The Nightmare Decks' setup cards, each kept flipped beside the quest deck; their
-- text is the scenario's (see 'LordOfTheRings.Scenario.Types.NightmareDeck').
allNightmareCards :: Map CardCode CardDef
allNightmareCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ passageThroughMirkwoodNightmare
      , journeyAlongTheAnduinNightmare
      , escapeFromDolGuldurNightmare
      , theHuntForGollumNightmare
      , conflictAtTheCarrockNightmare
      , aJourneyToRhosgobelNightmare
      , theHillsOfEmynMuilNightmare
      , theDeadMarshesNightmare
      , returnToMirkwoodNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

-- Shadows of Mirkwood Nightmare Decks

theHuntForGollumNightmare :: CardDef
theHuntForGollumNightmare = nightmareSetup "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-001" "The Hunt for Gollum" TheHuntForGollumNightmare

conflictAtTheCarrockNightmare :: CardDef
conflictAtTheCarrockNightmare = nightmareSetup "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-001" "Conflict at the Carrock" ConflictAtTheCarrockNightmare

aJourneyToRhosgobelNightmare :: CardDef
aJourneyToRhosgobelNightmare = nightmareSetup "a-journey-to-rhosgobel-nightmare-a-journey-to-rhosgobel-001" "A Journey to Rhosgobel" AJourneyToRhosgobelNightmare

theHillsOfEmynMuilNightmare :: CardDef
theHillsOfEmynMuilNightmare = nightmareSetup "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-001" "The Hills of Emyn Muil" TheHillsOfEmynMuilNightmare

theDeadMarshesNightmare :: CardDef
theDeadMarshesNightmare = nightmareSetup "the-dead-marshes-nightmare-the-dead-marshes-001" "The Dead Marshes" TheDeadMarshesNightmare

returnToMirkwoodNightmare :: CardDef
returnToMirkwoodNightmare = nightmareSetup "return-to-mirkwood-nightmare-return-to-mirkwood-001" "Return to Mirkwood" ReturnToMirkwoodNightmare
