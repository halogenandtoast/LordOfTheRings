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
      , theFordsOfIsenNightmare
      , toCatchAnOrcNightmare
      , intoFangornNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

theFordsOfIsenNightmare :: CardDef
theFordsOfIsenNightmare = nightmareSetup "the-voice-of-isengard-nightmare-the-fords-of-isen-001" "The Fords of Isen" TheFordsOfIsenNightmare

toCatchAnOrcNightmare :: CardDef
toCatchAnOrcNightmare = nightmareSetup "the-voice-of-isengard-nightmare-to-catch-an-orc-001" "To Catch an Orc" ToCatchAnOrcNightmare

intoFangornNightmare :: CardDef
intoFangornNightmare = nightmareSetup "the-voice-of-isengard-nightmare-into-fangorn-001" "Into Fangorn" IntoFangornNightmare
