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
      , intoThePitNightmare
      , theSeventhLevelNightmare
      , flightFromMoriaNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

intoThePitNightmare :: CardDef
intoThePitNightmare = nightmareSetup "khazad-dum-nightmare-into-the-pit-001" "Into the Pit" IntoThePitNightmare

theSeventhLevelNightmare :: CardDef
theSeventhLevelNightmare = nightmareSetup "khazad-dum-nightmare-the-seventh-level-001" "The Seventh Level" TheSeventhLevelNightmare

flightFromMoriaNightmare :: CardDef
flightFromMoriaNightmare = nightmareSetup "khazad-dum-nightmare-flight-from-moria-001" "Flight from Moria" FlightFromMoriaNightmare
