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
      , intrudersInChetwoodNightmare
      , theWeatherHillsNightmare
      , deadmensDikeNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

intrudersInChetwoodNightmare :: CardDef
intrudersInChetwoodNightmare = nightmareSetup "the-lost-realm-nightmare-invaders-in-chetwood-001" "Intruders in Chetwood" IntrudersInChetwoodNightmare

theWeatherHillsNightmare :: CardDef
theWeatherHillsNightmare = nightmareSetup "the-lost-realm-nightmare-the-weather-hills-011" "The Weather Hills" TheWeatherHillsNightmare

deadmensDikeNightmare :: CardDef
deadmensDikeNightmare = nightmareSetup "the-lost-realm-nightmare-deadmens-dike-019" "Deadmen's Dike" DeadmensDikeNightmare
