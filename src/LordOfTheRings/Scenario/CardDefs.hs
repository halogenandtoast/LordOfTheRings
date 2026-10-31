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
      , flightOfTheStormcallerNightmare
      , theThingInTheDepthsNightmare
      , templeOfTheDeceivedNightmare
      , theDrownedRuinsNightmare
      , aStormOnCobasHavenNightmare
      , theCityOfCorsairsNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

-- Dream-chaser Nightmare Decks

flightOfTheStormcallerNightmare :: CardDef
flightOfTheStormcallerNightmare = nightmareSetup "flight-of-the-stormcaller-nightmare-flight-of-the-stormcaller-001" "Flight of the Stormcaller" FlightOfTheStormcallerNightmare

theThingInTheDepthsNightmare :: CardDef
theThingInTheDepthsNightmare = nightmareSetup "the-thing-in-the-depths-nightmare-the-thing-in-the-depths-001" "The Thing in the Depths" TheThingInTheDepthsNightmare

templeOfTheDeceivedNightmare :: CardDef
templeOfTheDeceivedNightmare = nightmareSetup "temple-of-the-deceived-nightmare-temple-of-the-deceived-001" "Temple of the Deceived" TempleOfTheDeceivedNightmare

theDrownedRuinsNightmare :: CardDef
theDrownedRuinsNightmare = nightmareSetup "the-drowned-ruins-nightmare-the-drowned-ruins-001" "The Drowned Ruins" TheDrownedRuinsNightmare

aStormOnCobasHavenNightmare :: CardDef
aStormOnCobasHavenNightmare = nightmareSetup "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-001" "A Storm on Cobas Haven" AStormOnCobasHavenNightmare

theCityOfCorsairsNightmare :: CardDef
theCityOfCorsairsNightmare = nightmareSetup "the-city-of-corsairs-nightmare-the-city-of-corsairs-001" "The City of Corsairs" TheCityOfCorsairsNightmare
