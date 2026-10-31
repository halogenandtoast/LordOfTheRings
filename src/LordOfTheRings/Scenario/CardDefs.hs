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
      , theRedhornGateNightmare
      , roadToRivendellNightmare
      , theWatcherInTheWaterNightmare
      , theLongDarkNightmare
      , foundationsOfStoneNightmare
      , shadowAndFlameNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

theRedhornGateNightmare :: CardDef
theRedhornGateNightmare = nightmareSetup "the-redhorn-gate-nightmare-the-redhorn-gate-001" "The Redhorn Gate" TheRedhornGateNightmare

roadToRivendellNightmare :: CardDef
roadToRivendellNightmare = nightmareSetup "road-to-rivendell-nightmare-road-to-rivendell-001" "Road to Rivendell" RoadToRivendellNightmare

theWatcherInTheWaterNightmare :: CardDef
theWatcherInTheWaterNightmare = nightmareSetup "the-watcher-in-the-water-nightmare-the-watcher-in-the-water-001" "The Watcher in the Water" TheWatcherInTheWaterNightmare

theLongDarkNightmare :: CardDef
theLongDarkNightmare = nightmareSetup "the-long-dark-nightmare-the-long-dark-001" "The Long Dark" TheLongDarkNightmare

foundationsOfStoneNightmare :: CardDef
foundationsOfStoneNightmare = nightmareSetup "foundations-of-stone-nightmare-foundations-of-stone-001" "Foundations of Stone" FoundationsOfStoneNightmare

shadowAndFlameNightmare :: CardDef
shadowAndFlameNightmare = nightmareSetup "shadow-and-flame-nightmare-shadow-and-flame-001" "Shadow and Flame" ShadowAndFlameNightmare
