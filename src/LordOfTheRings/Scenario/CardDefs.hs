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
      , voyageAcrossBelegaerNightmare
      , theFateOfNumenorNightmare
      , raidOnTheGreyHavensNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

voyageAcrossBelegaerNightmare :: CardDef
voyageAcrossBelegaerNightmare = nightmareSetup "the-grey-havens-nightmare-voyage-across-belegaer-001" "Voyage Across Belegaer" VoyageAcrossBelegaerNightmare

theFateOfNumenorNightmare :: CardDef
theFateOfNumenorNightmare = nightmareSetup "the-grey-havens-nightmare-the-fate-of-numenor-001" "The Fate of Númenor" TheFateOfNumenorNightmare

raidOnTheGreyHavensNightmare :: CardDef
raidOnTheGreyHavensNightmare = nightmareSetup "the-grey-havens-nightmare-raid-on-the-grey-havens-001" "Raid on the Grey Havens" RaidOnTheGreyHavensNightmare
