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
      , perilInPelargirNightmare
      , intoIthilienNightmare
      , theSiegeOfCairAndrosNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

perilInPelargirNightmare :: CardDef
perilInPelargirNightmare = nightmareSetup "heirs-of-numenor-nightmare-peril-in-pelargir-001" "Peril in Pelargir" PerilInPelargirNightmare

intoIthilienNightmare :: CardDef
intoIthilienNightmare = nightmareSetup "heirs-of-numenor-nightmare-into-ithilien-001" "Into Ithilien" IntoIthilienNightmare

theSiegeOfCairAndrosNightmare :: CardDef
theSiegeOfCairAndrosNightmare = nightmareSetup "heirs-of-numenor-nightmare-the-siege-of-cair-andros-001" "The Siege of Cair Andros" TheSiegeOfCairAndrosNightmare
