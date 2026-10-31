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
      , theDunlandTrapNightmare
      , theThreeTrialsNightmare
      , troubleInTharbadNightmare
      , theNinInEilphNightmare
      , celebrimborsSecretNightmare
      , theAntleredCrownNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

-- The Ring-maker Nightmare Decks

theDunlandTrapNightmare :: CardDef
theDunlandTrapNightmare = nightmareSetup "the-dunland-trap-nightmare-the-dunland-trap-001" "The Dunland Trap" TheDunlandTrapNightmare

theThreeTrialsNightmare :: CardDef
theThreeTrialsNightmare = nightmareSetup "the-three-trials-nightmare-the-three-trials-001" "The Three Trials" TheThreeTrialsNightmare

troubleInTharbadNightmare :: CardDef
troubleInTharbadNightmare = nightmareSetup "trouble-in-tharbad-nightmare-trouble-in-tharbad-001" "Trouble in Tharbad" TroubleInTharbadNightmare

theNinInEilphNightmare :: CardDef
theNinInEilphNightmare = nightmareSetup "the-nin-in-eilph-nightmare-the-nin-in-eilph-001" "The Nîn-in-Eilph" TheNinInEilphNightmare

celebrimborsSecretNightmare :: CardDef
celebrimborsSecretNightmare = nightmareSetup "celebrimbors-secret-nightmare-celebrimbors-secret-001" "Celebrimbor's Secret" CelebrimborsSecretNightmare

theAntleredCrownNightmare :: CardDef
theAntleredCrownNightmare = nightmareSetup "the-antlered-crown-nightmare-the-antlered-crown-001" "The Antlered Crown" TheAntleredCrownNightmare
