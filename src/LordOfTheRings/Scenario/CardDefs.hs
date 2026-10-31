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
      , theWastesOfEriadorNightmare
      , escapeFromMountGramNightmare
      , acrossTheEttenmoorsNightmare
      , theTreacheryOfRhudaurNightmare
      , theBattleOfCarnDumNightmare
      , theDreadRealmNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

theWastesOfEriadorNightmare :: CardDef
theWastesOfEriadorNightmare = nightmareSetup "the-wastes-of-eriador-nightmare-the-wastes-of-eriador-001" "The Wastes of Eriador" TheWastesOfEriadorNightmare

escapeFromMountGramNightmare :: CardDef
escapeFromMountGramNightmare = nightmareSetup "escape-from-mount-gram-nightmare-escape-from-mount-gram-001" "Escape from Mount Gram" EscapeFromMountGramNightmare

acrossTheEttenmoorsNightmare :: CardDef
acrossTheEttenmoorsNightmare = nightmareSetup "across-the-ettenmoors-nightmare-across-the-ettenmoors-001" "Across the Ettenmoors" AcrossTheEttenmoorsNightmare

theTreacheryOfRhudaurNightmare :: CardDef
theTreacheryOfRhudaurNightmare = nightmareSetup "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-001" "The Treachery of Rhudaur" TheTreacheryOfRhudaurNightmare

theBattleOfCarnDumNightmare :: CardDef
theBattleOfCarnDumNightmare = nightmareSetup "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-001" "The Battle of Carn Dûm" TheBattleOfCarnDumNightmare

theDreadRealmNightmare :: CardDef
theDreadRealmNightmare = nightmareSetup "the-dread-realm-nightmare-the-dread-realm-001" "The Dread Realm" TheDreadRealmNightmare
