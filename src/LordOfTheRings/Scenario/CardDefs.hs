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
      , theStewardsFearNightmare
      , theDruadanForestNightmare
      , encounterAtAmonDinNightmare
      , assaultOnOsgiliathNightmare
      , theBloodOfGondorNightmare
      , theMorgulValeNightmare
      ]

passageThroughMirkwoodNightmare :: CardDef
passageThroughMirkwoodNightmare = nightmareSetup "core-set-nightmare-passage-through-mirkwood-001" "Passage Through Mirkwood" PassageThroughMirkwoodNightmare

journeyAlongTheAnduinNightmare :: CardDef
journeyAlongTheAnduinNightmare = nightmareSetup "core-set-nightmare-journey-along-the-anduin-001" "Journey Along the Anduin" JourneyAlongTheAnduinNightmare

escapeFromDolGuldurNightmare :: CardDef
escapeFromDolGuldurNightmare = nightmareSetup "core-set-nightmare-escape-from-dol-guldur-001" "Escape from Dol Guldur" EscapeFromDolGuldurNightmare

theStewardsFearNightmare :: CardDef
theStewardsFearNightmare = nightmareSetup "the-stewards-fear-nightmare-the-stewards-fear-001" "The Steward's Fear" TheStewardsFearNightmare

theDruadanForestNightmare :: CardDef
theDruadanForestNightmare = nightmareSetup "the-druadan-forest-nightmare-the-druadan-forest-001" "The Drúadan Forest" TheDruadanForestNightmare

encounterAtAmonDinNightmare :: CardDef
encounterAtAmonDinNightmare = nightmareSetup "encounter-at-amon-din-nightmare-encounter-at-amon-din-001" "Encounter at Amon Dîn" EncounterAtAmonDinNightmare

assaultOnOsgiliathNightmare :: CardDef
assaultOnOsgiliathNightmare = nightmareSetup "assault-on-osgiliath-nightmare-assault-on-osgiliath-001" "Assault on Osgiliath" AssaultOnOsgiliathNightmare

theBloodOfGondorNightmare :: CardDef
theBloodOfGondorNightmare = nightmareSetup "the-blood-of-gondor-nightmare-the-blood-of-gondor-001" "The Blood of Gondor" TheBloodOfGondorNightmare

theMorgulValeNightmare :: CardDef
theMorgulValeNightmare = nightmareSetup "the-morgul-vale-nightmare-the-morgul-vale-001" "The Morgul Vale" TheMorgulValeNightmare
