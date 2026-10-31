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
      , theFordsOfIsenNightmare
      , toCatchAnOrcNightmare
      , intoFangornNightmare
      , voyageAcrossBelegaerNightmare
      , theFateOfNumenorNightmare
      , raidOnTheGreyHavensNightmare
      , intrudersInChetwoodNightmare
      , theWeatherHillsNightmare
      , deadmensDikeNightmare
      , intoThePitNightmare
      , theSeventhLevelNightmare
      , flightFromMoriaNightmare
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

perilInPelargirNightmare :: CardDef
perilInPelargirNightmare = nightmareSetup "heirs-of-numenor-nightmare-peril-in-pelargir-001" "Peril in Pelargir" PerilInPelargirNightmare

intoIthilienNightmare :: CardDef
intoIthilienNightmare = nightmareSetup "heirs-of-numenor-nightmare-into-ithilien-001" "Into Ithilien" IntoIthilienNightmare

theSiegeOfCairAndrosNightmare :: CardDef
theSiegeOfCairAndrosNightmare = nightmareSetup "heirs-of-numenor-nightmare-the-siege-of-cair-andros-001" "The Siege of Cair Andros" TheSiegeOfCairAndrosNightmare

theFordsOfIsenNightmare :: CardDef
theFordsOfIsenNightmare = nightmareSetup "the-voice-of-isengard-nightmare-the-fords-of-isen-001" "The Fords of Isen" TheFordsOfIsenNightmare

toCatchAnOrcNightmare :: CardDef
toCatchAnOrcNightmare = nightmareSetup "the-voice-of-isengard-nightmare-to-catch-an-orc-001" "To Catch an Orc" ToCatchAnOrcNightmare

intoFangornNightmare :: CardDef
intoFangornNightmare = nightmareSetup "the-voice-of-isengard-nightmare-into-fangorn-001" "Into Fangorn" IntoFangornNightmare

voyageAcrossBelegaerNightmare :: CardDef
voyageAcrossBelegaerNightmare = nightmareSetup "the-grey-havens-nightmare-voyage-across-belegaer-001" "Voyage Across Belegaer" VoyageAcrossBelegaerNightmare

theFateOfNumenorNightmare :: CardDef
theFateOfNumenorNightmare = nightmareSetup "the-grey-havens-nightmare-the-fate-of-numenor-001" "The Fate of Númenor" TheFateOfNumenorNightmare

raidOnTheGreyHavensNightmare :: CardDef
raidOnTheGreyHavensNightmare = nightmareSetup "the-grey-havens-nightmare-raid-on-the-grey-havens-001" "Raid on the Grey Havens" RaidOnTheGreyHavensNightmare

intrudersInChetwoodNightmare :: CardDef
intrudersInChetwoodNightmare = nightmareSetup "the-lost-realm-nightmare-invaders-in-chetwood-001" "Intruders in Chetwood" IntrudersInChetwoodNightmare

theWeatherHillsNightmare :: CardDef
theWeatherHillsNightmare = nightmareSetup "the-lost-realm-nightmare-the-weather-hills-011" "The Weather Hills" TheWeatherHillsNightmare

deadmensDikeNightmare :: CardDef
deadmensDikeNightmare = nightmareSetup "the-lost-realm-nightmare-deadmens-dike-019" "Deadmen's Dike" DeadmensDikeNightmare

intoThePitNightmare :: CardDef
intoThePitNightmare = nightmareSetup "khazad-dum-nightmare-into-the-pit-001" "Into the Pit" IntoThePitNightmare

theSeventhLevelNightmare :: CardDef
theSeventhLevelNightmare = nightmareSetup "khazad-dum-nightmare-the-seventh-level-001" "The Seventh Level" TheSeventhLevelNightmare

flightFromMoriaNightmare :: CardDef
flightFromMoriaNightmare = nightmareSetup "khazad-dum-nightmare-flight-from-moria-001" "Flight from Moria" FlightFromMoriaNightmare

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
