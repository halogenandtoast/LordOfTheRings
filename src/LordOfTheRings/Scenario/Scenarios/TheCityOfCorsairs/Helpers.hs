{- | What the stages of The City of Corsairs share with the scenario: whether the
Stormcaller was destroyed during stage 1, kept as the scenario's meta, and the switch to
the second encounter deck.
-}
module LordOfTheRings.Scenario.Scenarios.TheCityOfCorsairs.Helpers (
  stormcallerDestroyed,
  rememberStormcallerDestroyed,
  switchEncounterDeck,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (ReverseQueue)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))

-- | Whether the Stormcaller was destroyed during stage 1, as The Haven of Umbar asks.
stormcallerDestroyed :: HasGame m => m Bool
stormcallerDestroyed = getsGame ((== Just True) . parseMaybe parseJSON . scenarioMeta . toAttrs . gameScenario)

-- | The Coast of Umbar: the Stormcaller was destroyed during stage 1.
rememberStormcallerDestroyed :: ReverseQueue m => m ()
rememberStormcallerDestroyed = push $ ScenarioSpecific "cityOfCorsairs.stormcallerDestroyed" Null

{- | Fate of the Dream-chaser: the Corsair deck and its discard pile, and each Battle-hardened
and Watch Tower in the encounter deck and discard pile, are shuffled into the second
encounter deck, which becomes the active encounter deck; the current one is set aside.
-}
switchEncounterDeck :: ReverseQueue m => m ()
switchEncounterDeck = push $ ScenarioSpecific "cityOfCorsairs.switchEncounterDeck" Null
