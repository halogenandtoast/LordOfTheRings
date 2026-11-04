{- | What the cards of The Uruk-hai share: the pursuit tracker's value, kept by the scenario
in its meta, and the Captives, the heroes faceup under To Isengard! or attached to
Grishnákh (MEC45 rules, "Pursuit Value & Pursuit Tracker").

A Captive is a hero guarded by an encounter card (see 'GuardCharacter'): in play, but under
no player's control.
-}
module LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (
  pursuitOf,
  getPursuit,
  increasePursuit,
  reducePursuit,
  setPursuit,
  getCaptives,
  getToIsengard,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Entities (entitiesHeroes, entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities, gameScenario)
import LordOfTheRings.Hero.Types (HeroAttrs (heroGuardedBy))
import LordOfTheRings.Id (HeroId)
import LordOfTheRings.Message (Message (ScenarioSpecific))
import LordOfTheRings.Message.Lifted (ReverseQueue)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (Objective)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioMeta)

-- | The pursuit value the scenario's meta holds, 0 before the pursuit tracker is set.
pursuitOf :: Value -> Int
pursuitOf = fromMaybe 0 . parseMaybe (withObject "meta" (.: "pursuit"))

getPursuit :: HasGame m => m Int
getPursuit = getsGame (pursuitOf . scenarioMeta . toAttrs . gameScenario)

-- | The scenario moves the pursuit tracker by this much, never below 0.
increasePursuit :: ReverseQueue m => Int -> m ()
increasePursuit = push . ScenarioSpecific "urukHai.pursuit" . toJSON

reducePursuit :: ReverseQueue m => Int -> m ()
reducePursuit = increasePursuit . negate

-- | "Set the pursuit value to X."
setPursuit :: ReverseQueue m => Int -> m ()
setPursuit = push . ScenarioSpecific "urukHai.setPursuit" . toJSON

-- | The Captives in play: the heroes an encounter card guards.
getCaptives :: HasGame m => m [HeroId]
getCaptives = getsGame (map (.id) . filter (isJust . heroGuardedBy . toAttrs) . toList . entitiesHeroes . gameEntities)

getToIsengard :: HasGame m => m (Maybe Objective)
getToIsengard = getsGame (find ((== toCardCode Objectives.toIsengard) . toCardCode) . toList . entitiesObjectives . gameEntities)
