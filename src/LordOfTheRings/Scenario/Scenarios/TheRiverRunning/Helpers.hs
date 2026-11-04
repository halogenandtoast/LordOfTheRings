-- | What the cards of The River Running share: the resources on Easterling Pursuit.
module LordOfTheRings.Scenario.Scenarios.TheRiverRunning.Helpers (
  getEasterlingPursuit,
  pursuitResources,
  placeOnEasterlingPursuit,
) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id (ObjectiveId)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (Objective, objectiveTokens)
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Token

-- | Easterling Pursuit, in the staging area.
getEasterlingPursuit :: HasGame m => m (Maybe ObjectiveId)
getEasterlingPursuit = fmap toId <$> pursuit

pursuit :: HasGame m => m (Maybe Objective)
pursuit = getsGame $ find ((== toCardCode Objectives.easterlingPursuit) . toCardCode) . toList . (.entities.objectives)

-- | The resource tokens on Easterling Pursuit.
pursuitResources :: HasGame m => m Int
pursuitResources = maybe 0 (countTokens Resource . objectiveTokens . toAttrs) <$> pursuit

placeOnEasterlingPursuit :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeOnEasterlingPursuit source n = getEasterlingPursuit >>= traverse_ \o -> placeResources source o n
