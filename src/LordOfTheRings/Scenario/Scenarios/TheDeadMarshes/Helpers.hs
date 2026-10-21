-- | What The Dead Marshes' cards share: Gollum, gaining resource tokens as he slips away.
module LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum, getGollum) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Classes.Entity (toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Id (ObjectiveId)
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Prelude
import LordOfTheRings.Source (Sourceable)

-- | Gollum, while he is in play.
getGollum :: (HasGame m) => m (Maybe ObjectiveId)
getGollum = getsGame (fmap toId . find ((== toCardCode Objectives.gollumTheDeadMarshes) . toCardCode) . toList . (.entities.objectives))

-- | Place resource tokens on Gollum, if he is in play.
placeOnGollum :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeOnGollum source n = getGollum >>= traverse_ \g -> placeResources source g n
