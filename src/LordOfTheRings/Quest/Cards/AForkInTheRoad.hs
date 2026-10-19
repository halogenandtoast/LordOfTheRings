module LordOfTheRings.Quest.Cards.AForkInTheRoad (aForkInTheRoad) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AForkInTheRoad = AForkInTheRoad QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

aForkInTheRoad :: QuestCard AForkInTheRoad
aForkInTheRoad = quest AForkInTheRoad Cards.aForkInTheRoad
