module LordOfTheRings.Quest.Cards.AChosenPathDontLeaveThePath (aChosenPathDontLeaveThePath) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AChosenPathDontLeaveThePath = AChosenPathDontLeaveThePath QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

aChosenPathDontLeaveThePath :: QuestCard AChosenPathDontLeaveThePath
aChosenPathDontLeaveThePath = quest AChosenPathDontLeaveThePath Cards.aChosenPathDontLeaveThePath
