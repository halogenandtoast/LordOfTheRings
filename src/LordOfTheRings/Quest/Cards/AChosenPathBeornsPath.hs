module LordOfTheRings.Quest.Cards.AChosenPathBeornsPath (aChosenPathBeornsPath) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AChosenPathBeornsPath = AChosenPathBeornsPath QuestAttrs
  deriving anyclass (IsQuest, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

aChosenPathBeornsPath :: QuestCard AChosenPathBeornsPath
aChosenPathBeornsPath = quest AChosenPathBeornsPath Cards.aChosenPathBeornsPath
