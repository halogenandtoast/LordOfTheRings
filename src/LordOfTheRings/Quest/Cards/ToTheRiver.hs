module LordOfTheRings.Quest.Cards.ToTheRiver (toTheRiver) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ToTheRiver = ToTheRiver QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

toTheRiver :: QuestCard ToTheRiver
toTheRiver = quest ToTheRiver Cards.toTheRiver
