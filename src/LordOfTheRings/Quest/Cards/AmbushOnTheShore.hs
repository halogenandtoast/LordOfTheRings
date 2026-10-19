module LordOfTheRings.Quest.Cards.AmbushOnTheShore (ambushOnTheShore) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AmbushOnTheShore = AmbushOnTheShore QuestAttrs
  deriving anyclass (IsQuest, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

ambushOnTheShore :: QuestCard AmbushOnTheShore
ambushOnTheShore = quest AmbushOnTheShore Cards.ambushOnTheShore
