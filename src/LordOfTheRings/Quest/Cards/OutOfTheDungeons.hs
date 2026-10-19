module LordOfTheRings.Quest.Cards.OutOfTheDungeons (outOfTheDungeons) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype OutOfTheDungeons = OutOfTheDungeons QuestAttrs
  deriving anyclass (IsQuest, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

outOfTheDungeons :: QuestCard OutOfTheDungeons
outOfTheDungeons = quest OutOfTheDungeons Cards.outOfTheDungeons
