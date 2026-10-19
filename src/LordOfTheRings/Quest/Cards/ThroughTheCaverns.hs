module LordOfTheRings.Quest.Cards.ThroughTheCaverns (throughTheCaverns) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype ThroughTheCaverns = ThroughTheCaverns QuestAttrs
  deriving anyclass (IsQuest, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheCaverns :: QuestCard ThroughTheCaverns
throughTheCaverns = quest ThroughTheCaverns Cards.throughTheCaverns
