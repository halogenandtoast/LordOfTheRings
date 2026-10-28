module LordOfTheRings.Quest.Cards.BreakthroughAtTheCitadel (breakthroughAtTheCitadel) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype BreakthroughAtTheCitadel = BreakthroughAtTheCitadel QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

breakthroughAtTheCitadel :: QuestCard BreakthroughAtTheCitadel
breakthroughAtTheCitadel = quest BreakthroughAtTheCitadel Cards.breakthroughAtTheCitadel

-- | Siege.
-- Breakthrough at the Citadel adds 5 threat to the staging area.
instance HasModifiersFor BreakthroughAtTheCitadel where
  getModifiersFor (BreakthroughAtTheCitadel a) = modified a GameTarget [StagingThreatBonus 5]
