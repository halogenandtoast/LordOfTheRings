module LordOfTheRings.Quest.Cards.FullSailAhead (fullSailAhead) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype FullSailAhead = FullSailAhead QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fullSailAhead :: QuestCard FullSailAhead
fullSailAhead = quest FullSailAhead Cards.fullSailAhead

-- | Sailing. No more than 8 progress can be placed here each round. If you are off-course,
-- no more than 4 progress can be placed here each round instead.
-- Winning as this stage is defeated with the Stormcaller at stage 2 is the scenario's.
instance HasModifiersFor FullSailAhead where
  getModifiersFor (FullSailAhead a) = when a.flipped do
    offCourse <- isOffCourse
    modified a a [ProgressPerRoundLimit $ if offCourse then 4 else 8]

-- | When Revealed: Search the encounter deck and discard pile for 1 copy of Swift Raider and
-- add it to the staging area. Shuffle the encounter deck.
instance RunMessage FullSailAhead where
  runMessage msg q@(FullSailAhead attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      FullSailAhead <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Enemies.swiftRaider)
      pure q
    _ -> FullSailAhead <$> runMessage msg attrs
