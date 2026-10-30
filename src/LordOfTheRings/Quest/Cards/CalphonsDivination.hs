module LordOfTheRings.Quest.Cards.CalphonsDivination (calphonsDivination) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Boarding))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype CalphonsDivination = CalphonsDivination QuestAttrs
  deriving anyclass (IsQuest, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

calphonsDivination :: QuestCard CalphonsDivination
calphonsDivination = quest CalphonsDivination Cards.calphonsDivination

-- | Sailing. While you are off-course, each Ship enemy gains Boarding 1.
instance HasModifiersFor CalphonsDivination where
  getModifiersFor (CalphonsDivination a) = when a.flipped do
    whenM isOffCourse do
      ships <- select shipEnemy
      for_ ships \s -> modified a s [GainKeyword (Boarding 1)]

-- | When Revealed: Search the encounter deck and discard pile for 1 copy of Scouting Ship
-- and add it to the staging area. Shuffle the encounter deck.
instance RunMessage CalphonsDivination where
  runMessage msg q@(CalphonsDivination attrs) = case msg of
    Flip (isTarget attrs -> True) -> do
      askToResolveWhenRevealed attrs
      CalphonsDivination <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Enemies.scoutingShip)
      pure q
    _ -> CalphonsDivination <$> runMessage msg attrs
