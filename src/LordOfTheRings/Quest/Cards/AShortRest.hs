module LordOfTheRings.Quest.Cards.AShortRest (aShortRest) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AShortRest = AShortRest QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Setup: Shuffle the Over the Misty Mountains Grim and Western Lands encounter sets into
one encounter deck and make it the active encounter deck. Then, shuffle The Great Goblin
and Misty Mountain Goblins encounter sets into a second encounter deck and set it aside,
inactive. The scenario builds both decks as it gathers the encounter sets.
-}
aShortRest :: QuestCard AShortRest
aShortRest = quest AShortRest Cards.aShortRest

-- | When Revealed: Each player may search his deck for 1 treasure card and add it to his
-- hand, then shuffle his deck. Advance to stage 2A.
instance RunMessage AShortRest where
  runMessage msg q@(AShortRest attrs) = case msg of
    DoStep 7 Setup -> do
      askToPerformSetup attrs
      pure q
    Flip (isTarget attrs -> True) | not attrs.flipped -> do
      askToResolveWhenRevealed attrs
      AShortRest <$> runMessage msg attrs
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> searchDeck p attrs maxBound TreasureCard False
      advanceQuest
      pure q
    _ -> AShortRest <$> runMessage msg attrs
