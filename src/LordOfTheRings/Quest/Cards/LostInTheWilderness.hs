module LordOfTheRings.Quest.Cards.LostInTheWilderness (lostInTheWilderness) where

import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype LostInTheWilderness = LostInTheWilderness QuestAttrs
  deriving anyclass (IsQuest, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Encounter side quest.
lostInTheWilderness :: QuestCard LostInTheWilderness
lostInTheWilderness = quest LostInTheWilderness Cards.lostInTheWilderness

-- | When Revealed: Each player places all cards in his hand facedown underneath Lost in the
-- Wilderness.
-- Forced: When Lost in the Wilderness is defeated, return each card underneath to its
-- owner's hand.
instance RunMessage LostInTheWilderness where
  runMessage msg q@(LostInTheWilderness attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        hand <- (.hand) <$> getPlayer p
        unless (null hand) $ placeUnderneath attrs hand
      pure q
    CompleteQuest qid | qid == attrs.id -> do
      for_ [pc | PlayerCard pc <- attrs.underneath] putInHand
      LostInTheWilderness <$> runMessage msg attrs
    _ -> LostInTheWilderness <$> runMessage msg attrs
