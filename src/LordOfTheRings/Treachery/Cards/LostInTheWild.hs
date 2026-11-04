module LordOfTheRings.Treachery.Cards.LostInTheWild (lostInTheWild) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype LostInTheWild = LostInTheWild TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

lostInTheWild :: TreacheryCard LostInTheWild
lostInTheWild = treachery LostInTheWild Cards.lostInTheWild

{- | When Revealed: The player with the most cards in his hand without a copy of Lost in the
Wild adds Lost in the Wild to his hand, the first player choosing among those tied (FAQ
1.03). (While in a player's hand, Lost in the Wild gains: "Cannot be discarded by player
card effect. Forced: After you play a card, discard each card in your hand.")
Player card effects already skip an encounter card in hand; in hand, its card answers the
windows through a temporary instance of it.
-}
instance RunMessage LostInTheWild where
  runMessage msg t@(LostInTheWild attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      hands <- for players \p -> (p,) . (.hand) <$> getPlayer p
      let eligible = [(p, length hand) | (p, hand) <- hands, attrs.treacheryCardCode `notElem` map toCardCode hand]
          most = foldr (max . snd) 0 eligible
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "lostInTheWild.choosePlayer"
        causedBy attrs
        targets [p | (p, n) <- eligible, n == most] \p -> drawTreacheryIntoHand p attrs.id
      pure t
    CheckWindows ws -> do
      holder <- handHolding attrs.cardId
      for_ holder \p ->
        when (notNull [() | Window After (PlayedCard p' _) <- ws, p' == p]) $ discardEachCardInHand p
      LostInTheWild <$> runMessage msg attrs
    _ -> LostInTheWild <$> runMessage msg attrs
