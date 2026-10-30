module LordOfTheRings.Ally.Cards.ElvenJeweler (elvenJeweler) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Card (Card (PlayerCard))
import LordOfTheRings.Player.Types (discardableHand)

newtype ElvenJeweler = ElvenJeweler AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Action: Discard 2 cards from your hand to put Elven Jeweler into play from your hand,
-- under your control. Offered with 2 other cards in hand to discard.
elvenJeweler :: AllyCard ElvenJeweler
elvenJeweler = ally (\a -> ElvenJeweler a {allyHandAction = Just $ PlayerExists (ActivePlayer <> PlayerWithCardsInHand 3)}) Cards.elvenJeweler

instance RunMessage ElvenJeweler where
  runMessage msg s@(ElvenJeweler a) = case msg of
    UseHandAction pid cid | cid == a.allyCardId -> do
      player <- getPlayer pid
      let discardSome :: ReverseQueue n => Int -> [Card] -> n ()
          discardSome n cards = when (n > 0) $ chooseOneM pid do
            questionLabeled "chooseCardToDiscardFromHand"
            causedBy (CardIdTarget cid)
            targets cards \c -> do
              discardFromHand pid c
              discardSome (n - 1) (filter ((/= c.id) . (.id)) cards)
      discardSome 2 [c | c <- discardableHand player, c.id /= cid]
      for_ [pc | PlayerCard pc <- player.hand, pc.id == cid] (putAllyIntoPlayUnder pid)
      pure s
    _ -> ElvenJeweler <$> runMessage msg a
