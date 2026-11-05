module LordOfTheRings.Treachery.Cards.PoisonedCounselsTheTreasonOfSaruman (poisonedCounselsTheTreasonOfSaruman) where

import LordOfTheRings.Card (Card (EncounterCard, PlayerCard))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PoisonedCounselsTheTreasonOfSaruman = PoisonedCounselsTheTreasonOfSaruman TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | A burden with a player card back, shuffled into a player's deck. Poisoned Counsels
functions like a player card: drawn, it stays in its player's hand, and discarded it goes
to his discard pile (see 'FunctionsLikeAPlayerCard').
-}
poisonedCounselsTheTreasonOfSaruman :: TreacheryCard PoisonedCounselsTheTreasonOfSaruman
poisonedCounselsTheTreasonOfSaruman = treachery PoisonedCounselsTheTreasonOfSaruman Cards.poisonedCounselsTheTreasonOfSaruman

-- | Forced: After you draw this card, discard all other cards in your hand. (Its card, out
-- of play in the hand, answers the draw.)
instance RunMessage PoisonedCounselsTheTreasonOfSaruman where
  runMessage msg t@(PoisonedCounselsTheTreasonOfSaruman attrs) = case msg of
    EncounterCardDrawn pid card | card.id == attrs.cardId -> do
      player <- getPlayer pid
      for_ player.hand \case
        PlayerCard c -> push $ DiscardFromHand pid c.id
        EncounterCard ec -> when (ec.id /= card.id) $ discardEncounterCardFromHand pid ec
      pure t
    _ -> PoisonedCounselsTheTreasonOfSaruman <$> runMessage msg attrs
