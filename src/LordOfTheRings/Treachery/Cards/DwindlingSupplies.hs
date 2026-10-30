module LordOfTheRings.Treachery.Cards.DwindlingSupplies (dwindlingSupplies) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DwindlingSupplies = DwindlingSupplies TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dwindlingSupplies :: TreacheryCard DwindlingSupplies
dwindlingSupplies = treachery DwindlingSupplies Cards.dwindlingSupplies

-- | When Revealed: Each player must randomly discard cards from his hand until he has no
-- more than 3 cards in his hand. If no cards were discarded by this effect, Dwindling
-- Supplies gains doomed 2.
-- Shadow: Defending character gets -2 defense for this attack.
instance RunMessage DwindlingSupplies where
  runMessage msg t@(DwindlingSupplies attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      excess <- for players \p -> (p,) . subtract 3 . length . (.hand) <$> getPlayer p
      for_ [(p, n) | (p, n) <- excess, n > 0] \(p, n) -> discardRandomFromHand p n
      when (all ((<= 0) . snd) excess) $ raiseThreatFromDoomed 2
      pure t
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> attackModifier (CardIdSource cid) d (DefenseModifier (-2))
      pure t
    _ -> DwindlingSupplies <$> runMessage msg attrs
