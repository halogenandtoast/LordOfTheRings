module LordOfTheRings.Treachery.Cards.BrutalAssault (brutalAssault) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BrutalAssault = BrutalAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

brutalAssault :: TreacheryCard BrutalAssault
brutalAssault = treachery BrutalAssault Cards.brutalAssault

-- | When Revealed: Each player assigns X damage among characters he controls, where X is
-- the number of cards in his hand.
-- Shadow: Assign X damage among characters you control. X is the number of cards in your
-- hand.
instance RunMessage BrutalAssault where
  runMessage msg t@(BrutalAssault attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> assignHandSizeDamage p
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      assignHandSizeDamage pid
      pure t
    _ -> BrutalAssault <$> runMessage msg attrs
   where
    assignHandSizeDamage pid = do
      n <- length . (.hand) <$> getPlayer pid
      when (n > 0) $ dealDamageDivided attrs pid n
