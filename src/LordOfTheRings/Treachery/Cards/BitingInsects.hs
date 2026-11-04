module LordOfTheRings.Treachery.Cards.BitingInsects (bitingInsects) where

import LordOfTheRings.Player.Types (discardableHand)
import LordOfTheRings.Scenario.Scenarios.TheMumakil.Helpers (printedCost)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype BitingInsects = BitingInsects TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bitingInsects :: TreacheryCard BitingInsects
bitingInsects = treachery BitingInsects Cards.bitingInsects

-- | When Revealed: Each player discards a random card from his hand and assigns X damage
-- among characters he controls. X is the discarded card's cost.
-- Shadow: Discard a random card from your hand and assign X damage among characters you
-- control. X is the discarded card's cost.
instance RunMessage BitingInsects where
  runMessage msg t@(BitingInsects attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer bite
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      bite pid
      pure t
    _ -> BitingInsects <$> runMessage msg attrs
   where
    bite pid = do
      hand <- discardableHand <$> getPlayer pid
      picked <- take 1 <$> shuffleM hand
      for_ picked \card -> do
        discardFromHand pid card
        let x = printedCost card
        when (x > 0) $ dealDamageDivided attrs pid x
