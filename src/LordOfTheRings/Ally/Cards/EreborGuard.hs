module LordOfTheRings.Ally.Cards.EreborGuard (ereborGuard) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))

newtype EreborGuard = EreborGuard AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ereborGuard :: AllyCard EreborGuard
ereborGuard = ally EreborGuard Cards.ereborGuard

-- | Response: When you play Erebor Guard from your hand, discard the top 2 cards of your
-- deck to reduce its cost by 2. Asked while it is paid for.
instance HasModifiersFor EreborGuard where
  getModifiersFor (EreborGuard a) = when (a.placement == Unplaced) do
    deck <- (.deck) <$> getPlayer a.owner
    when (length deck >= 2) $ modified a (CardIdTarget a.allyCardId) [ReducedByDiscardingFromDeck 2]

instance RunMessage EreborGuard where
  runMessage msg (EreborGuard a) = EreborGuard <$> runMessage msg a
