module LordOfTheRings.Ally.Cards.EreborGuard (ereborGuard) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))

newtype EreborGuard = EreborGuard AllyAttrs
  deriving anyclass (IsAlly)
  deriving newtype (Show, Eq, ToJSON, Entity)

ereborGuard :: AllyCard EreborGuard
ereborGuard = ally EreborGuard Cards.ereborGuard

-- | Response: When you play Erebor Guard from your hand, discard the top 2 cards of your
-- deck to reduce its cost by 2.
--
-- ponytail: always taken when the deck has 2 cards to discard; ask while paying once a
-- cost can be chosen there
instance HasModifiersFor EreborGuard where
  getModifiersFor (EreborGuard a) = when (a.placement == Unplaced) do
    deck <- (.deck) <$> getPlayer a.owner
    when (length deck >= 2) $ modified a (CardIdTarget a.allyCardId) [CostModifier (-2)]

instance HasAbilities EreborGuard where
  getAbilities (EreborGuard a) = [mkAbility a 1 $ forced $ PlayedFromHandWindow After (CardIdTarget a.allyCardId)]

instance RunMessage EreborGuard where
  runMessage msg s@(EreborGuard a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discardTopOfDeck pid 2
      pure s
    _ -> EreborGuard <$> runMessage msg a
