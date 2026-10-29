module LordOfTheRings.Treachery.Cards.IllTidings (illTidings) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype IllTidings = IllTidings TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

illTidings :: TreacheryCard IllTidings
illTidings = treachery IllTidings Cards.illTidings

-- | When Revealed: The first player draws this card into his hand. Ill Tidings cannot
-- leave that player's hand. Then, if the first player has 5 or more cards in his hand,
-- Ill Tidings gains Surge.
-- The card is out of play once drawn, so the hand it joins is counted as it goes.
instance RunMessage IllTidings where
  runMessage msg t@(IllTidings attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      hand <- length . (.hand) <$> getPlayer lead
      drawTreacheryIntoHand lead attrs.id
      when (hand + 1 >= 5) revealEncounterCard
      pure t
    _ -> IllTidings <$> runMessage msg attrs
