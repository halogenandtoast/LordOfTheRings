module LordOfTheRings.Treachery.Cards.NightWanesSlow (nightWanesSlow) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NightWanesSlow = NightWanesSlow TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

nightWanesSlow :: TreacheryCard NightWanesSlow
nightWanesSlow = treachery NightWanesSlow Cards.nightWanesSlow

-- | The out-of-play Forced effect is handled by the scenario: it interrupts the quest's
-- one-at-a-time discards, even though no instance of this card is in play to hear them.
instance RunMessage NightWanesSlow where
  runMessage msg t@(NightWanesSlow attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      shuffleEncounterDiscardIntoDeck AnyCard
      revealEncounterCard
      pure t
    _ -> NightWanesSlow <$> runMessage msg attrs
