module LordOfTheRings.Treachery.Cards.Nightfall (nightfall) where

import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Nightfall = Nightfall TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

nightfall :: TreacheryCard Nightfall
nightfall = treachery Nightfall Cards.nightfall

-- | When Revealed: The first player makes an escape test, dealing 2 cards from the
-- encounter deck. If this test is failed, place 1 resource token on Gollum and raise each
-- player's threat by 2.
instance RunMessage Nightfall where
  runMessage msg t@(Nightfall attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      escapeTest lead attrs 2 EscapeWillpower (placeOnGollum attrs 1 >> eachPlayer (`encounterRaisesThreat` 2)) (pure ())
      pure t
    _ -> Nightfall <$> runMessage msg attrs
