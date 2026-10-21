module LordOfTheRings.Treachery.Cards.ThroughTheMist (throughTheMist) where

import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ThroughTheMist = ThroughTheMist TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

throughTheMist :: TreacheryCard ThroughTheMist
throughTheMist = treachery ThroughTheMist Cards.throughTheMist

-- | When Revealed: The first player makes an escape test counting attack instead of
-- willpower, dealing 2 cards from the encounter deck. If this test is failed, place 1
-- resource token on Gollum and raise each player's threat by 1.
instance RunMessage ThroughTheMist where
  runMessage msg t@(ThroughTheMist attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      escapeTest lead attrs 2 EscapeAttack (placeOnGollum attrs 1 >> eachPlayer (`raiseThreat` 1)) (pure ())
      pure t
    _ -> ThroughTheMist <$> runMessage msg attrs
