module LordOfTheRings.Treachery.Cards.TheLightsOfTheDead (theLightsOfTheDead) where

import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes.Helpers (placeOnGollum)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheLightsOfTheDead = TheLightsOfTheDead TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLightsOfTheDead :: TreacheryCard TheLightsOfTheDead
theLightsOfTheDead = treachery TheLightsOfTheDead Cards.theLightsOfTheDead

-- | When Revealed: Each player must make an escape test, dealing 2 cards from the
-- encounter deck for each test. Each player that fails this test places 1 resource token
-- on Gollum, and raises his threat by 1.
instance RunMessage TheLightsOfTheDead where
  runMessage msg t@(TheLightsOfTheDead attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> escapeTest p attrs 2 EscapeWillpower (placeOnGollum attrs 1 >> encounterRaisesThreat p 1) (pure ())
      pure t
    _ -> TheLightsOfTheDead <$> runMessage msg attrs
