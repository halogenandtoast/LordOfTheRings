module LordOfTheRings.Treachery.Cards.UnwelcomeDiscovery (unwelcomeDiscovery) where

import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (revealUnderworldCard)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnwelcomeDiscovery = UnwelcomeDiscovery TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

unwelcomeDiscovery :: TreacheryCard UnwelcomeDiscovery
unwelcomeDiscovery = treachery UnwelcomeDiscovery Cards.unwelcomeDiscovery

-- | Surge.
-- When Revealed: Reveal 1 card from the underworld deck and add it to the staging area. If
-- it is a Clue card, discard it instead.
instance RunMessage UnwelcomeDiscovery where
  runMessage msg t@(UnwelcomeDiscovery attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealUnderworldCard
      pure t
    _ -> UnwelcomeDiscovery <$> runMessage msg attrs
