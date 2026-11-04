module LordOfTheRings.Treachery.Cards.TheWayIsShut (theWayIsShut) where

import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (attachOvercomeByFearOr)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheWayIsShut = TheWayIsShut TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Peril. Surge.
theWayIsShut :: TreacheryCard TheWayIsShut
theWayIsShut = treachery TheWayIsShut Cards.theWayIsShut

-- | When Revealed: Either attach a set aside Overcome by Fear to your threat dial, or progress
-- cannot be placed on the current quest until the end of the round.
instance RunMessage TheWayIsShut where
  runMessage msg t@(TheWayIsShut attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      revealer <- getRevealer
      current <- getCurrentQuest
      attachOvercomeByFearOr attrs revealer "theWayIsShut" True
        $ for_ current \q -> roundModifier attrs q (ProgressLimit 0)
      pure t
    _ -> TheWayIsShut <$> runMessage msg attrs
