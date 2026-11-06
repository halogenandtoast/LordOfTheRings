module LordOfTheRings.Treachery.Cards.TheGloaming (theGloaming) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (currentNightObjective)

newtype TheGloaming = TheGloaming TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theGloaming :: TreacheryCard TheGloaming
theGloaming = treachery TheGloaming Cards.theGloaming

instance RunMessage TheGloaming where
  runMessage msg t@(TheGloaming a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      traverse_ (\oid -> placeProgress a oid 1) =<< currentNightObjective
      pure t
    _ -> TheGloaming <$> runMessage msg a
