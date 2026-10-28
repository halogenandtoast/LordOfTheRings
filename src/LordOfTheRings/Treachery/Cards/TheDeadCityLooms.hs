module LordOfTheRings.Treachery.Cards.TheDeadCityLooms (theDeadCityLooms) where

import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (towerProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheDeadCityLooms = TheDeadCityLooms TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDeadCityLooms :: TreacheryCard TheDeadCityLooms
theDeadCityLooms = treachery TheDeadCityLooms Cards.theDeadCityLooms

-- | When Revealed: Raise each players' threat by the number of progress tokens on To the
-- Tower. If there are no progress tokens on To the Tower, The Dead City Looms gains Surge.
-- Shadow: Attacking enemy gets +1 attack for each progress token on To the Tower.
instance RunMessage TheDeadCityLooms where
  runMessage msg t@(TheDeadCityLooms attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      progress <- towerProgress
      if progress == 0
        then revealEncounterCardAfter (toTarget attrs)
        else eachPlayer \p -> encounterRaisesThreat p progress
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid =<< towerProgress
      pure t
    _ -> TheDeadCityLooms <$> runMessage msg attrs
