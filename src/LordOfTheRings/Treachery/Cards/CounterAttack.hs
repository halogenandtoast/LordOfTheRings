module LordOfTheRings.Treachery.Cards.CounterAttack (counterAttack) where

import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CounterAttack = CounterAttack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

counterAttack :: TreacheryCard CounterAttack
counterAttack = treachery CounterAttack Cards.counterAttack

-- | Surge.
-- When Revealed: Each player must either return the location he controls with the highest
-- threat to the staging area, or raise his threat by the total threat of all locations he
-- controls.
instance RunMessage CounterAttack where
  runMessage msg t@(CounterAttack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> do
        locations <- controlledLocations pid
        total <- sum <$> traverse getLocationThreat locations
        unless (null locations) $ chooseOneM pid do
          questionLabeled "counterAttack.choose"
          causedBy attrs
          labeled "counterAttack.return" $ returnHighestThreatLocation attrs pid
          labeledWith "counterAttack.raise" [intVar "n" total] $ encounterRaisesThreat pid total
      pure t
    _ -> CounterAttack <$> runMessage msg attrs
