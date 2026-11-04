module LordOfTheRings.Treachery.Cards.DeterminedDefense (determinedDefense) where

import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn.Helpers (castlesInVictoryDisplay)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DeterminedDefense = DeterminedDefense TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

determinedDefense :: TreacheryCard DeterminedDefense
determinedDefense = treachery DeterminedDefense Cards.determinedDefense

-- | When Revealed: Until the end of the phase, add 2 [threat] to the staging area. Add 2
-- additional [threat] for each Castle side quest in the victory display.
-- Shadow: Exhaust a character you control.
instance RunMessage DeterminedDefense where
  runMessage msg t@(DeterminedDefense attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      n <- castlesInVictoryDisplay
      phaseModifier attrs GameTarget (StagingThreatBonus (2 + 2 * n))
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      chooseAndExhaust attrs pid 1
      pure t
    _ -> DeterminedDefense <$> runMessage msg attrs
