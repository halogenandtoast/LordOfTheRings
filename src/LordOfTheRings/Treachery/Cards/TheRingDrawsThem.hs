module LordOfTheRings.Treachery.Cards.TheRingDrawsThem (theRingDrawsThem) where

import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Helpers.Fellowship (theOneRingExhausted)
import LordOfTheRings.Treachery.Import.Lifted

newtype TheRingDrawsThem = TheRingDrawsThem TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the A Shadow of the Past set. Surge.
theRingDrawsThem :: TreacheryCard TheRingDrawsThem
theRingDrawsThem = treachery TheRingDrawsThem Cards.theRingDrawsThem

-- | If The One Ring is exhausted, this effect cannot be canceled: read while it is being
-- revealed.
instance HasModifiersFor TheRingDrawsThem where
  getModifiersFor (TheRingDrawsThem a) =
    when (a.placement == Unplaced) $ whenM theOneRingExhausted $ modified a GameTarget [CannotCancelEncounterEffects]

-- | When Revealed: At the end of the phase, each enemy in the staging area engages the
-- first player.
instance RunMessage TheRingDrawsThem where
  runMessage msg t@(TheRingDrawsThem attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      phaseModifier attrs GameTarget StagingEnemiesEngageFirstPlayerAtEndOfPhase
      pure t
    _ -> TheRingDrawsThem <$> runMessage msg attrs
