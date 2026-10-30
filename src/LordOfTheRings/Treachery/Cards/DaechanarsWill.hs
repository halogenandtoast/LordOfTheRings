module LordOfTheRings.Treachery.Cards.DaechanarsWill (daechanarsWill) where

import LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum.Helpers (flipThaurdir, getThaurdir, thaurdirCaptain)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DaechanarsWill = DaechanarsWill TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

daechanarsWill :: TreacheryCard DaechanarsWill
daechanarsWill = treachery DaechanarsWill Cards.daechanarsWill

-- | When Revealed: Flip Thaurdir. Then, if he is Captain side faceup, Daechanar's Will
-- gains surge.
-- Shadow: Flip Thaurdir after this attack.
instance RunMessage DaechanarsWill where
  runMessage msg t@(DaechanarsWill attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      flipThaurdir
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      captain <- selectAny thaurdirCaptain
      when captain revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      getThaurdir >>= traverse_ \thaurdir -> afterAttack eid [Flip (toTarget thaurdir)]
      pure t
    _ -> DaechanarsWill <$> runMessage msg attrs
