module LordOfTheRings.Treachery.Cards.NoTurningBack (noTurningBack) where

import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (attachOvercomeByFearOr, attachOvercomeByFearOrDiscardAlly)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NoTurningBack = NoTurningBack TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

noTurningBack :: TreacheryCard NoTurningBack
noTurningBack = treachery NoTurningBack Cards.noTurningBack

-- | When Revealed: Each player must choose: either attach a set aside Overcome by Fear to
-- your threat dial, or discard an ally you control.
-- Shadow: Either attach a set aside Overcome by Fear to your threat dial, or attacking enemy
-- makes an additional attack after this one.
--
-- The players choose one after another, in turn order, each seeing the copies the ones
-- before him left set aside.
instance RunMessage NoTurningBack where
  runMessage msg t@(NoTurningBack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      doStep 1 msg
      pure t
    DoStep n (WhenRevealed (isTarget attrs -> True)) -> do
      players <- getRemainingPlayers
      for_ (atMay players (n - 1)) \p -> do
        attachOvercomeByFearOrDiscardAlly attrs p "noTurningBack"
        doStep (n + 1) (WhenRevealed $ toTarget attrs)
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachOvercomeByFearOr attrs pid "noTurningBackShadow" True $ doStep 1 msg
      pure t
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == attrs.cardId -> do
      additionalAttack pid eid
      pure t
    _ -> NoTurningBack <$> runMessage msg attrs
