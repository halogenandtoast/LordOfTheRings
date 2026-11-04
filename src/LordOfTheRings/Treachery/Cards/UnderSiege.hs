module LordOfTheRings.Treachery.Cards.UnderSiege (underSiege) where

import LordOfTheRings.Helpers.ControlledLocation (returnHighestThreatLocation)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UnderSiege = UnderSiege TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

underSiege :: TreacheryCard UnderSiege
underSiege = treachery UnderSiege Cards.underSiege

-- | When Revealed: Until the end of the phase, add X threat to the staging area, where X is
-- 1 more than the number of locations the players control, counted as it is revealed.
-- Shadow: Attacking enemy gets +1 attack. If this attack destroys a character, return the
-- highest threat location you control to the staging area.
instance RunMessage UnderSiege where
  runMessage msg t@(UnderSiege attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- (+ 1) . length <$> select (LocationControlledBy AnyPlayer)
      phaseModifier attrs GameTarget (StagingThreatBonus x)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == attrs.cardId -> do
      returnHighestThreatLocation attrs pid
      pure t
    _ -> UnderSiege <$> runMessage msg attrs
