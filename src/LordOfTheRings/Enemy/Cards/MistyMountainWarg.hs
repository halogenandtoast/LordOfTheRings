module LordOfTheRings.Enemy.Cards.MistyMountainWarg (mistyMountainWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth.Helpers (
  damageAnActiveLocation,
  damageEachActiveLocation,
 )

newtype MistyMountainWarg = MistyMountainWarg EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mistyMountainWarg :: EnemyCard MistyMountainWarg
mistyMountainWarg = enemy MistyMountainWarg Cards.mistyMountainWarg

instance RunMessage MistyMountainWarg where
  runMessage msg e@(MistyMountainWarg a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      you <- getRevealer
      attackModifier a a.id AttackMadeByOwnAbility
      immediateAttack you a.id
      pure e
    -- The damage belongs only to the immediate attack made by this revelation.
    CheckWindows ws | any (\case Window After (EnemyAttacking eid _) -> eid == a.id; _ -> False) ws -> do
      own <- elem AttackMadeByOwnAbility <$> getModifiers a
      when own $ whenM (attackDestroyedACharacter a.id) $ damageEachActiveLocation a 2
      MistyMountainWarg <$> runMessage msg a
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure e
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      damageAnActiveLocation a 1
      pure e
    _ -> MistyMountainWarg <$> runMessage msg a
