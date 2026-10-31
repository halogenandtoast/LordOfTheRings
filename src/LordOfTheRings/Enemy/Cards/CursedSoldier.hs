module LordOfTheRings.Enemy.Cards.CursedSoldier (cursedSoldier) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.IslandMap (activePosition)

newtype CursedSoldier = CursedSoldier EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cursedSoldier :: EnemyCard CursedSoldier
cursedSoldier = enemy CursedSoldier Cards.cursedSoldier

-- | While the active location is in the top row, Cursed Soldier gets +3 threat.
-- While the active location is in the middle row, Cursed Soldier gets +3 attack.
-- While the active location is in the bottom row, Cursed Soldier gets +3 defense.
instance HasModifiersFor CursedSoldier where
  getModifiersFor (CursedSoldier a) =
    activePosition >>= traverse_ \pos -> case pos.row of
      0 -> modifySelf a [ThreatModifier 3]
      1 -> modifySelf a [AttackModifier 3]
      2 -> modifySelf a [DefenseModifier 3]
      _ -> pure ()

instance RunMessage CursedSoldier where
  runMessage msg (CursedSoldier attrs) = CursedSoldier <$> runMessage msg attrs
