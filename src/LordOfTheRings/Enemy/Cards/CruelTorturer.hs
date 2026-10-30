module LordOfTheRings.Enemy.Cards.CruelTorturer (cruelTorturer) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CruelTorturer = CruelTorturer EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Capture 1, which the scenario resolves.
cruelTorturer :: EnemyCard CruelTorturer
cruelTorturer = enemy CruelTorturer Cards.cruelTorturer

-- | Forced: When Cruel Torturer would damage a character by an attack, capture that
-- character underneath Cruel Torturer instead.
instance HasModifiersFor CruelTorturer where
  getModifiersFor (CruelTorturer a) = modified a a [CaptureInsteadOfAttackDamage]

instance RunMessage CruelTorturer where
  runMessage msg (CruelTorturer attrs) = CruelTorturer <$> runMessage msg attrs
