module LordOfTheRings.Enemy.Cards.SavageHillman (savageHillman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))

newtype SavageHillman = SavageHillman EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

savageHillman :: EnemyCard SavageHillman
savageHillman = enemy SavageHillman Cards.savageHillman

-- | Surge.
-- While the engaged player has 3 or more cards in hand, Savage Hillman cannot take damage.
-- While the engaged player has 5 or more cards in hand, Savage Hillman gets +2 Attack.
instance HasModifiersFor SavageHillman where
  getModifiersFor (SavageHillman a) = case a.placement of
    EngagedWith pid -> do
      n <- length . (.hand) <$> getPlayer pid
      modified a a $ [CannotTakeDamage | n >= 3] <> [AttackModifier 2 | n >= 5]
    _ -> pure ()

instance RunMessage SavageHillman where
  runMessage msg (SavageHillman attrs) = SavageHillman <$> runMessage msg attrs
