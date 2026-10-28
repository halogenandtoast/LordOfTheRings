module LordOfTheRings.Enemy.Cards.NazgulOfMinasMorgul (nazgulOfMinasMorgul) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype NazgulOfMinasMorgul = NazgulOfMinasMorgul EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

nazgulOfMinasMorgul :: EnemyCard NazgulOfMinasMorgul
nazgulOfMinasMorgul = enemy NazgulOfMinasMorgul Cards.nazgulOfMinasMorgul

-- | Cannot have attachments. Reduce any amount of damage dealt to Nazgûl of Minas Morgul
-- to 1.
instance HasModifiersFor NazgulOfMinasMorgul where
  getModifiersFor (NazgulOfMinasMorgul a) = modified a a [CannotHaveAttachments, DamageReducedTo 1]

instance RunMessage NazgulOfMinasMorgul where
  runMessage msg (NazgulOfMinasMorgul attrs) = NazgulOfMinasMorgul <$> runMessage msg attrs
