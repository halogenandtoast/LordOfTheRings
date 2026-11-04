module LordOfTheRings.Enemy.Cards.StuartNightmare (stuartNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype StuartNightmare = StuartNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

stuartNightmare :: EnemyCard StuartNightmare
stuartNightmare = enemy StuartNightmare Cards.stuartNightmare

-- | Cannot have player attachments.
-- While Stuart is in the victory display, each Troll enemy gets +1 defense and gains
-- "Cannot have player attachments." (Conflict at the Carrock reads the victory display.)
instance HasModifiersFor StuartNightmare where
  getModifiersFor (StuartNightmare a) = modifySelf a [CannotHavePlayerAttachments]

instance RunMessage StuartNightmare where
  runMessage msg (StuartNightmare attrs) = StuartNightmare <$> runMessage msg attrs
