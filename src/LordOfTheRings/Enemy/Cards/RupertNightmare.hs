module LordOfTheRings.Enemy.Cards.RupertNightmare (rupertNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RupertNightmare = RupertNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

rupertNightmare :: EnemyCard RupertNightmare
rupertNightmare = enemy RupertNightmare Cards.rupertNightmare

-- | Rupert's defense cannot be reduced.
-- While Rupert is in the victory display, the defense of each Troll enemy cannot be
-- reduced. (Conflict at the Carrock reads the victory display.)
instance HasModifiersFor RupertNightmare where
  getModifiersFor (RupertNightmare a) = modifySelf a [CannotHaveDefenseReduced]

instance RunMessage RupertNightmare where
  runMessage msg (RupertNightmare attrs) = RupertNightmare <$> runMessage msg attrs
