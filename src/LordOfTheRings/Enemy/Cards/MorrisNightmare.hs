module LordOfTheRings.Enemy.Cards.MorrisNightmare (morrisNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MorrisNightmare = MorrisNightmare EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

morrisNightmare :: EnemyCard MorrisNightmare
morrisNightmare = enemy MorrisNightmare Cards.morrisNightmare

-- | Immune to player events.
-- While Morris is in the victory display, each Troll enemy gets +1 attack and gains "Immune
-- to player events." (Conflict at the Carrock reads the victory display.)
instance HasModifiersFor MorrisNightmare where
  getModifiersFor (MorrisNightmare a) = modifySelf a [ImmuneToPlayerEvents]

instance RunMessage MorrisNightmare where
  runMessage msg (MorrisNightmare attrs) = MorrisNightmare <$> runMessage msg attrs
