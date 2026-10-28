module LordOfTheRings.Enemy.Cards.DruBuriDru (druBuriDru) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype DruBuriDru = DruBuriDru EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

druBuriDru :: EnemyCard DruBuriDru
druBuriDru = enemy DruBuriDru Cards.druBuriDru

-- | Allies cannot defend against Drû-buri-Drû.
-- While Drû-buri-Drû is in the victory display, characters get +1 willpower and +1
-- defense. Unless Drû-buri-Drû is in the victory display, the players cannot win: The
-- Passage Out holds both, as he enters play at that stage, the last, and reaches the
-- victory display only by its effect.
instance HasModifiersFor DruBuriDru where
  getModifiersFor (DruBuriDru a) = modified a a [CannotBeDefendedByAllies]

instance RunMessage DruBuriDru where
  runMessage msg (DruBuriDru attrs) = DruBuriDru <$> runMessage msg attrs
