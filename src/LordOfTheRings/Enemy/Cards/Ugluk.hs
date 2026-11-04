module LordOfTheRings.Enemy.Cards.Ugluk (ugluk) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Ugluk = Ugluk EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Toughness 3, as its card def says.
ugluk :: EnemyCard Ugluk
ugluk = enemy Ugluk Cards.ugluk

-- | Cannot have attachments.
-- Allies cannot be declared as attackers or defenders against Uglúk.
instance HasModifiersFor Ugluk where
  getModifiersFor (Ugluk a) = modifySelf a [CannotHaveAttachments, NotFoughtByAllies, CannotBeDefendedByAllies]

instance RunMessage Ugluk where
  runMessage msg (Ugluk attrs) = Ugluk <$> runMessage msg attrs
