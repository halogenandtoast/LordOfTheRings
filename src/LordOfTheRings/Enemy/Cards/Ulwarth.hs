module LordOfTheRings.Enemy.Cards.Ulwarth (ulwarth) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Ulwarth = Ulwarth EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ulwarth :: EnemyCard Ulwarth
ulwarth = enemy Ulwarth Cards.ulwarth

-- | Immune to player card effects.
-- Only the engaged player can declare attackers or defenders against Ulwarth.
-- Allies cannot be declared as attackers or defenders against Ulwarth.
instance HasModifiersFor Ulwarth where
  getModifiersFor (Ulwarth a) = modifySelf a [ImmuneToPlayerCardEffects, OnlyEngagedPlayerFights, NotFoughtByAllies]

instance RunMessage Ulwarth where
  runMessage msg (Ulwarth attrs) = Ulwarth <$> runMessage msg attrs
