module LordOfTheRings.Enemy.Cards.Wainriders (wainriders) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype Wainriders = Wainriders EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

wainriders :: EnemyCard Wainriders
wainriders = enemy Wainriders Cards.wainriders

instance HasModifiersFor Wainriders where
  getModifiersFor (Wainriders a) = modifySelf a [AttackDamageRaisesDefendingPlayerThreat]

instance RunMessage Wainriders where
  runMessage msg (Wainriders attrs) = Wainriders <$> runMessage msg attrs
