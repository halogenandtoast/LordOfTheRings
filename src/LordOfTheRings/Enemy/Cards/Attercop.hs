module LordOfTheRings.Enemy.Cards.Attercop (attercop) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FliesAndSpiders.Helpers (poisonOn, poisonedCharacters)

newtype Attercop = Attercop EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

attercop :: EnemyCard Attercop
attercop = enemy Attercop Cards.attercop

instance HasModifiersFor Attercop where
  getModifiersFor (Attercop a) = do
    poisoned <- poisonedCharacters
    total <- sum <$> traverse poisonOn poisoned
    modifySelf a [EngagementCostModifier (-total)]
    for_ poisoned \c -> modified a c [CannotFight (EnemyWithId a.id)]

instance RunMessage Attercop where
  runMessage msg (Attercop a) = Attercop <$> runMessage msg a
