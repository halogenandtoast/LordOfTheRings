module LordOfTheRings.Enemy.Cards.MarshAdder (marshAdder) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MarshAdder = MarshAdder EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

marshAdder :: EnemyCard MarshAdder
marshAdder = enemy MarshAdder Cards.marshAdder

-- | Forced: Each time Marsh Adder attacks you, raise your threat by 1.
instance HasAbilities MarshAdder where
  getAbilities (MarshAdder a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage MarshAdder where
  runMessage msg e@(MarshAdder attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ (headMay [pid | Window _ (EnemyAttacking _ pid) <- windows]) \pid -> raiseThreat pid 1
      pure e
    _ -> MarshAdder <$> runMessage msg attrs
