module LordOfTheRings.Enemy.Cards.SnowWarg (snowWarg) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype SnowWarg = SnowWarg EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

snowWarg :: EnemyCard SnowWarg
snowWarg = enemy SnowWarg Cards.snowWarg

-- | Allies cannot defend while Snow Warg is attacking.
instance HasModifiersFor SnowWarg where
  getModifiersFor (SnowWarg a) = modified a a [CannotBeDefendedByAllies]

-- | Forced: After a character is declared as a defender against Snow Warg, deal 1 damage
-- to the defending character, if able.
instance HasAbilities SnowWarg where
  getAbilities (SnowWarg a) = [mkAbility a 1 $ forced $ DefenderDeclaredAgainstWindow After a.id]

instance RunMessage SnowWarg where
  runMessage msg e@(SnowWarg attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [d | Window _ (DeclaredAsDefender d _) <- ws] \d -> dealDamage attrs d 1
      pure e
    _ -> SnowWarg <$> runMessage msg attrs
