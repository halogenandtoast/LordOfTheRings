module LordOfTheRings.Enemy.Cards.GoblinAxeman (goblinAxeman) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Cave))

newtype GoblinAxeman = GoblinAxeman EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinAxeman :: EnemyCard GoblinAxeman
goblinAxeman = enemy GoblinAxeman Cards.goblinAxeman

-- | Goblin Axeman gets +1 attack for each Cave location in play.
instance HasModifiersFor GoblinAxeman where
  getModifiersFor (GoblinAxeman a) = do
    caves <- length <$> select (LocationWithTrait Cave)
    when (caves > 0) $ modifySelf a [AttackModifier caves]

-- | Shadow: Defending character gets -1 defense.
instance RunMessage GoblinAxeman where
  runMessage msg e@(GoblinAxeman attrs) = case msg of
    ResolveShadowEffect cid _ _ defenders | cid == attrs.cardId -> do
      for_ defenders \d -> attackModifier attrs d (DefenseModifier (-1))
      pure e
    _ -> GoblinAxeman <$> runMessage msg attrs
