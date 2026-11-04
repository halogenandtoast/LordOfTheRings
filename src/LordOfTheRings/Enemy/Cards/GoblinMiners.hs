module LordOfTheRings.Enemy.Cards.GoblinMiners (goblinMiners) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Trait (Trait (Cave))

newtype GoblinMiners = GoblinMiners EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinMiners :: EnemyCard GoblinMiners
goblinMiners = enemy GoblinMiners Cards.goblinMiners

-- | Goblin Miners gets +1 defense for each Cave location in play.
instance HasModifiersFor GoblinMiners where
  getModifiersFor (GoblinMiners a) = do
    caves <- length <$> select (LocationWithTrait Cave)
    when (caves > 0) $ modifySelf a [DefenseModifier caves]

-- | Shadow: attacking enemy gets +1 attack.
instance RunMessage GoblinMiners where
  runMessage msg e@(GoblinMiners attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      pure e
    _ -> GoblinMiners <$> runMessage msg attrs
