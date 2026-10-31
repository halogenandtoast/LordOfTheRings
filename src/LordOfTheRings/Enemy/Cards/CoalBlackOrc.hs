module LordOfTheRings.Enemy.Cards.CoalBlackOrc (coalBlackOrc) where

import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CoalBlackOrc = CoalBlackOrc EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

coalBlackOrc :: EnemyCard CoalBlackOrc
coalBlackOrc = enemy CoalBlackOrc Cards.coalBlackOrc

victoryPoints :: HasGame m => m Int
victoryPoints = getsGame ((.victoryPoints) . toAttrs . (.scenario))

-- | X is the number of victory points in the victory display: its threat and attack.
instance HasModifiersFor CoalBlackOrc where
  getModifiersFor (CoalBlackOrc a) = do
    x <- victoryPoints
    when (x > 0) $ modifySelf a [ThreatModifier x, AttackModifier x]

-- | Shadow: Attacking enemy gets +X attack. X is the number of points in the victory
-- display.
instance RunMessage CoalBlackOrc where
  runMessage msg e@(CoalBlackOrc attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- victoryPoints
      when (x > 0) $ enemyGetsAttack attrs eid x
      pure e
    _ -> CoalBlackOrc <$> runMessage msg attrs
