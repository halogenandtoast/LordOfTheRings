module LordOfTheRings.Enemy.Cards.SilvertineMarauder (silvertineMarauder) where

import LordOfTheRings.Classes.HasGame (HasGame)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)

newtype SilvertineMarauder = SilvertineMarauder EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

silvertineMarauder :: EnemyCard SilvertineMarauder
silvertineMarauder = enemy SilvertineMarauder Cards.silvertineMarauder

-- | Whether a defender has less than 2 willpower.
weakDefender :: HasGame m => [Target] -> m Bool
weakDefender = fmap (any (< 2)) . traverse getWillpower

-- | While attacking, Silvertine Marauder gets +2 [Attack] if the defending character has
-- less than 2 [Willpower].
-- Shadow: Attacking enemy gets +2 [Attack] if the defending character has less than 2
-- [Willpower].
instance RunMessage SilvertineMarauder where
  runMessage msg e@(SilvertineMarauder attrs) = case msg of
    -- checked once its shadow cards have resolved, as one may lower the defender's
    -- willpower, before the damage
    ResolveShadow eid _ (_ : _) | eid == attrs.id -> do
      doStep 1 msg
      pure e
    DoStep 1 (ResolveShadow eid _ defenders) | eid == attrs.id -> do
      weak <- weakDefender defenders
      when weak $ enemyGetsAttack attrs eid 2
      pure e
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      weak <- weakDefender defenders
      when weak $ enemyGetsAttack attrs eid 2
      pure e
    _ -> SilvertineMarauder <$> runMessage msg attrs
