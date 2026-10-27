module LordOfTheRings.Enemy.Cards.StrayGoblin (strayGoblin) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype StrayGoblin = StrayGoblin EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

strayGoblin :: EnemyCard StrayGoblin
strayGoblin = enemy StrayGoblin Cards.strayGoblin

-- | X is the number of players in the game.
instance HasModifiersFor StrayGoblin where
  getModifiersFor (StrayGoblin a) = do
    players <- length <$> getRemainingPlayers
    modifySelf a [ThreatModifier players, AttackModifier players]

-- | Shadow: attacking enemy gets +X attack. X is the number of players in the game.
instance RunMessage StrayGoblin where
  runMessage msg e@(StrayGoblin attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      players <- length <$> getRemainingPlayers
      enemyGetsAttack attrs eid players
      pure e
    _ -> StrayGoblin <$> runMessage msg attrs
