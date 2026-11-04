module LordOfTheRings.Enemy.Cards.WereWorm (wereWorm) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.DesertCrossing.Helpers (getTemperature, temperatureX)

newtype WereWorm = WereWorm EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

wereWorm :: EnemyCard WereWorm
wereWorm = enemy WereWorm Cards.wereWorm

-- | X is the tens digit of the temperature: its attack and defense, printed X.
instance HasModifiersFor WereWorm where
  getModifiersFor (WereWorm a) = do
    x <- temperatureX
    when (x > 0) $ modifySelf a [AttackModifier x, DefenseModifier x]

-- | Forced: After Were-worm engages you, it makes an immediate attack. (Do not deal it a
-- shadow card.)
instance HasAbilities WereWorm where
  getAbilities (WereWorm a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if the temperature is 40 or
-- higher).
instance RunMessage WereWorm where
  runMessage msg e@(WereWorm attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> immediateAttackWithoutShadow pid attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      t <- getTemperature
      enemyGetsAttack attrs eid (if t >= 40 then 2 else 1)
      pure e
    _ -> WereWorm <$> runMessage msg attrs
