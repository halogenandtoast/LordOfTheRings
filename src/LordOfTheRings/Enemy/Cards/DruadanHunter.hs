module LordOfTheRings.Enemy.Cards.DruadanHunter (druadanHunter) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Archery))

newtype DruadanHunter = DruadanHunter EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

druadanHunter :: EnemyCard DruadanHunter
druadanHunter = enemy DruadanHunter Cards.druadanHunter

-- | Prowl 1. Archery X. X is the number of heroes in play with no resources in their
-- resource pool.
instance HasModifiersFor DruadanHunter where
  getModifiersFor (DruadanHunter a) = do
    x <- length <$> select (not_ HeroWithResources)
    when (x > 0) $ modified a a [GainKeyword (Archery x)]

-- | Shadow: Attacking enemy gets +2 attack.
instance RunMessage DruadanHunter where
  runMessage msg e@(DruadanHunter attrs) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 2
      pure e
    _ -> DruadanHunter <$> runMessage msg attrs
