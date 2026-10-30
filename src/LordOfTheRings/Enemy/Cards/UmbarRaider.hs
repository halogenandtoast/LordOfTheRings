module LordOfTheRings.Enemy.Cards.UmbarRaider (umbarRaider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype UmbarRaider = UmbarRaider EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarRaider :: EnemyCard UmbarRaider
umbarRaider = enemy UmbarRaider Cards.umbarRaider

-- | Umbar Raider gets +1 attack for each resource on it.
instance HasModifiersFor UmbarRaider where
  getModifiersFor (UmbarRaider a) = do
    let n = countTokens Resource a.enemyTokens
    when (n > 0) $ modified a a [AttackModifier n]

-- | Forced: After Umbar Raider attacks you, move 1 resource from one of your heroes'
-- resource pools to Umbar Raider.
instance HasAbilities UmbarRaider where
  getAbilities (UmbarRaider a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage UmbarRaider where
  runMessage msg e@(UmbarRaider attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] (takeResourceFromAHero attrs)
      pure e
    _ -> UmbarRaider <$> runMessage msg attrs
