module LordOfTheRings.Enemy.Cards.NamelessCaveDweller (namelessCaveDweller) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheRuinsOfBelegost.Helpers (playerControlsLoot)

newtype NamelessCaveDweller = NamelessCaveDweller EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

namelessCaveDweller :: EnemyCard NamelessCaveDweller
namelessCaveDweller = enemy NamelessCaveDweller Cards.namelessCaveDweller

instance HasModifiersFor NamelessCaveDweller where
  getModifiersFor (NamelessCaveDweller a) =
    modifySelf a [CannotHavePlayerAttachments, StatsFromActiveDiscover 1]

instance RunMessage NamelessCaveDweller where
  runMessage msg e@(NamelessCaveDweller a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      ownsLoot <- playerControlsLoot pid
      enemyAttackModifier a eid $ AttackModifier (if ownsLoot then 2 else 1)
      pure e
    _ -> NamelessCaveDweller <$> runMessage msg a
