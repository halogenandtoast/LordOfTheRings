module LordOfTheRings.Enemy.Cards.GoblinTrapper (goblinTrapper) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinTrapper = GoblinTrapper EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinTrapper :: EnemyCard GoblinTrapper
goblinTrapper = enemy GoblinTrapper Cards.goblinTrapper

-- | Forced: After Goblin Trapper engages a player, that player must exhaust a character
-- he controls.
instance HasAbilities GoblinTrapper where
  getAbilities (GoblinTrapper a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

-- | Shadow: Add Goblin Trapper to the staging area.
instance RunMessage GoblinTrapper where
  runMessage msg e@(GoblinTrapper attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> chooseAndExhaust attrs pid 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      putShadowEnemyIntoStaging eid cid
      pure e
    _ -> GoblinTrapper <$> runMessage msg attrs
