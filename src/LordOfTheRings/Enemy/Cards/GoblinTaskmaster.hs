module LordOfTheRings.Enemy.Cards.GoblinTaskmaster (goblinTaskmaster) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GoblinTaskmaster = GoblinTaskmaster EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

goblinTaskmaster :: EnemyCard GoblinTaskmaster
goblinTaskmaster = enemy GoblinTaskmaster Cards.goblinTaskmaster

-- | Ambush.
-- Forced: After Goblin Taskmaster engages a player, that player deals 2 damage to 1
-- character he controls.
instance HasAbilities GoblinTaskmaster where
  getAbilities (GoblinTaskmaster a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage GoblinTaskmaster where
  runMessage msg e@(GoblinTaskmaster attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> dealDamageToOneCharacter attrs pid 2
      pure e
    _ -> GoblinTaskmaster <$> runMessage msg attrs
