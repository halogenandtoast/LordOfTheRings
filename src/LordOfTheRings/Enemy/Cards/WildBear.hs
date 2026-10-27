module LordOfTheRings.Enemy.Cards.WildBear (wildBear) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype WildBear = WildBear EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wildBear :: EnemyCard WildBear
wildBear = enemy WildBear Cards.wildBear

-- | Ambush.
-- Forced: After Wild Bear engages a player, it makes an immediate attack.
instance HasAbilities WildBear where
  getAbilities (WildBear a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage WildBear where
  runMessage msg e@(WildBear attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> immediateAttack pid attrs.id
      pure e
    _ -> WildBear <$> runMessage msg attrs
