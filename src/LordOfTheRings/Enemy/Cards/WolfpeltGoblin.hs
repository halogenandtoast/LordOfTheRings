module LordOfTheRings.Enemy.Cards.WolfpeltGoblin (wolfpeltGoblin) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)

newtype WolfpeltGoblin = WolfpeltGoblin EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

wolfpeltGoblin :: EnemyCard WolfpeltGoblin
wolfpeltGoblin = enemy WolfpeltGoblin Cards.wolfpeltGoblin

-- | Forced: After engaging Wolfpelt Goblin, the engaged player must deal X damage to 1
-- character he controls, where X is the active location's remaining quest points. Then,
-- remove 2 progress from the active location, if able.
instance HasAbilities WolfpeltGoblin where
  getAbilities (WolfpeltGoblin a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage WolfpeltGoblin where
  runMessage msg e@(WolfpeltGoblin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      actives <- traverse getLocation =<< select IsActiveLocation
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> for_ actives \l -> do
        x <- subtract l.progress <$> getLocationQuestPoints l.id
        when (x > 0) $ dealDamageToOneCharacter attrs pid x
        when (l.progress > 0) $ removeTokens attrs l Progress (min 2 l.progress)
      pure e
    _ -> WolfpeltGoblin <$> runMessage msg attrs
