module LordOfTheRings.Enemy.Cards.RaidLeader (raidLeader) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype RaidLeader = RaidLeader EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

raidLeader :: EnemyCard RaidLeader
raidLeader = enemy RaidLeader Cards.raidLeader

-- | Raid Leader gets +1 defense for each resource on it.
instance HasModifiersFor RaidLeader where
  getModifiersFor (RaidLeader a) = do
    let n = countTokens Resource a.enemyTokens
    when (n > 0) $ modified a a [DefenseModifier n]

-- | Forced: When Raid Leader engages you, move 1 resource from each of your heroes'
-- resource pools to Raid Leader.
instance HasAbilities RaidLeader where
  getAbilities (RaidLeader a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

instance RunMessage RaidLeader where
  runMessage msg e@(RaidLeader attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> do
        heroes <- select (HeroControlledBy (PlayerWithId pid) <> HeroWithResources)
        for_ heroes \h -> do
          removeTokens attrs h Resource 1
          placeResources attrs attrs 1
      pure e
    _ -> RaidLeader <$> runMessage msg attrs
