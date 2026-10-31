module LordOfTheRings.Enemy.Cards.MithrilSeeker (mithrilSeeker) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype MithrilSeeker = MithrilSeeker EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

mithrilSeeker :: EnemyCard MithrilSeeker
mithrilSeeker = enemy MithrilSeeker Cards.mithrilSeeker

-- | Mithril Seeker gets +1 [Attack] for each resource on it.
instance HasModifiersFor MithrilSeeker where
  getModifiersFor (MithrilSeeker a) = do
    let n = countTokens Resource (enemyTokens a)
    when (n > 0) $ modifySelf a [AttackModifier n]

-- | Forced: When Mithril Seeker attacks, move 1 resource from each of the defending
-- player's heroes to Mithril Seeker.
instance HasAbilities MithrilSeeker where
  getAbilities (MithrilSeeker a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow When a.id]

instance RunMessage MithrilSeeker where
  runMessage msg e@(MithrilSeeker attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyAttacking _ pid) <- ws] \pid -> do
        heroes <- traverse getHero =<< select (HeroControlledBy $ PlayerWithId pid)
        for_ [h | h <- heroes, h.resources > 0] \h -> do
          removeTokens attrs h Resource 1
          placeResources attrs attrs 1
      pure e
    _ -> MithrilSeeker <$> runMessage msg attrs
