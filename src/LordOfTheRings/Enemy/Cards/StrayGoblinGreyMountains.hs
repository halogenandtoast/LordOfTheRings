module LordOfTheRings.Enemy.Cards.StrayGoblinGreyMountains (strayGoblinGreyMountains) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

-- | Grey Mountain Goblins' Stray Goblin; Shadow and Flame's is
-- "LordOfTheRings.Enemy.Cards.StrayGoblin".
newtype StrayGoblinGreyMountains = StrayGoblinGreyMountains EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

strayGoblinGreyMountains :: EnemyCard StrayGoblinGreyMountains
strayGoblinGreyMountains = enemy StrayGoblinGreyMountains Cards.strayGoblinGreyMountains

-- | Surge. Cannot be optionally engaged.
instance HasModifiersFor StrayGoblinGreyMountains where
  getModifiersFor (StrayGoblinGreyMountains a) = modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | While Stray Goblin is in the staging area, it gains: "Forced: After a player engages an
-- enemy, Stray Goblin engages that player."
instance HasAbilities StrayGoblinGreyMountains where
  getAbilities (StrayGoblinGreyMountains a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | a.placement == StagingArea]

-- | Shadow: Add Stray Goblin to the staging area.
instance RunMessage StrayGoblinGreyMountains where
  runMessage msg e@(StrayGoblinGreyMountains attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window After (EnemyEngaged eid pid) <- ws, eid /= attrs.id]) \pid ->
        engageEnemy pid attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      putShadowEnemyIntoStaging eid cid
      pure e
    _ -> StrayGoblinGreyMountains <$> runMessage msg attrs
