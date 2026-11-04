module LordOfTheRings.Enemy.Cards.TorechGorgorPatrol (torechGorgorPatrol) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.UnderTheAshMountains.Helpers (ashX)

newtype TorechGorgorPatrol = TorechGorgorPatrol EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

torechGorgorPatrol :: EnemyCard TorechGorgorPatrol
torechGorgorPatrol = enemy TorechGorgorPatrol Cards.torechGorgorPatrol

-- | X is 1 more than the number of quest cards in play.
-- While Torech Gorgor Patrol is in the staging area, no more than 5 progress can be placed
-- on the main quest each round.
instance HasModifiersFor TorechGorgorPatrol where
  getModifiersFor (TorechGorgorPatrol a) = do
    x <- ashX
    modifySelf a [ThreatModifier x]
    when (a.placement == StagingArea) do
      mains <- getMainQuests
      for_ mains \q -> modified a q [ProgressPerRoundLimit 5]

-- | Forced: After Torech Gorgor Patrol engages a player, it makes an attack.
instance HasAbilities TorechGorgorPatrol where
  getAbilities (TorechGorgorPatrol a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow After a.id]

instance RunMessage TorechGorgorPatrol where
  runMessage msg e@(TorechGorgorPatrol attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (take 1 [pid | Window _ (EnemyEngaged _ pid) <- ws]) \pid -> immediateAttack pid attrs.id
      pure e
    _ -> TorechGorgorPatrol <$> runMessage msg attrs
