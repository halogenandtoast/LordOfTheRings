module LordOfTheRings.Enemy.Cards.RavenClawDruid (ravenClawDruid) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (activeLocationTimeX, removeTimeCountersFromLocations)

newtype RavenClawDruid = RavenClawDruid EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenClawDruid :: EnemyCard RavenClawDruid
ravenClawDruid = enemy RavenClawDruid Cards.ravenClawDruid

-- | Forced: After the engaged player draws any number of cards, remove 1 time counter from
-- a location in play. The engaged player chooses which.
instance HasAbilities RavenClawDruid where
  getAbilities (RavenClawDruid a) =
    [mkAbility a 1 $ forced $ DrewCardsWindow After (Just pid) | EngagedWith pid <- [a.placement]]

-- | Shadow: Attacking enemy gets +X attack, where X is the Time X value on the active
-- location.
instance RunMessage RavenClawDruid where
  runMessage msg e@(RavenClawDruid attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [pid | EngagedWith pid <- [attrs.placement]] \pid -> removeTimeCountersFromLocations attrs pid 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      x <- activeLocationTimeX
      when (x > 0) $ enemyGetsAttack attrs eid x
      pure e
    _ -> RavenClawDruid <$> runMessage msg attrs
