module LordOfTheRings.Enemy.Cards.IslandWatcher (islandWatcher) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Ruins))

newtype IslandWatcher = IslandWatcher EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

islandWatcher :: EnemyCard IslandWatcher
islandWatcher = enemy IslandWatcher Cards.islandWatcher

-- | Island Watcher cannot be optionally engaged.
instance HasModifiersFor IslandWatcher where
  getModifiersFor (IslandWatcher a) = modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | While Island Watcher is in the staging area, it gains: "Forced: At the beginning of the
-- encounter phase, if the active location has the Ruins trait, Island Watcher engages the
-- player with the highest threat. Then, deal it 2 shadow cards."
instance HasAbilities IslandWatcher where
  getAbilities (IslandWatcher a) =
    [ restricted a 1 (exists $ IsActiveLocation <> LocationWithTrait Ruins) $ forced $ PhaseBeginsWindow After EncounterPhase
    | a.placement == StagingArea
    ]

instance RunMessage IslandWatcher where
  runMessage msg e@(IslandWatcher attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHighestThreatPlayer attrs \pid -> do
        engageEnemy pid attrs.id
        dealShadowCard attrs.id
        dealShadowCard attrs.id
      pure e
    _ -> IslandWatcher <$> runMessage msg attrs
