module LordOfTheRings.Enemy.Cards.GollumMountDoom (gollumMountDoom) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype GollumMountDoom = GollumMountDoom EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Forced: After Gollum is discarded during a Fortitude test, put him into play engaged with
the first player. Then, choose an encounter card discarded for this test and ignore its Eye
icons. Out of play as it triggers, the scenario resolves it (see
"LordOfTheRings.Scenario.Scenarios.MountDoom").
-}
gollumMountDoom :: EnemyCard GollumMountDoom
gollumMountDoom = enemy GollumMountDoom Cards.gollumMountDoom

-- | Immune to non-Fellowship player card effects. Gollum engages the first player.
instance HasModifiersFor GollumMountDoom where
  getModifiersFor (GollumMountDoom a) = do
    lead <- getLead
    modifySelf a [ImmuneToNonFellowshipPlayerCardEffects, EngagesOnly lead]

instance RunMessage GollumMountDoom where
  runMessage msg (GollumMountDoom attrs) = GollumMountDoom <$> runMessage msg attrs
