module LordOfTheRings.Enemy.Cards.RavenSpirit (ravenSpirit) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (guardianAwake, spiritEngages)
import LordOfTheRings.Trait (Trait (Raven))

newtype RavenSpirit = RavenSpirit EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

ravenSpirit :: EnemyCard RavenSpirit
ravenSpirit = enemy RavenSpirit Cards.ravenSpirit

-- | If Raven's Guardian is in play or in the victory display, each Raven enemy gets +1 threat.
instance HasModifiersFor RavenSpirit where
  getModifiersFor (RavenSpirit a) = whenM (guardianAwake Cards.ravensGuardian) do
    enemies <- select (EnemyWithTrait Raven)
    for_ enemies \e -> modified a e [ThreatModifier 1]

-- | Forced: At the beginning of the encounter phase, Raven Spirit engages the player who
-- controls Key of the Raven or is engaged with Raven's Guardian, if able.
instance HasAbilities RavenSpirit where
  getAbilities (RavenSpirit a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

instance RunMessage RavenSpirit where
  runMessage msg e@(RavenSpirit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      spiritEngages attrs.id Cards.ravenSpirit Cards.ravensGuardian
      pure e
    _ -> RavenSpirit <$> runMessage msg attrs
