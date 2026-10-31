module LordOfTheRings.Enemy.Cards.WolfSpirit (wolfSpirit) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (guardianAwake, spiritEngages)
import LordOfTheRings.Trait (Trait (Wolf))

newtype WolfSpirit = WolfSpirit EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

wolfSpirit :: EnemyCard WolfSpirit
wolfSpirit = enemy WolfSpirit Cards.wolfSpirit

-- | If Wolf's Guardian is in play or in the victory display, each Wolf enemy gets +1 attack.
instance HasModifiersFor WolfSpirit where
  getModifiersFor (WolfSpirit a) = whenM (guardianAwake Cards.wolfsGuardian) do
    enemies <- select (EnemyWithTrait Wolf)
    for_ enemies \e -> modified a e [AttackModifier 1]

-- | Forced: At the beginning of the encounter phase, Wolf Spirit engages the player who
-- controls Key of the Wolf or is engaged with Wolf's Guardian, if able.
instance HasAbilities WolfSpirit where
  getAbilities (WolfSpirit a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

instance RunMessage WolfSpirit where
  runMessage msg e@(WolfSpirit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      spiritEngages attrs.id Cards.wolfSpirit Cards.wolfsGuardian
      pure e
    _ -> WolfSpirit <$> runMessage msg attrs
