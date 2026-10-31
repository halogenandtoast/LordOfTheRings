module LordOfTheRings.Enemy.Cards.BoarSpirit (boarSpirit) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials.Helpers (guardianAwake, spiritEngages)
import LordOfTheRings.Trait (Trait (Boar))

newtype BoarSpirit = BoarSpirit EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

boarSpirit :: EnemyCard BoarSpirit
boarSpirit = enemy BoarSpirit Cards.boarSpirit

-- | If Boar's Guardian is in play or in the victory display, each Boar enemy gets +1 defense.
instance HasModifiersFor BoarSpirit where
  getModifiersFor (BoarSpirit a) = whenM (guardianAwake Cards.boarsGuardian) do
    enemies <- select (EnemyWithTrait Boar)
    for_ enemies \e -> modified a e [DefenseModifier 1]

-- | Forced: At the beginning of the encounter phase, Boar Spirit engages the player who
-- controls Key of the Boar or is engaged with Boar's Guardian, if able.
instance HasAbilities BoarSpirit where
  getAbilities (BoarSpirit a) = [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]

instance RunMessage BoarSpirit where
  runMessage msg e@(BoarSpirit attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      spiritEngages attrs.id Cards.boarSpirit Cards.boarsGuardian
      pure e
    _ -> BoarSpirit <$> runMessage msg attrs
