module LordOfTheRings.Enemy.Cards.PrisonGuardCirithGurat (prisonGuardCirithGurat) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat.Helpers (guardingAnObjective)

newtype PrisonGuardCirithGurat = PrisonGuardCirithGurat EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Dungeons of Cirith Gurat's Prison Guard (Escape from Mount Gram has its own).
prisonGuardCirithGurat :: EnemyCard PrisonGuardCirithGurat
prisonGuardCirithGurat = enemy PrisonGuardCirithGurat Cards.prisonGuardCirithGurat

-- | While Prison Guard is guarding an objective, it gets +2 defense.
instance HasModifiersFor PrisonGuardCirithGurat where
  getModifiersFor (PrisonGuardCirithGurat a) = whenM (guardingAnObjective a) $ modifySelf a [DefenseModifier 2]

{- | While Prison Guard is not guarding an objective, it gains: "Forced: After Prison Guard
attacks and damages an ally, attach that ally to Prison Guard."

-}
instance RunMessage PrisonGuardCirithGurat where
  runMessage msg (PrisonGuardCirithGurat attrs) = case msg of
    DoStep 1 (DealDamage (EnemyAttackSource eid) ally@(AllyTarget _) n) | eid == attrs.id && n > 0 -> do
      unlessM (guardingAnObjective attrs) $ guardCharacterWith ally attrs
      PrisonGuardCirithGurat <$> runMessage msg attrs
    _ -> PrisonGuardCirithGurat <$> runMessage msg attrs
