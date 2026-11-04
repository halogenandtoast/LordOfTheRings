module LordOfTheRings.Enemy.Cards.PrisonGuardCirithGurat (prisonGuardCirithGurat) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
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
attacks and damages an ally, attach that ally to Prison Guard." Its attack's damage marks
the ally, which it takes once the attack has resolved, as Strangling Python attaches, if
the ally is still in play and Prison Guard is guarding no objective then.
-}
instance RunMessage PrisonGuardCirithGurat where
  runMessage msg e@(PrisonGuardCirithGurat attrs) = case msg of
    DoStep 1 (DealDamage (EnemyAttackSource eid) (AllyTarget _) n) | eid == attrs.id && n > 0 -> do
      afterAttack attrs.id [DoStep 2 msg]
      PrisonGuardCirithGurat <$> runMessage msg attrs
    DoStep 2 (DoStep 1 (DealDamage (EnemyAttackSource eid) ally@(AllyTarget aid) _)) | eid == attrs.id -> do
      -- once, however many times the attack damaged it
      present <- selectAny (AllyWithId aid)
      held <- if present then (== AttachedTo (CardIdTarget attrs.cardId)) . (.placement) <$> getAlly aid else pure True
      unless held $ unlessM (guardingAnObjective attrs) $ guardCharacterWith ally attrs
      pure e
    _ -> PrisonGuardCirithGurat <$> runMessage msg attrs
