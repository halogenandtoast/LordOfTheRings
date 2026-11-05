module LordOfTheRings.Enemy.Cards.FellRiderNightmare (fellRiderNightmare) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Helpers.Fellowship (theOneRingExhausted)
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)
import LordOfTheRings.Trait (Trait (Morgul))

newtype FellRiderNightmare = FellRiderNightmare EnemyAttrs
  deriving anyclass (IsEnemy)
  deriving newtype (Show, Eq, ToJSON, Entity)

fellRiderNightmare :: EnemyCard FellRiderNightmare
fellRiderNightmare = enemy FellRiderNightmare Cards.fellRiderNightmare

instance HasModifiersFor FellRiderNightmare where
  getModifiersFor (FellRiderNightmare a) = modifySelf a [CannotHaveAttachmentsWithoutTrait Morgul]

instance HasAbilities FellRiderNightmare where
  getAbilities (FellRiderNightmare a) = [mkAbility a 1 $ forced $ EnemyAttacksWindow After a.id]

instance RunMessage FellRiderNightmare where
  runMessage msg e@(FellRiderNightmare a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      whenM (attackDestroyedACharacter a.id) $ reduceRingBearersLife 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      exhausted <- theOneRingExhausted
      enemyGetsAttack (CardIdSource cid) eid (if exhausted then 2 else 1)
      pure e
    _ -> FellRiderNightmare <$> runMessage msg a
