module LordOfTheRings.Enemy.Cards.StranglingPython (stranglingPython) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Trait (Trait (Condition))

newtype StranglingPython = StranglingPython EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Surge, on its card def.
stranglingPython :: EnemyCard StranglingPython
stranglingPython = enemy StranglingPython Cards.stranglingPython

-- | As a Condition attachment: "Attached character cannot ready." Attached, it has the
-- Condition trait, for what discards a Condition attachment to find.
instance HasModifiersFor StranglingPython where
  getModifiersFor (StranglingPython a) = withHost a \host -> do
    modified a host [CannotReady]
    modifySelf a [GainTrait Condition]

{- | Forced: After Strangling Python damages a character, attach it to that character. It
attaches once its attack has resolved, as Writhing Tentacle does, if the character is
still in play.
-}
instance RunMessage StranglingPython where
  runMessage msg e@(StranglingPython attrs) = case msg of
    DoStep 1 (DealDamage (sourceEnemy -> Just eid) target n) | eid == attrs.id, n > 0, isCharacter target -> do
      afterAttack attrs.id [DoStep 2 msg]
      StranglingPython <$> runMessage msg attrs
    DoStep 2 (DoStep 1 (DealDamage (sourceEnemy -> Just eid) target _)) | eid == attrs.id -> do
      alive <- case target of
        HeroTarget hid -> selectAny (HeroWithId hid)
        AllyTarget aid -> selectAny (AllyWithId aid)
        _ -> pure False
      when (alive && not (attached attrs.placement)) $ placeEnemy attrs.id (AttachedTo target)
      pure e
    _ -> StranglingPython <$> runMessage msg attrs
   where
    isCharacter = \case
      HeroTarget _ -> True
      AllyTarget _ -> True
      _ -> False
    attached = \case
      AttachedTo _ -> True
      _ -> False
