module LordOfTheRings.Enemy.Cards.RingwraithOfSauron (ringwraithOfSauron) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Morgul))

newtype RingwraithOfSauron = RingwraithOfSauron EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

ringwraithOfSauron :: EnemyCard RingwraithOfSauron
ringwraithOfSauron = enemy RingwraithOfSauron Cards.ringwraithOfSauron

-- | Cannot have non-Morgul attachments.
instance HasModifiersFor RingwraithOfSauron where
  getModifiersFor (RingwraithOfSauron a) = modifySelf a [CannotHaveAttachmentsWithoutTrait Morgul]

-- | While Ringwraith of Sauron is in the staging area, it gains: "Forced: After a player
-- fails a Fortitude test, Ringwraith of Sauron engages that player and makes an immediate
-- attack."
instance HasAbilities RingwraithOfSauron where
  getAbilities (RingwraithOfSauron a) = [mkAbility a 1 $ forced $ FailedFortitudeTestWindow After AnyPlayer | a.placement == StagingArea]

instance RunMessage RingwraithOfSauron where
  runMessage msg e@(RingwraithOfSauron attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ (headMay [pid | Window _ (FailedFortitudeTest pid) <- ws]) \pid -> do
        engageEnemy pid attrs.id
        immediateAttack pid attrs.id
      pure e
    _ -> RingwraithOfSauron <$> runMessage msg attrs
