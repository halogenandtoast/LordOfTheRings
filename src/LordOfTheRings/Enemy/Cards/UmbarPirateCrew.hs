module LordOfTheRings.Enemy.Cards.UmbarPirateCrew (umbarPirateCrew) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfGondor.Helpers (shipIsActive)

newtype UmbarPirateCrew = UmbarPirateCrew EnemyAttrs
  deriving anyclass IsEnemy
  deriving newtype (Show, Eq, ToJSON, Entity)

umbarPirateCrew :: EnemyCard UmbarPirateCrew
umbarPirateCrew = enemy UmbarPirateCrew Cards.umbarPirateCrew

-- | Cannot have attachments.
instance HasModifiersFor UmbarPirateCrew where
  getModifiersFor (UmbarPirateCrew a) = modifySelf a [CannotHaveAttachments]

-- | Forced: When Umbar Pirate Crew engages you, exhaust a character you control.
instance HasAbilities UmbarPirateCrew where
  getAbilities (UmbarPirateCrew a) = [mkAbility a 1 $ forced $ EnemyEngagedWindow When a.id]

-- | Shadow: If the active location is a Ship, return attacking enemy to the staging area
-- after this attack.
instance RunMessage UmbarPirateCrew where
  runMessage msg e@(UmbarPirateCrew attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 ws -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- ws] \pid -> chooseAndExhaust attrs pid 1
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ship <- shipIsActive
      when ship $ returnToStagingAfterAttack eid
      pure e
    _ -> UmbarPirateCrew <$> runMessage msg attrs
