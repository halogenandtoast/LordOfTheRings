module LordOfTheRings.Enemy.Cards.CrazedCaptive (crazedCaptive) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted

newtype CrazedCaptive = CrazedCaptive EnemyAttrs
  deriving anyclass (IsEnemy, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crazedCaptive :: EnemyCard CrazedCaptive
crazedCaptive = enemy CrazedCaptive Cards.crazedCaptive

instance HasAbilities CrazedCaptive where
  getAbilities (CrazedCaptive a) = [mkAbility a 1 $ forced $ ThisEnemyDefeatedWindow When a.id]

-- | Doomed 1.
-- Forced: If Crazed Captive is defeated, raise each player's threat by 7.
-- Shadow: If attacking enemy is not defeated this phase, raise defending player's threat by
-- 7 at the end of the phase.
instance RunMessage CrazedCaptive where
  runMessage msg e@(CrazedCaptive attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      eachPlayer \pid -> encounterRaisesThreat pid 7
      pure e
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      phaseModifier attrs eid (RaisesThreatAtEndOfPhase pid 7)
      pure e
    _ -> CrazedCaptive <$> runMessage msg attrs
