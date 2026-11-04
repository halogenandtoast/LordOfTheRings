module LordOfTheRings.Ally.Cards.BywaterShirriff (bywaterShirriff) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (placementController)

newtype BywaterShirriff = BywaterShirriff AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

bywaterShirriff :: AllyCard BywaterShirriff
bywaterShirriff = ally BywaterShirriff Cards.bywaterShirriff

-- | Bywater Shirriff gets +1 willpower, +1 attack, and +1 defense while you are engaged
-- with an enemy with engagement cost higher than your threat.
instance HasModifiersFor BywaterShirriff where
  getModifiersFor (BywaterShirriff a) = for_ (placementController a.placement) \pid -> do
    engaged <- selectAny (EnemyEngagedWith (PlayerWithId pid) <> EnemyWithEngagementCostAboveThreatOf pid)
    when engaged $ modifySelf a [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
