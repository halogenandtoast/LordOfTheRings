module LordOfTheRings.Objective.Cards.GandalfsMap (gandalfsMap) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Claim
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))

newtype GandalfsMap = GandalfsMap ObjectiveAttrs
  deriving anyclass (IsObjective)
  deriving newtype (Show, Eq, ToJSON, Entity)

gandalfsMap :: ObjectiveCard GandalfsMap
gandalfsMap = objective GandalfsMap Cards.gandalfsMap

instance HasAbilities GandalfsMap where
  getAbilities (GandalfsMap a) = claimAbility a

-- | Attached hero cannot attack or defend.
instance HasModifiersFor GandalfsMap where
  getModifiersFor (GandalfsMap a) = case a.placement of
    AttachedTo host -> modified a host [CannotAttackOrDefend]
    _ -> pure ()

instance RunMessage GandalfsMap where
  runMessage msg o@(GandalfsMap attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      push =<< claimObjective pid attrs
      pure o
    _ -> GandalfsMap <$> runMessage msg attrs
