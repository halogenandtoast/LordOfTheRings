module LordOfTheRings.Ally.Cards.HalbaradLeadership (halbaradLeadership) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced), placementController)

newtype HalbaradLeadership = HalbaradLeadership AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

halbaradLeadership :: AllyCard HalbaradLeadership
halbaradLeadership = ally HalbaradLeadership Cards.halbaradLeadership

-- | Reduce the cost to play Halbarad by 1 for each enemy engaged with you. Each enemy
-- engaged with you gets +10 engagement cost.
instance HasModifiersFor HalbaradLeadership where
  getModifiersFor (HalbaradLeadership a)
    | a.placement == Unplaced = do
        n <- length <$> select (EnemyEngagedWith $ PlayerWithId a.controller)
        when (n > 0) $ modified a (CardIdTarget a.allyCardId) [CostModifier (negate n)]
    | otherwise = for_ (placementController a.placement) \you -> do
        enemies <- select (EnemyEngagedWith $ PlayerWithId you)
        for_ enemies \eid -> modified a eid [EngagementCostModifier 10]
