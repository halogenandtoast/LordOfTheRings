module LordOfTheRings.Ally.Cards.NorthernBowmaster (northernBowmaster) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))

newtype NorthernBowmaster = NorthernBowmaster AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, on its card def.
northernBowmaster :: AllyCard NorthernBowmaster
northernBowmaster = ally NorthernBowmaster Cards.northernBowmaster

-- | You cannot play Northern Bowmaster from your hand unless you are engaged with an enemy.
instance HasModifiersFor NorthernBowmaster where
  getModifiersFor (NorthernBowmaster a) = when (a.placement == Unplaced) do
    engaged <- selectAny (EnemyEngagedWith $ PlayerWithId a.owner)
    unless engaged $ modified a (PlayerTarget a.owner) [CannotPlay (cardIs Cards.northernBowmaster)]

instance RunMessage NorthernBowmaster where
  runMessage msg (NorthernBowmaster a) = NorthernBowmaster <$> runMessage msg a
