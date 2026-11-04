module LordOfTheRings.Ally.Cards.MirkwoodHunter (mirkwoodHunter) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))

newtype MirkwoodHunter = MirkwoodHunter AllyAttrs
  deriving anyclass (IsAlly, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Ranged, on its card def.
mirkwoodHunter :: AllyCard MirkwoodHunter
mirkwoodHunter = ally MirkwoodHunter Cards.mirkwoodHunter

-- | Reduce the cost to play Mirkwood Hunter by 1 for each attachment on the active
-- location.
instance HasModifiersFor MirkwoodHunter where
  getModifiersFor (MirkwoodHunter a) = when (a.placement == Unplaced) do
    n <- activeLocationAttachments
    when (n > 0) $ modified a (CardIdTarget a.allyCardId) [CostModifier (negate n)]

instance RunMessage MirkwoodHunter where
  runMessage msg (MirkwoodHunter a) = MirkwoodHunter <$> runMessage msg a
