module LordOfTheRings.Location.Cards.FloodedRuins (floodedRuins) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

-- | Uncharted, under a Lost Island side.
newtype FloodedRuins = FloodedRuins LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

floodedRuins :: LocationCard FloodedRuins
floodedRuins = location FloodedRuins Cards.floodedRuins

-- | While Flooded Ruins is the active location, it contributes its threat to the staging
-- area.
-- ponytail: its printed threat, as no card here changes it
instance HasModifiersFor FloodedRuins where
  getModifiersFor (FloodedRuins a) = when (a.placement == ActiveLocation) $ modified a GameTarget [StagingThreatBonus a.threat]

instance RunMessage FloodedRuins where
  runMessage msg (FloodedRuins a) = FloodedRuins <$> runMessage msg a
