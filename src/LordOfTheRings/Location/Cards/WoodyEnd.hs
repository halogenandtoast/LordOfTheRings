module LordOfTheRings.Location.Cards.WoodyEnd (woodyEnd) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype WoodyEnd = WoodyEnd LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodyEnd :: LocationCard WoodyEnd
woodyEnd = location WoodyEnd Cards.woodyEnd

-- | While Woody End is the active location, reduce each Hide X value by 1.
instance HasModifiersFor WoodyEnd where
  getModifiersFor (WoodyEnd a) = when (a.placement == ActiveLocation) $ modified a GameTarget [HideValue (-1)]

instance RunMessage WoodyEnd where
  runMessage msg (WoodyEnd a) = WoodyEnd <$> runMessage msg a
