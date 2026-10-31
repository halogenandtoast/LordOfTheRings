module LordOfTheRings.Location.Cards.IthilienOverlook (ithilienOverlook) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (InVictoryDisplay, Unplaced))

newtype IthilienOverlook = IthilienOverlook LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

ithilienOverlook :: LocationCard IthilienOverlook
ithilienOverlook = location IthilienOverlook Cards.ithilienOverlook

-- | Archery 2.
-- When assigning archery damage to a character, you must continue assigning archery damage
-- to that character until that character is destroyed or there is no more archery damage
-- to assign.
instance HasModifiersFor IthilienOverlook where
  getModifiersFor (IthilienOverlook a) =
    when (a.placement `notElem` [InVictoryDisplay, Unplaced]) $ modified a GameTarget [ArcheryDamageStaysOnCharacter]

instance RunMessage IthilienOverlook where
  runMessage msg (IthilienOverlook a) = IthilienOverlook <$> runMessage msg a
