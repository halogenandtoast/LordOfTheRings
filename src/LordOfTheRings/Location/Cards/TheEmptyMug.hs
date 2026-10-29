module LordOfTheRings.Location.Cards.TheEmptyMug (theEmptyMug) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheEmptyMug = TheEmptyMug LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theEmptyMug :: LocationCard TheEmptyMug
theEmptyMug = location TheEmptyMug Cards.theEmptyMug

-- | Players cannot reduce their threat.
-- (While The Empty Mug is in the victory display, the current quest gains its Forced: see
-- the stages of Trouble in Tharbad.)
instance HasModifiersFor TheEmptyMug where
  getModifiersFor (TheEmptyMug a) = do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotReduceThreat]

instance RunMessage TheEmptyMug where
  runMessage msg (TheEmptyMug attrs) = TheEmptyMug <$> runMessage msg attrs
