module LordOfTheRings.Location.Cards.TheWhiteBridge (theWhiteBridge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (towerProgress)

newtype TheWhiteBridge = TheWhiteBridge LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWhiteBridge :: LocationCard TheWhiteBridge
theWhiteBridge = location TheWhiteBridge Cards.theWhiteBridge

-- | X is the number of progress tokens on To the Tower.
instance HasModifiersFor TheWhiteBridge where
  getModifiersFor (TheWhiteBridge a) = do
    x <- towerProgress
    modified a a [ThreatModifier x, QuestPointModifier x]

instance RunMessage TheWhiteBridge where
  runMessage msg (TheWhiteBridge a) = TheWhiteBridge <$> runMessage msg a
