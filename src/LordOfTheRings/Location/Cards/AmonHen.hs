module LordOfTheRings.Location.Cards.AmonHen (amonHen) where

import LordOfTheRings.Card (CardType (EventType))
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype AmonHen = AmonHen LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

amonHen :: LocationCard AmonHen
amonHen = location AmonHen Cards.amonHen

-- | X is double the number of players in the game. While Amon Hen is the active location,
-- players cannot play events.
instance HasModifiersFor AmonHen where
  getModifiersFor (AmonHen a) = do
    players <- length <$> getRemainingPlayers
    modified a a [ThreatModifier (2 * players)]
    when (a.placement == ActiveLocation) $ modified a GameTarget [CannotPlay (CardWithType EventType)]
