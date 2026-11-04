module LordOfTheRings.Location.Cards.TheLastBridge (theLastBridge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheLastBridge = TheLastBridge LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLastBridge :: LocationCard TheLastBridge
theLastBridge = location TheLastBridge Cards.theLastBridge

-- | Immune to player card effects. X is the number of players in the game. With the
-- Burden deck empty its travel cost cannot be paid, so the players cannot travel here.
instance HasModifiersFor TheLastBridge where
  getModifiersFor (TheLastBridge a) = do
    players <- length <$> getRemainingPlayers
    burdens <- getBurdenDeck
    modifySelf a $ [ImmuneToPlayerCardEffects, ThreatModifier players] <> [CannotBeTraveledTo | null burdens]

-- | Travel: The first player must reveal the top card of the burden deck and resolve that
-- card as if it were just revealed from the encounter deck to travel here.
instance RunMessage TheLastBridge where
  runMessage msg (TheLastBridge a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealTopOfBurdenDeck
      TheLastBridge <$> runMessage msg a
    _ -> TheLastBridge <$> runMessage msg a
