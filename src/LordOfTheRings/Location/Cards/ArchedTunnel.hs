module LordOfTheRings.Location.Cards.ArchedTunnel (archedTunnel) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (placeUnderworldCards, revealUnderneath)

newtype ArchedTunnel = ArchedTunnel LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

archedTunnel :: LocationCard ArchedTunnel
archedTunnel = location ArchedTunnel Cards.archedTunnel

-- | Underworld 1 (see the scenario).
-- Travel: Reveal all facedown cards underneath Arched Tunnel, one at a time. Then, place
-- the top card of the underworld deck facedown underneath Arched Tunnel, if able.
instance RunMessage ArchedTunnel where
  runMessage msg l@(ArchedTunnel a) = case msg of
    TravelTo lid | lid == a.id -> do
      revealUnderneath a.id
      placeUnderworldCards a.id 1
      pure l
    _ -> ArchedTunnel <$> runMessage msg a
