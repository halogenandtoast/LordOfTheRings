module LordOfTheRings.Location.Cards.TheSpidersRing (theSpidersRing) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheSpidersRing = TheSpidersRing LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theSpidersRing :: LocationCard TheSpidersRing
theSpidersRing = location TheSpidersRing Cards.theSpidersRing

-- | While The Spider's Ring is the active location, the player guarding Gollum cannot
-- change. (See the scenario's helpers.)
-- Shadow: If this attack is undefended, return any current active location to the staging
-- area. The Spider's Ring becomes the active location.
instance RunMessage TheSpidersRing where
  runMessage msg l@(TheSpidersRing a) = case msg of
    ResolveShadowEffect cid eid _ Nothing | cid == a.cardId -> do
      putShadowLocationIntoPlay eid cid
      pure l
    _ -> TheSpidersRing <$> runMessage msg a
