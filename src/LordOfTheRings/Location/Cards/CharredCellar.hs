module LordOfTheRings.Location.Cards.CharredCellar (charredCellar) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype CharredCellar = CharredCellar LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

charredCellar :: LocationCard CharredCellar
charredCellar = location CharredCellar Cards.charredCellar

-- | While Charred Cellar is the active location, it gains: "Forced: After a player's threat
-- is increased by an encounter card effect, that player must deal 2 damage to a character
-- he controls." It answers the window itself, for whichever player it names.
instance RunMessage CharredCellar where
  runMessage msg (CharredCellar a) = case msg of
    CheckWindows ws | a.placement == ActiveLocation -> do
      for_ [pid | Window After (ThreatRaisedByEncounter pid _) <- ws] \pid -> dealDamageToOneCharacter a pid 2
      CharredCellar <$> runMessage msg a
    _ -> CharredCellar <$> runMessage msg a
