module LordOfTheRings.Location.Cards.TunnelsOfMountGram (tunnelsOfMountGram) where

import LordOfTheRings.Card (toCard)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Entities (entitiesAttachments)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedUnder)

newtype TunnelsOfMountGram = TunnelsOfMountGram LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Capture 1, which the scenario resolves.
tunnelsOfMountGram :: LocationCard TunnelsOfMountGram
tunnelsOfMountGram = location TunnelsOfMountGram Cards.tunnelsOfMountGram

-- | X is the number of cards captured underneath Tunnels of Mount Gram.
instance HasModifiersFor TunnelsOfMountGram where
  getModifiersFor (TunnelsOfMountGram a) = do
    x <- length <$> capturedUnder a.cardId
    modified a a [ThreatModifier x | x > 0]

-- | Shadow: Capture each attachment attached to the defending character underneath the
-- attacking enemy.
instance RunMessage TunnelsOfMountGram where
  runMessage msg l@(TunnelsOfMountGram a) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      attachments <- getsGame (toList . entitiesAttachments . gameEntities)
      let held = [toCard att | att <- attachments, att.placement `elem` map AttachedTo defenders]
      unless (null held) $ placeUnderneath eid held
      pure l
    _ -> TunnelsOfMountGram <$> runMessage msg a
