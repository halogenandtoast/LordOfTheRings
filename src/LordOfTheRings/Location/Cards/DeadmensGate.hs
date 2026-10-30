module LordOfTheRings.Location.Cards.DeadmensGate (deadmensGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype DeadmensGate = DeadmensGate LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

deadmensGate :: LocationCard DeadmensGate
deadmensGate = location DeadmensGate Cards.deadmensGate

-- | While Deadmen's Gate is the active location, it gains: "Forced: After a player
-- triggers an 'Action' or 'Response' effect, he must discard the top card of his deck."
-- Read off the abilities as they are used and the events as they are played, every event
-- being an Action or a Response; what the card pushes follows what they push.
-- Shadow: Discard an attachment you control.
instance RunMessage DeadmensGate where
  runMessage msg l@(DeadmensGate a) = case msg of
    UseAbility pid ab _ | active, triggered ab.abilityType -> do
      discardTopOfDeck pid 1
      DeadmensGate <$> runMessage msg a
    PlayEvent pid _ _ | active -> do
      discardTopOfDeck pid 1
      DeadmensGate <$> runMessage msg a
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      chooseAndDiscard a pid attachments
      pure l
    _ -> DeadmensGate <$> runMessage msg a
   where
    active = a.placement == ActiveLocation
    triggered = \case
      ActionAbility _ -> True
      ResponseAbility _ _ -> True
      _ -> False
