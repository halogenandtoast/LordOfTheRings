module LordOfTheRings.Treachery.Cards.WhipLash (whipLash) where

import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype WhipLash = WhipLash TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

whipLash :: TreacheryCard WhipLash
whipLash = treachery WhipLash Cards.whipLash

-- | When Revealed: Each player must discard 1 attachment he controls, if able.
-- Shadow: If attacking enemy is Durin's Bane, the defending player discards all
-- attachments he controls.
instance RunMessage WhipLash where
  runMessage msg t@(WhipLash attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> chooseAndDiscard attrs p =<< getAttachmentsControlledBy p
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      whenM (isDurinsBane eid) $ traverse_ (discard attrs) =<< getAttachmentsControlledBy pid
      pure t
    _ -> WhipLash <$> runMessage msg attrs
