module LordOfTheRings.Treachery.Cards.Disappearances (disappearances) where

import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype Disappearances = Disappearances TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

disappearances :: TreacheryCard Disappearances
disappearances = treachery Disappearances Cards.disappearances

-- | When Revealed: Each player discards all ally cards from his hand. If no cards are
-- discarded this way, Disappearances gains doomed 2.
-- Shadow: Discard an attachment you control.
instance RunMessage Disappearances where
  runMessage msg t@(Disappearances attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      players <- getRemainingPlayers
      discarded <- for players \pid -> do
        allies <- filter ((== AllyType) . (.cardType)) . (.hand) <$> getPlayer pid
        for_ allies (discardFromHand pid)
        pure $ length allies
      when (sum discarded == 0) $ raiseThreatFromDoomed 2
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard attrs pid attachments
      pure t
    _ -> Disappearances <$> runMessage msg attrs
