module LordOfTheRings.Treachery.Cards.TorrentOfHail (torrentOfHail) where

import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TorrentOfHail = TorrentOfHail TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

torrentOfHail :: TreacheryCard TorrentOfHail
torrentOfHail = treachery TorrentOfHail Cards.torrentOfHail

-- | When Revealed: Deal 1 damage to each questing character. If it is Night, discard an
-- attachment from each character damaged by this effect.
-- Shadow: Discard 1 attachment you control (2 attachments instead if it is Night).
instance RunMessage TorrentOfHail where
  runMessage msg t@(TorrentOfHail attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      night <- isNight
      questing <- selectCharacters CommittedHero CommittedAlly
      for_ questing \c -> dealDamage attrs c 1
      when night $ for_ questing \c -> do
        attachments <- getAttachmentsOn c
        controller <- characterController c
        for_ controller \p -> chooseAndDiscard attrs p attachments
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      n <- bool 1 2 <$> isNight
      attachments <- getAttachmentsControlledBy pid
      let source = CardIdSource cid
      if
        | length attachments <= n -> for_ attachments discard
        | n == 1 -> chooseAndDiscard source pid attachments
        | otherwise -> chooseOneM pid do
          questionLabeled "torrentOfHail.discard"
          causedBy attrs
          targets attachments \x -> do
            discard x
            chooseAndDiscard source pid (filter (/= x) attachments)
      pure t
    _ -> TorrentOfHail <$> runMessage msg attrs
