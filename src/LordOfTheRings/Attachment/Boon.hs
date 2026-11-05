-- | What the Dream-chaser campaign's boon attachments share: each is removed from the
-- game as it leaves play, and has an Upgraded side once XP has upgraded it.
module LordOfTheRings.Attachment.Boon (removedAsItLeaves, isUpgradedSide) where

import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardDef)
import LordOfTheRings.Card.CardDef (cdCardTraits)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Trait (Trait (Upgraded))

-- | When it leaves play, remove it from the game: discarded, it is taken back out of its
-- owner's discard pile.
removedAsItLeaves :: Message -> AttachmentAttrs -> QueueT Message GameT AttachmentAttrs
removedAsItLeaves msg a = case msg of
  Discard _ (isTarget a -> True) -> do
    a' <- runMessage msg a
    removeFromDiscard a.owner a.cardId
    pure a'
  _ -> runMessage msg a

isUpgradedSide :: AttachmentAttrs -> Bool
isUpgradedSide a = Upgraded `member` cdCardTraits (toCardDef a)
