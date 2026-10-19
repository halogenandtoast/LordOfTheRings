{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Attachment (module LordOfTheRings.Attachment, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Attachment.Cards.BladeOfGondolin
import LordOfTheRings.Attachment.Cards.CelebriansStone
import LordOfTheRings.Attachment.Cards.CitadelPlate
import LordOfTheRings.Attachment.Cards.DarkKnowledge
import LordOfTheRings.Attachment.Cards.DunedainMark
import LordOfTheRings.Attachment.Cards.DwarvenAxe
import LordOfTheRings.Attachment.Cards.ForestSnare
import LordOfTheRings.Attachment.Cards.HornOfGondor
import LordOfTheRings.Attachment.Cards.MendorsSupport
import LordOfTheRings.Attachment.Cards.PowerInTheEarth
import LordOfTheRings.Attachment.Cards.ProtectorOfLorien
import LordOfTheRings.Attachment.Cards.SelfPreservation
import LordOfTheRings.Attachment.Cards.StewardOfGondor
import LordOfTheRings.Attachment.Cards.TheFavorOfTheLady
import LordOfTheRings.Attachment.Cards.UnexpectedCourage
import LordOfTheRings.Attachment.Runner ()
import LordOfTheRings.Attachment.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored attachment is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Attachment where
  parseJSON v = do
    attrs <- parseJSON @AttachmentAttrs v
    pure $ overAttrs (const attrs) $ lookupAttachment (attachmentCardCode attrs) (attachmentCardId attrs) (attachmentId attrs) (attachmentOwner attrs)

instance RunMessage Attachment where
  runMessage msg (Attachment a) = Attachment <$> runMessage msg a

createAttachment :: PlayerCard -> AttachmentId -> PlayerId -> Attachment
createAttachment card ident pid = lookupAttachment (toCardCode card) (pcId card) ident pid

lookupAttachment :: (HasCallStack) => CardCode -> CardId -> AttachmentId -> PlayerId -> Attachment
lookupAttachment cardCode cardId ident pid = case Map.lookup cardCode allAttachments of
  Just (SomeAttachmentCard builder) -> Attachment $ cbCardBuilder builder cardId (ident, pid)
  Nothing -> error $ "Unknown attachment: " <> show cardCode

newAttachment :: (MonadRandom m) => PlayerId -> PlayerCard -> m Attachment
newAttachment pid card = do
  ident <- getRandom
  pure $ createAttachment card ident pid

allAttachments :: Map CardCode SomeAttachmentCard
allAttachments =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeAttachmentCard builder) <-
        [ SomeAttachmentCard stewardOfGondor
        , SomeAttachmentCard celebriansStone
        , SomeAttachmentCard bladeOfGondolin
        , SomeAttachmentCard citadelPlate
        , SomeAttachmentCard dwarvenAxe
        , SomeAttachmentCard hornOfGondor
        , SomeAttachmentCard theFavorOfTheLady
        , SomeAttachmentCard powerInTheEarth
        , SomeAttachmentCard unexpectedCourage
        , SomeAttachmentCard forestSnare
        , SomeAttachmentCard protectorOfLorien
        , SomeAttachmentCard darkKnowledge
        , SomeAttachmentCard selfPreservation
        , SomeAttachmentCard dunedainMark
        , SomeAttachmentCard mendorsSupport
        ]
    ]
