{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Attachment (module LordOfTheRings.Attachment, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Attachment.Cards.AncientMathom
import LordOfTheRings.Attachment.Cards.SpearOfTheMark
import LordOfTheRings.Attachment.Cards.SteedOfTheMark
import LordOfTheRings.Attachment.Cards.RohanWarhorse
import LordOfTheRings.Attachment.Cards.Firefoot
import LordOfTheRings.Attachment.Cards.TheRedArrow
import LordOfTheRings.Attachment.Cards.WarAxe
import LordOfTheRings.Attachment.Cards.HornOfTheMark
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
import LordOfTheRings.Attachment.Cards.SongOfKings
import LordOfTheRings.Attachment.Cards.DunedainWarning
import LordOfTheRings.Attachment.Cards.BornAloft
import LordOfTheRings.Attachment.Cards.NorAmIAStranger
import LordOfTheRings.Attachment.Cards.ABurningBrand
import LordOfTheRings.Attachment.Cards.SongOfWisdom
import LordOfTheRings.Attachment.Cards.DunedainQuest
import LordOfTheRings.Attachment.Cards.SongOfTravel
import LordOfTheRings.Attachment.Cards.DunedainCache
import LordOfTheRings.Attachment.Cards.SongOfMocking
import LordOfTheRings.Attachment.Cards.FastHitch
import LordOfTheRings.Attachment.Cards.SongOfBattle
import LordOfTheRings.Attachment.Cards.DunedainSignal
import LordOfTheRings.Attachment.Cards.SupportOfTheEagles
import LordOfTheRings.Attachment.Cards.Vilya
import LordOfTheRings.Attachment.Cards.Nenya
import LordOfTheRings.Attachment.Cards.OLorien
import LordOfTheRings.Attachment.Cards.Lembas
import LordOfTheRings.Attachment.Cards.BowOfTheGaladhrim
import LordOfTheRings.Attachment.Cards.Wingfoot
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
        , SomeAttachmentCard vilya
        , SomeAttachmentCard nenya
        , SomeAttachmentCard oLorien
        , SomeAttachmentCard lembas
        , SomeAttachmentCard bowOfTheGaladhrim
        , SomeAttachmentCard wingfoot
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
        , SomeAttachmentCard songOfKings
        , SomeAttachmentCard dunedainWarning
        , SomeAttachmentCard bornAloft
        , SomeAttachmentCard norAmIAStranger
        , SomeAttachmentCard aBurningBrand
        , SomeAttachmentCard songOfWisdom
        , SomeAttachmentCard dunedainQuest
        , SomeAttachmentCard songOfTravel
        , SomeAttachmentCard dunedainCache
        , SomeAttachmentCard songOfMocking
        , SomeAttachmentCard fastHitch
        , SomeAttachmentCard songOfBattle
        , SomeAttachmentCard dunedainSignal
        , SomeAttachmentCard supportOfTheEagles
        , SomeAttachmentCard ancientMathom
        , SomeAttachmentCard spearOfTheMark
        , SomeAttachmentCard steedOfTheMark
        , SomeAttachmentCard rohanWarhorse
        , SomeAttachmentCard firefoot
        , SomeAttachmentCard theRedArrow
        , SomeAttachmentCard warAxe
        , SomeAttachmentCard hornOfTheMark
        ]
    ]
