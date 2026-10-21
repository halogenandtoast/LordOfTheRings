{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Attachment.Types (module LordOfTheRings.Attachment.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasAbilities
import LordOfTheRings.Classes.HasModifiersFor
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Criteria (Criterion (NoCriteria))
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Name
import LordOfTheRings.Matcher.Types
import LordOfTheRings.Placement (Placement (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

class
  ( Typeable a
  , Show a
  , ToJSON a
  , HasAbilities a
  , HasModifiersFor a
  , RunMessage a
  , Entity a
  , EntityId a ~ AttachmentId
  , EntityAttrs a ~ AttachmentAttrs
  )
  => IsAttachment a

type AttachmentCard a = CardBuilder (AttachmentId, PlayerId) a

data Attachment = forall a. (IsAttachment a) => Attachment a

data SomeAttachmentCard = forall a. (IsAttachment a) => SomeAttachmentCard (AttachmentCard a)

data AttachmentAttrs = AttachmentAttrs
  { attachmentId :: AttachmentId
  , attachmentCardId :: CardId
  , attachmentCardCode :: CardCode
  , attachmentPlacement :: Placement
  , attachmentTokens :: Tokens
  , attachmentOwner :: PlayerId
  , attachmentExhausted :: Bool
  , attachmentAttachTo :: AttachMatcher
  , attachmentCriteria :: Criterion
  -- ^ what must be true for the attachment to be played
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''AttachmentAttrs

instance ToJSON AttachmentAttrs where
  toJSON = genericToJSON (aesonOptions $ Just "attachment")

instance FromJSON AttachmentAttrs where
  parseJSON = genericParseJSON (aesonOptions $ Just "attachment") . withDefaults [("criteria", toJSON NoCriteria)]

attachment :: (AttachmentAttrs -> a) -> CardDef -> AttachmentCard a
attachment f cardDef =
  CardBuilder
    { cbCardDef = cardDef
    , cbCardBuilder = \cardId (ident, pid) ->
        f
          AttachmentAttrs
            { attachmentId = ident
            , attachmentCardId = cardId
            , attachmentCardCode = cdCardCode cardDef
            , attachmentPlacement = Unplaced
            , attachmentTokens = mempty
            , attachmentOwner = pid
            , attachmentExhausted = False
            , attachmentAttachTo = AttachToHero AnyHero
            , attachmentCriteria = NoCriteria
            }
    }

instance Entity AttachmentAttrs where
  type EntityId AttachmentAttrs = AttachmentId
  type EntityAttrs AttachmentAttrs = AttachmentAttrs
  toId = attachmentId
  toAttrs a = a
  overAttrs f a = f a

instance Entity Attachment where
  type EntityId Attachment = AttachmentId
  type EntityAttrs Attachment = AttachmentAttrs
  toId = toId . toAttrs
  toAttrs (Attachment a) = toAttrs a
  overAttrs f (Attachment a) = Attachment $ overAttrs f a

instance HasAbilities Attachment where
  getAbilities (Attachment a) = getAbilities a

instance HasModifiersFor Attachment where
  getModifiersFor (Attachment a) = getModifiersFor a

instance Show Attachment where
  show (Attachment a) = show a

instance ToJSON Attachment where
  toJSON (Attachment a) = toJSON a

instance IsCard AttachmentAttrs where
  toCard a = PlayerCard $ MkPlayerCard {pcId = attachmentCardId a, pcCardCode = attachmentCardCode a, pcOwner = Just (attachmentOwner a)}

instance IsCard Attachment where
  toCard = toCard . toAttrs

instance HasCardCode AttachmentAttrs where
  toCardCode = attachmentCardCode

instance HasCardCode Attachment where
  toCardCode = toCardCode . toAttrs

instance HasCardDef AttachmentAttrs where
  toCardDef = toCardDef . attachmentCardCode

instance HasCardDef Attachment where
  toCardDef = toCardDef . toAttrs

instance Targetable AttachmentAttrs where
  toTarget = AttachmentTarget . attachmentId

instance Targetable Attachment where
  toTarget = toTarget . toAttrs

instance Sourceable AttachmentAttrs where
  toSource = AttachmentSource . attachmentId
  isSource attrs (AbilitySource source _) = isSource attrs source
  isSource attrs source = toSource attrs == source

instance Sourceable Attachment where
  toSource = toSource . toAttrs
  isSource = isSource . toAttrs

instance HasField "id" AttachmentAttrs AttachmentId where
  getField = attachmentId

instance HasField "id" Attachment AttachmentId where
  getField = toId

instance HasField "placement" AttachmentAttrs Placement where
  getField = attachmentPlacement

instance HasField "placement" Attachment Placement where
  getField = (.placement) . toAttrs

instance HasField "name" AttachmentAttrs Name where
  getField = cdName . toCardDef

instance HasField "name" Attachment Name where
  getField = (.name) . toAttrs

attachTo :: (IsAttachment a) => AttachMatcher -> AttachmentCard a -> AttachmentCard a
attachTo matcher = fmap (overAttrs (\a -> a {attachmentAttachTo = matcher}))

-- | What must be true for the attachment to be played, given the player who would play it.
playCriteria :: (IsAttachment a) => (PlayerId -> Criterion) -> AttachmentCard a -> AttachmentCard a
playCriteria f = fmap (overAttrs (\a -> a {attachmentCriteria = f a.owner}))

instance HasField "attachTo" AttachmentAttrs AttachMatcher where
  getField = attachmentAttachTo

instance HasField "owner" AttachmentAttrs PlayerId where
  getField = attachmentOwner

instance HasField "cardId" AttachmentAttrs CardId where
  getField = attachmentCardId
