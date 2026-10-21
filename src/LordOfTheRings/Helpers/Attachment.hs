module LordOfTheRings.Helpers.Attachment (module LordOfTheRings.Helpers.Attachment) where

import Data.UUID qualified as UUID
import LordOfTheRings.Attachment
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Criteria (Criterion)
import LordOfTheRings.Keyword (Keyword (Restricted))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Id
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier
import LordOfTheRings.Prelude
import LordOfTheRings.Target

-- | What an attachment card can be attached to, read from its builder.
attachMatcherFor :: CardCode -> AttachMatcher
attachMatcherFor code =
  (toAttrs $ lookupAttachment code (CardId UUID.nil) (AttachmentId UUID.nil) (PlayerId UUID.nil)).attachTo

-- | What must be true for the player card, an attachment, to be played by its owner.
attachmentCriteriaFor :: PlayerCard -> Criterion
attachmentCriteriaFor card =
  (toAttrs $ lookupAttachment (toCardCode card) card.id (AttachmentId UUID.nil) (fromMaybe (PlayerId UUID.nil) card.owner)).attachmentCriteria

-- | Where the attachment card can go: its matcher's hosts, less those already carrying
-- a copy of a card limited to 1 per character, and, for a restricted card, those that
-- cannot have restricted attachments.
cardHosts :: (HasGame m) => CardCode -> m [Target]
cardHosts code = do
  attached <- getsGame (toList . (.entities.attachments))
  let copyOn host = any (\a -> a.placement == AttachedTo host && cdName (toCardDef a) == cdName def) attached
  candidates <- attachmentHosts (attachMatcherFor code)
  flip filterM [h | h <- candidates, not (cdLimitOnePerHost def && copyOn h)] \host ->
    if Restricted `member` cdKeywords def then notElem CannotHaveRestrictedAttachments <$> getModifiers host else pure True
 where
  def = toCardDef code

attachmentHosts :: (HasGame m) => AttachMatcher -> m [Target]
attachmentHosts matcher = filterM (fmap (notElem CannotHaveAttachments) . getModifiers) =<< hosts matcher

hosts :: (HasGame m) => AttachMatcher -> m [Target]
hosts = \case
  AttachToHero m -> map toTarget <$> select m
  AttachToCharacter -> (<>) <$> (map toTarget <$> select AnyHero) <*> (map toTarget <$> select AnyAlly)
  AttachToCharacterMatching hm am -> (<>) <$> (map toTarget <$> select hm) <*> (map toTarget <$> select am)
  AttachToAlly m -> map toTarget <$> select m
  AttachToEnemy m -> map toTarget <$> select m
  AttachToLocation m -> map toTarget <$> select m
  PlayIntoStagingArea -> pure []

-- | What the host's modifiers take off the cost of attaching the card to it.
attachDiscount :: (HasGame m) => Card -> Target -> m Int
attachDiscount card host = do
  mods <- getModifiers host
  pure $ sum [n | ReduceCostToAttach m n <- mods, cardMatches m card]
