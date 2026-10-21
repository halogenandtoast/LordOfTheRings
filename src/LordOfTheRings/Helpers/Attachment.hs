module LordOfTheRings.Helpers.Attachment (module LordOfTheRings.Helpers.Attachment) where

import Data.UUID qualified as UUID
import LordOfTheRings.Attachment
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.HasGame
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
