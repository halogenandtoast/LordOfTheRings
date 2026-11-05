module LordOfTheRings.Helpers.Attachment (module LordOfTheRings.Helpers.Attachment) where

import Data.UUID qualified as UUID
import {-# SOURCE #-} LordOfTheRings.Attachment (lookupAttachment)
import LordOfTheRings.Attachment.Types
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

-- | What an attachment card can be attached to, read from its builder, for its owner, as
-- Resourceful's "a hero you control" reads.
attachMatcherFor :: PlayerId -> CardCode -> AttachMatcher
attachMatcherFor owner code =
  (toAttrs $ lookupAttachment code (CardId UUID.nil) (AttachmentId UUID.nil) owner).attachTo

-- | What must be true for the player card, an attachment, to be played by its owner.
attachmentCriteriaFor :: PlayerCard -> Criterion
attachmentCriteriaFor card =
  (toAttrs $ lookupAttachment (toCardCode card) card.id (AttachmentId UUID.nil) (fromMaybe (PlayerId UUID.nil) card.owner)).attachmentCriteria

-- | Where the attachment card can go: its matcher's hosts, less those already carrying
-- a copy of a card limited to 1 per character, and, for a restricted card, those that
-- cannot have restricted attachments.
cardHosts :: (HasGame m) => PlayerId -> CardCode -> m [Target]
cardHosts owner code = do
  -- with its text blank, an attachment has no "Attach to" to be attached by, as Amon Lhaw
  -- blanks them
  blank <- elem BlankAttachments <$> getModifiers GameTarget
  attached <- getsGame (toList . (.entities.attachments))
  let copyOn host = any (\a -> a.placement == AttachedTo host && cdName (toCardDef a) == cdName def) attached
  candidates <- attachmentHosts (attachMatcherFor owner code)
  flip filterM [h | not blank, h <- candidates, not (cdLimitOnePerHost def && copyOn h)] \host -> do
    mods <- getModifiers host
    pure
      $ (Restricted `notMember` cdKeywords def || CannotHaveRestrictedAttachments `notElem` mods)
      && and [tr `member` cdCardTraits def | CannotHaveAttachmentsWithoutTrait tr <- mods]
 where
  def = toCardDef code

{- | The matching attachment cards in the player's hand he could play now for so much less:
the unique rule lets each in, his heroes have the resources of its sphere, and it has a
host to go on, as Anvil of Gundabad's action plays an Item attachment "reducing its cost
by 1".
-}
playableAttachmentsInHand :: (HasGame m) => PlayerId -> CardMatcher -> Int -> m [Card]
playableAttachmentsInHand pid matcher reduction = do
  player <- getPlayer pid
  flip filterM [c | c@(PlayerCard _) <- player.hand, cardMatches matcher c] \c -> do
    resources <- getResourcesOfSphere pid (fromMaybe Neutral c.sphere)
    let cost = maybe 0 costAsResources c.cost
    andM [passesUnique c, pure (resources >= cost - reduction), notNull <$> cardHosts pid (toCardCode c)]

attachmentHosts :: (HasGame m) => AttachMatcher -> m [Target]
attachmentHosts matcher = filterM (fmap (\ms -> CannotHaveAttachments `notElem` ms && CannotHavePlayerAttachments `notElem` ms) . getModifiers) =<< hosts matcher

hosts :: (HasGame m) => AttachMatcher -> m [Target]
hosts = \case
  AttachToHero m -> map toTarget <$> select m
  AttachToCharacter -> (<>) <$> (map toTarget <$> select AnyHero) <*> (map toTarget <$> select AnyAlly)
  AttachToCharacterMatching hm am -> (<>) <$> (map toTarget <$> select hm) <*> (map toTarget <$> select am)
  AttachToAlly m -> map toTarget <$> select m
  AttachToEnemy m -> map toTarget <$> select m
  AttachToLocation m -> map toTarget <$> select m
  AttachToQuest -> getQuestCards
  AttachToPlayer -> map PlayerTarget <$> getRemainingPlayers
  PlayIntoStagingArea -> pure []
  PlayIntoPlayArea -> pure []

-- | What the host's modifiers take off the cost of attaching the card to it.
attachDiscount :: (HasGame m) => Card -> Target -> m Int
attachDiscount card host = do
  mods <- getModifiers host
  pure $ sum [n | ReduceCostToAttach m n <- mods, cardMatches m card]

-- | The heroes the card is attached to, as "the hero with Master of the Hunt attached"
-- reads.
heroesWithAttached :: HasGame m => CardDef -> m [Target]
heroesWithAttached def = getsGame \g ->
  [h | a <- toList g.entities.attachments, toCardCode a == toCardCode def, AttachedTo h@(HeroTarget _) <- [(toAttrs a).placement]]
