module LordOfTheRings.Attachment.Cards.EnlargedQuarters (enlargedQuarters, enlargedQuartersUpgraded) where

import LordOfTheRings.Attachment.Boon
import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype EnlargedQuarters = EnlargedQuarters AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A Modification boon.
enlargedQuarters :: AttachmentCard EnlargedQuarters
enlargedQuarters = attachment EnlargedQuarters Cards.enlargedQuarters & attachTo (AttachToAlly shipObjective)

enlargedQuartersUpgraded :: AttachmentCard EnlargedQuarters
enlargedQuartersUpgraded = attachment EnlargedQuarters Cards.enlargedQuartersUpgraded & attachTo (AttachToAlly shipObjective)

-- | Attached Ship gets +3 hit points and can be committed to any Sailing test. Upgraded,
-- it counts as 1 additional character when committed to a Sailing test.
-- ponytail: "1 additional" counts the ship as 2 characters; on the Dream-chaser, which
-- already counts as more, it adds nothing.
instance HasModifiersFor EnlargedQuarters where
  getModifiersFor (EnlargedQuarters a) =
    withHost a \host -> modified a host $ [HitPointModifier 3, CommittableToAnySailingTest] <> [CountsAsSailingCharacters 2 | isUpgradedSide a]

instance RunMessage EnlargedQuarters where
  runMessage msg (EnlargedQuarters a) = EnlargedQuarters <$> removedAsItLeaves msg a
