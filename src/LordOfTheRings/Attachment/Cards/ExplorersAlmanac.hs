module LordOfTheRings.Attachment.Cards.ExplorersAlmanac (explorersAlmanac) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ExplorersAlmanac = ExplorersAlmanac AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a location in the staging area.
explorersAlmanac :: AttachmentCard ExplorersAlmanac
explorersAlmanac = attachment ExplorersAlmanac Cards.explorersAlmanac & attachTo (AttachToLocation LocationInStagingArea)

-- | Progress from questing successfully may be placed on the attached location before it is
-- placed on the current quest.
instance HasModifiersFor ExplorersAlmanac where
  getModifiersFor (ExplorersAlmanac a) = withHost a \host -> modified a host [MayTakeQuestingProgress]
