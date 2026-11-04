module LordOfTheRings.Location.Cards.BlockedPassage (blockedPassage) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands.Helpers (isOnTrack)

newtype BlockedPassage = BlockedPassage LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Track 2.
blockedPassage :: LocationCard BlockedPassage
blockedPassage = location BlockedPassage Cards.blockedPassage

-- | While On Track is in play, progress cannot be placed on locations in the staging area.
instance HasModifiersFor BlockedPassage where
  getModifiersFor (BlockedPassage a) = whenM isOnTrack do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressLimit 0]

-- | Shadow: Discard an attachment you control.
instance RunMessage BlockedPassage where
  runMessage msg l@(BlockedPassage attrs) = case msg of
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      attachments <- getAttachmentsControlledBy pid
      unless (null attachments) $ chooseAndDiscard attrs pid attachments
      pure l
    _ -> BlockedPassage <$> runMessage msg attrs
