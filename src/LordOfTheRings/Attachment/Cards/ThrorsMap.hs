module LordOfTheRings.Attachment.Cards.ThrorsMap (throrsMap) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Phase (Phase (TravelPhase))

newtype ThrorsMap = ThrorsMap AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
throrsMap :: AttachmentCard ThrorsMap
throrsMap = attachment ThrorsMap Cards.throrsMap

-- | Travel Action: Discard Thrór's Map to choose a location in the staging area. Make
-- that location the active location. (If there is another active location, return it to
-- the staging area.)
instance HasAbilities ThrorsMap where
  getAbilities (ThrorsMap a) = [restricted a 1 (DuringPhase TravelPhase <> exists LocationInStagingArea) $ ActionAbility Free]

instance RunMessage ThrorsMap where
  runMessage msg s@(ThrorsMap a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discard a
      staged <- select LocationInStagingArea
      active <- select IsActiveLocation
      chooseOneM pid do
        questionLabeled "throrsMap.chooseLocation"
        causedBy a
        targets staged \lid -> do
          for_ active \old -> push $ PlaceLocation old StagingArea
          push $ PlaceLocation lid ActiveLocation
      pure s
    _ -> ThrorsMap <$> runMessage msg a
