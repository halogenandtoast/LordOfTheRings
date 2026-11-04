module LordOfTheRings.Attachment.Cards.WoodmensPath (woodmensPath) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype WoodmensPath = WoodmensPath AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a location with no progress tokens on it.
woodmensPath :: AttachmentCard WoodmensPath
woodmensPath =
  attachment WoodmensPath Cards.woodmensPath
    & attachTo (AttachToLocation LocationWithoutProgress)

-- | While attached location is the active location, reduce its quest point value to 1.
instance HasModifiersFor WoodmensPath where
  getModifiersFor (WoodmensPath a) = case a.placement of
    AttachedTo host@(LocationTarget lid) -> do
      active <- selectAny (LocationWithId lid <> IsActiveLocation)
      when active $ modified a host [QuestPointsReducedTo 1]
    _ -> pure ()
