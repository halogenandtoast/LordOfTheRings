module LordOfTheRings.Attachment.Cards.PathOfNeed (pathOfNeed) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype PathOfNeed = PathOfNeed AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

pathOfNeed :: AttachmentCard PathOfNeed
pathOfNeed = attachment PathOfNeed Cards.pathOfNeed & attachTo (AttachToLocation AnyLocation)

-- | Heroes do not exhaust to attack, defend, or commit to a quest while attached location
-- is the active location.
instance HasModifiersFor PathOfNeed where
  getModifiersFor (PathOfNeed a) = case a.placement of
    AttachedTo (LocationTarget lid) -> do
      active <- selectAny (LocationWithId lid <> IsActiveLocation)
      when active do
        heroes <- select AnyHero
        for_ heroes \h -> modified a h [DoesNotExhaustToQuest, DoesNotExhaustToAttack, DoesNotExhaustToDefend]
    _ -> pure ()
