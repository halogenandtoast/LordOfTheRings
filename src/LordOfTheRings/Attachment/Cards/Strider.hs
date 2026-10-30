module LordOfTheRings.Attachment.Cards.Strider (strider) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Strider = Strider AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a hero.
strider :: AttachmentCard Strider
strider = attachment Strider Cards.strider

-- | While you control 2 or fewer heroes, attached hero does not exhaust to commit to the
-- quest. While you control 5 or fewer characters, attached hero gets +2 willpower.
instance HasModifiersFor Strider where
  getModifiersFor (Strider a) = withHost a \host -> do
    heroes <- selectCount (HeroControlledBy $ PlayerWithId a.controller)
    characters <- length <$> charactersControlledBy a.controller
    modified a host $ [DoesNotExhaustToCommit | heroes <= 2] <> [WillpowerModifier 2 | characters <= 5]
