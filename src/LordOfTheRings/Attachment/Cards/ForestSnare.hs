module LordOfTheRings.Attachment.Cards.ForestSnare (forestSnare) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ForestSnare = ForestSnare AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestSnare :: AttachmentCard ForestSnare
forestSnare = attachment ForestSnare Cards.forestSnare & attachTo (AttachToEnemy (EnemyEngagedWith AnyPlayer))

-- | Attached enemy cannot attack.
instance HasModifiersFor ForestSnare where
  getModifiersFor (ForestSnare a) = case a.placement of
    AttachedTo host -> modified a host [CannotAttack]
    _ -> pure ()
