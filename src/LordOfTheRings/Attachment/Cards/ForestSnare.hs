module LordOfTheRings.Attachment.Cards.ForestSnare (forestSnare) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype ForestSnare = ForestSnare AttachmentAttrs
  deriving anyclass (IsAttachment, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestSnare :: AttachmentCard ForestSnare
forestSnare = attachment ForestSnare Cards.forestSnare & attachTo (AttachToEnemy (EnemyEngagedWith AnyPlayer))
