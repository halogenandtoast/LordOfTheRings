module LordOfTheRings.Attachment.Cards.Glamdring (glamdring) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype Glamdring = Glamdring AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded (enemy or location), on its card def. Attach to a hero or Gandalf. Restricted.
glamdring :: AttachmentCard Glamdring
glamdring =
  attachment Glamdring Cards.glamdring
    & attachTo (AttachToCharacterMatching AnyHero (AllyWithTitle "Gandalf"))

-- | Attached character gets +2 [attack].
instance HasModifiersFor Glamdring where
  getModifiersFor (Glamdring a) = withHost a \host -> modified a host [AttackModifier 2]

-- | Response: After attached character destroys an enemy, draw 1 card.
instance HasAbilities Glamdring where
  getAbilities (Glamdring a) =
    [mkAbility a 1 $ response $ EnemyDestroyedByAttackWindow After (Just host) | AttachedTo host <- [a.placement]]

instance RunMessage Glamdring where
  runMessage msg s@(Glamdring a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> Glamdring <$> runMessage msg a
