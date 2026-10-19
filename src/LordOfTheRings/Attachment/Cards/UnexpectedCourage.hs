module LordOfTheRings.Attachment.Cards.UnexpectedCourage (unexpectedCourage) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted

newtype UnexpectedCourage = UnexpectedCourage AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

unexpectedCourage :: AttachmentCard UnexpectedCourage
unexpectedCourage = attachment UnexpectedCourage Cards.unexpectedCourage

-- | Action: Exhaust Unexpected Courage to ready attached hero.
instance HasAbilities UnexpectedCourage where
  getAbilities (UnexpectedCourage a) = case a.placement of
    AttachedTo (HeroTarget hid) -> [restricted a 1 (HeroExists $ HeroWithId hid <> ExhaustedHero) $ ActionAbility (ExhaustCost $ toTarget a)]
    _ -> []

instance RunMessage UnexpectedCourage where
  runMessage msg s@(UnexpectedCourage a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> UnexpectedCourage <$> runMessage msg a
