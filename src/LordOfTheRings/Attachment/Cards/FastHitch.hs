module LordOfTheRings.Attachment.Cards.FastHitch (fastHitch) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype FastHitch = FastHitch AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Hobbit character.
fastHitch :: AttachmentCard FastHitch
fastHitch = attachment FastHitch Cards.fastHitch & attachTo (AttachToCharacterMatching (HeroWithTrait Hobbit) (AllyWithTrait Hobbit))

-- | Action: Exhaust Fast Hitch to ready attached character.
instance HasAbilities FastHitch where
  getAbilities (FastHitch a) = [mkAbility a 1 $ ActionAbility (exhaustCost a) | AttachedTo _ <- [a.placement]]

instance RunMessage FastHitch where
  runMessage msg s@(FastHitch a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> FastHitch <$> runMessage msg a
