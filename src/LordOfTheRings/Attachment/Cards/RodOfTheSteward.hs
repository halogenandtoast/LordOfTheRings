module LordOfTheRings.Attachment.Cards.RodOfTheSteward (rodOfTheSteward) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype RodOfTheSteward = RodOfTheSteward AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Gondor hero.
rodOfTheSteward :: AttachmentCard RodOfTheSteward
rodOfTheSteward = attachment RodOfTheSteward Cards.rodOfTheSteward & attachTo (AttachToHero $ HeroWithTrait Gondor)

-- | Action: Spend 2 resources from attached hero's resource pool to draw 1 card.
instance HasAbilities RodOfTheSteward where
  getAbilities (RodOfTheSteward a) =
    [mkAbility a 1 $ ActionAbility (ResourceCost host 2) | AttachedTo host <- [a.placement]]

instance RunMessage RodOfTheSteward where
  runMessage msg s@(RodOfTheSteward a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards a pid 1
      pure s
    _ -> RodOfTheSteward <$> runMessage msg a
