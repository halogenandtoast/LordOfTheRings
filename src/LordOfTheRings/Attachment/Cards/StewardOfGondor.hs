module LordOfTheRings.Attachment.Cards.StewardOfGondor (stewardOfGondor) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Gondor))

newtype StewardOfGondor = StewardOfGondor AttachmentAttrs
  deriving anyclass (IsAttachment)
  deriving newtype (Show, Eq, ToJSON, Entity)

stewardOfGondor :: AttachmentCard StewardOfGondor
stewardOfGondor = attachment StewardOfGondor Cards.stewardOfGondor

-- | Attached hero gains the Gondor trait.
instance HasModifiersFor StewardOfGondor where
  getModifiersFor (StewardOfGondor a) = case a.placement of
    AttachedTo hero -> modified a hero [GainTrait Gondor]
    _ -> pure ()

-- | Action: Exhaust Steward of Gondor to add 2 resources to the attached hero's resource pool.
instance HasAbilities StewardOfGondor where
  getAbilities (StewardOfGondor a) =
    [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a) | AttachedTo _ <- [a.placement]]

instance RunMessage StewardOfGondor where
  runMessage msg s@(StewardOfGondor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      withHost attrs \hero -> placeResources attrs hero 2
      pure s
    _ -> StewardOfGondor <$> runMessage msg attrs
