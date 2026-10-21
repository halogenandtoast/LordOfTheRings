module LordOfTheRings.Attachment.Cards.NarvisBelt (narvisBelt) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (..))
import LordOfTheRings.Trait (Trait (Dwarf))

newtype NarvisBelt = NarvisBelt AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf hero.
narvisBelt :: AttachmentCard NarvisBelt
narvisBelt = attachment NarvisBelt Cards.narvisBelt & attachTo (AttachToHero $ HeroWithTrait Dwarf)

-- | Action: Exhaust Narvi's Belt to give attached hero a Leadership, Lore, Tactics or
-- Spirit icon until the end of the phase.
instance HasAbilities NarvisBelt where
  getAbilities (NarvisBelt a) = [mkAbility a 1 $ ActionAbility (ExhaustCost $ toTarget a)]

instance RunMessage NarvisBelt where
  runMessage msg s@(NarvisBelt a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      withHost a \host -> chooseOneM pid do
        questionLabeled "narvisBelt.chooseIcon"
        causedBy a
        for_ [(Leadership, "leadership"), (Lore, "lore"), (Tactics, "tactics"), (Spirit, "spirit")] \(sphere, key) ->
          labeled ("narvisBelt." <> key) $ phaseModifier a host (GainResourceIcon sphere)
      pure s
    _ -> NarvisBelt <$> runMessage msg a
