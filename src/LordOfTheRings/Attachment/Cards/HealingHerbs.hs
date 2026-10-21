module LordOfTheRings.Attachment.Cards.HealingHerbs (healingHerbs) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))

newtype HealingHerbs = HealingHerbs AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Lore hero.
healingHerbs :: AttachmentCard HealingHerbs
healingHerbs = attachment HealingHerbs Cards.healingHerbs & attachTo (AttachToHero $ HeroWithSphere Lore)

-- | Action: Discard Healing Herbs and exhaust attached hero to heal all damage on 1
-- character.
instance HasAbilities HealingHerbs where
  getAbilities (HealingHerbs a) = case a.placement of
    AttachedTo host -> [mkAbility a 1 $ ActionAbility (ExhaustCost host)]
    _ -> []

instance RunMessage HealingHerbs where
  runMessage msg s@(HealingHerbs a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      discard a
      damaged <- damagedCharacters AnyHero AnyAlly
      chooseOneM pid do
        questionLabeled "healingHerbs.heal"
        causedBy a
        targets damaged \c -> healAllDamage a c
      pure s
    _ -> HealingHerbs <$> runMessage msg a
