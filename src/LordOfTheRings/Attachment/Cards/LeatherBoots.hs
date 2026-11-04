module LordOfTheRings.Attachment.Cards.LeatherBoots (leatherBoots) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))
import LordOfTheRings.Trait (Trait (Ranger))

newtype LeatherBoots = LeatherBoots AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a [lore] or Ranger character. Limit 1 per character, on its card def.
leatherBoots :: AttachmentCard LeatherBoots
leatherBoots =
  attachment LeatherBoots Cards.leatherBoots
    & attachTo
      ( AttachToCharacterMatching
          (HeroMatchAny [HeroWithSphere Lore, HeroWithTrait Ranger])
          (AllyMatchAny [AllyWithSphere Lore, AllyWithTrait Ranger])
      )

-- | Response: After a location is revealed from the encounter deck, exhaust Leather Boots
-- to ready attached character.
instance HasAbilities LeatherBoots where
  getAbilities (LeatherBoots a) =
    [mkAbility a 1 $ ResponseAbility (LocationRevealedWindow After) (exhaustCost a) | AttachedTo _ <- [a.placement]]

instance RunMessage LeatherBoots where
  runMessage msg s@(LeatherBoots a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> LeatherBoots <$> runMessage msg a
