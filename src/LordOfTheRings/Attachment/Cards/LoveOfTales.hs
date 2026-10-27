module LordOfTheRings.Attachment.Cards.LoveOfTales (loveOfTales) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Lore))
import LordOfTheRings.Trait (Trait (Song))

newtype LoveOfTales = LoveOfTales AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a lore hero. Limit 1 per hero, as its card def says.
loveOfTales :: AttachmentCard LoveOfTales
loveOfTales = attachment LoveOfTales Cards.loveOfTales & attachTo (AttachToHero $ HeroWithSphere Lore)

-- | Response: After a Song card is played, exhaust Love of Tales to add 1 resource to
-- attached hero's resource pool.
instance HasAbilities LoveOfTales where
  getAbilities (LoveOfTales a) =
    [ mkAbility a 1 $ ResponseAbility (CardPlayedWindow After $ CardWithTrait Song) (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage LoveOfTales where
  runMessage msg s@(LoveOfTales a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> LoveOfTales <$> runMessage msg a
