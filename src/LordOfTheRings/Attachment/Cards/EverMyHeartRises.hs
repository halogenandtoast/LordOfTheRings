module LordOfTheRings.Attachment.Cards.EverMyHeartRises (everMyHeartRises) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Dwarf, Mountain, Underground))

newtype EverMyHeartRises = EverMyHeartRises AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Dwarf character.
everMyHeartRises :: AttachmentCard EverMyHeartRises
everMyHeartRises =
  attachment EverMyHeartRises Cards.everMyHeartRises
    & attachTo (AttachToCharacterMatching (HeroWithTrait Dwarf) (AllyWithTrait Dwarf))

-- | Response: After you travel to a Mountain or Underground location, ready attached
-- character and reduce your threat by 1.
instance HasAbilities EverMyHeartRises where
  getAbilities (EverMyHeartRises a) =
    [ mkAbility a 1 $ response $ TraveledToWindow After (LocationMatchAny [LocationWithTrait Mountain, LocationWithTrait Underground])
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage EverMyHeartRises where
  runMessage msg s@(EverMyHeartRises a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      withHost a ready
      lowerThreat pid 1
      pure s
    _ -> EverMyHeartRises <$> runMessage msg a
