module LordOfTheRings.Attachment.Cards.RedBookOfWestmarch (redBookOfWestmarch) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype RedBookOfWestmarch = RedBookOfWestmarch AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Hobbit hero. Restricted, on its card def.
redBookOfWestmarch :: AttachmentCard RedBookOfWestmarch
redBookOfWestmarch =
  attachment RedBookOfWestmarch Cards.redBookOfWestmarch & attachTo (AttachToHero $ HeroWithTrait Hobbit)

-- | While attached hero is committed to the quest, each Hobbit character gets +1 willpower.
instance HasModifiersFor RedBookOfWestmarch where
  getModifiersFor (RedBookOfWestmarch a) = case a.placement of
    AttachedTo (HeroTarget hid) -> do
      committed <- selectAny $ HeroWithId hid <> CommittedHero
      when committed do
        hobbits <- selectCharacters (HeroWithTrait Hobbit) (AllyWithTrait Hobbit)
        for_ hobbits \c -> modified a c [WillpowerModifier 1]
    _ -> pure ()

-- | Response: After you quest successfully, exhaust Red Book of Westmarch to add 1 resource
-- to attached hero's resource pool.
instance HasAbilities RedBookOfWestmarch where
  getAbilities (RedBookOfWestmarch a) =
    [ mkAbility a 1 $ ResponseAbility (QuestedSuccessfullyWindow After) (exhaustCost a)
    | AttachedTo (HeroTarget _) <- [a.placement]
    ]

instance RunMessage RedBookOfWestmarch where
  runMessage msg s@(RedBookOfWestmarch a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a \host -> placeResources a host 1
      pure s
    _ -> RedBookOfWestmarch <$> runMessage msg a
