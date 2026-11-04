module LordOfTheRings.Attachment.Cards.Snowmane (snowmane) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Restricted))
import LordOfTheRings.Trait (Trait (Rohan))

newtype Snowmane = Snowmane AttachmentAttrs
  deriving anyclass IsAttachment
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Rohan hero. Restricted.
snowmane :: AttachmentCard Snowmane
snowmane = attachment Snowmane Cards.snowmane & attachTo (AttachToHero $ HeroWithTrait Rohan)

-- | If attached hero is Théoden, Snowmane loses the restricted keyword.
instance HasModifiersFor Snowmane where
  getModifiersFor (Snowmane a) = case a.placement of
    AttachedTo (HeroTarget hid) -> do
      theoden <- selectAny (HeroWithId hid <> HeroWithTitle "Théoden")
      when theoden $ modifySelf a [LoseKeyword Restricted]
    _ -> pure ()

-- | Response: After attached hero quests successfully, ready attached hero.
instance HasAbilities Snowmane where
  getAbilities (Snowmane a) =
    [ restricted a 1 (exists $ HeroWithId hid <> CommittedHero <> ExhaustedHero) $ response $ QuestedSuccessfullyWindow After
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage Snowmane where
  runMessage msg s@(Snowmane a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withHost a ready
      pure s
    _ -> Snowmane <$> runMessage msg a
