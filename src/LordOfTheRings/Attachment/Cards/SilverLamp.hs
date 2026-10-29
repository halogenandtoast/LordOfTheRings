module LordOfTheRings.Attachment.Cards.SilverLamp (silverLamp) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype SilverLamp = SilverLamp AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a spirit hero.
silverLamp :: AttachmentCard SilverLamp
silverLamp = attachment SilverLamp Cards.silverLamp & attachTo (AttachToHero $ HeroWithSphere Spirit)

-- | While attached hero is ready, shadow cards dealt to enemies engaged with you are dealt
-- face up. (Shadow card effects are still resolved when resolving enemy attacks.)
instance HasAbilities SilverLamp where
  getAbilities (SilverLamp a) =
    [ restricted a 1 (exists $ HeroWithId hid <> ReadyHero) $ silent $ ShadowCardDealtWindow After a.controller
    | AttachedTo (HeroTarget hid) <- [a.placement]
    ]

instance RunMessage SilverLamp where
  runMessage msg s@(SilverLamp a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 windows -> do
      for_ [(eid, cid) | Window _ (ShadowCardDealt eid cid _) <- windows] (uncurry turnShadowCardFaceUp)
      pure s
    _ -> SilverLamp <$> runMessage msg a
