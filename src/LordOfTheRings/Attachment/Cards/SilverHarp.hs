module LordOfTheRings.Attachment.Cards.SilverHarp (silverHarp) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype SilverHarp = SilverHarp AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Restricted, on its card def. Attach to a spirit hero.
silverHarp :: AttachmentCard SilverHarp
silverHarp = attachment SilverHarp Cards.silverHarp & attachTo (AttachToHero $ HeroWithSphere Spirit)

-- | Response: After a card is discarded from your hand, exhaust Silver Harp to return that
-- card to your hand.
instance HasAbilities SilverHarp where
  getAbilities (SilverHarp a) =
    [ mkAbility a 1 $ ResponseAbility (DiscardedFromHandWindow After a.controller) (exhaustCost a)
    | AttachedTo _ <- [a.placement]
    ]

instance RunMessage SilverHarp where
  runMessage msg s@(SilverHarp a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ (take 1 [cid | Window _ (DiscardedFromHand _ cid) <- windows]) (returnFromDiscardToHand pid)
      pure s
    _ -> SilverHarp <$> runMessage msg a
