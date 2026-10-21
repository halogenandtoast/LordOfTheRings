module LordOfTheRings.Attachment.Cards.HornOfTheMark (hornOfTheMark) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Rohan))

newtype HornOfTheMark = HornOfTheMark AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hornOfTheMark :: AttachmentCard HornOfTheMark
hornOfTheMark =
  attachment HornOfTheMark Cards.hornOfTheMark
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Rohan, HeroWithTitle "Merry"])

-- | Response: When a character leaves play, if that character shares at least one trait
-- with the attached hero, exhaust Horn of the Mark to draw 1 card.
instance HasAbilities HornOfTheMark where
  getAbilities (HornOfTheMark a) =
    [mkAbility a 1 $ ResponseAbility (LeftPlaySharingTraitWindow When host) (exhaustCost a) | AttachedTo host@(HeroTarget _) <- [a.placement]]

instance RunMessage HornOfTheMark where
  runMessage msg s@(HornOfTheMark a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      drawCards pid 1
      pure s
    _ -> HornOfTheMark <$> runMessage msg a
