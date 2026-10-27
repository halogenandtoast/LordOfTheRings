module LordOfTheRings.Attachment.Cards.Asfaloth (asfaloth) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Trait (Trait (Noldor, Silvan))

newtype Asfaloth = Asfaloth AttachmentAttrs
  deriving anyclass (IsAttachment, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Attach to a Noldor or Silvan hero.
asfaloth :: AttachmentCard Asfaloth
asfaloth =
  attachment Asfaloth Cards.asfaloth
    & attachTo (AttachToHero $ HeroMatchAny [HeroWithTrait Noldor, HeroWithTrait Silvan])

-- | Action: Exhaust Asfaloth to place 1 progress token on any location. (2 tokens
-- instead if attached hero is Glorfindel.)
instance HasAbilities Asfaloth where
  getAbilities (Asfaloth a) =
    [restricted a 1 (exists AnyLocation) $ ActionAbility (exhaustCost a) | AttachedTo _ <- [a.placement]]

instance RunMessage Asfaloth where
  runMessage msg s@(Asfaloth a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      glorfindel <- case a.placement of
        AttachedTo (HeroTarget hid) -> selectAny (HeroWithId hid <> HeroWithTitle "Glorfindel")
        _ -> pure False
      locations <- select AnyLocation
      chooseOneM pid do
        questionLabeled "asfaloth.chooseLocation"
        causedBy a
        targets locations \l -> placeProgress a l (if glorfindel then 2 else 1)
      pure s
    _ -> Asfaloth <$> runMessage msg a
