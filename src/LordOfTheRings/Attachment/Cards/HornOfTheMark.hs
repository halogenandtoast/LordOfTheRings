module LordOfTheRings.Attachment.Cards.HornOfTheMark (hornOfTheMark) where

import LordOfTheRings.Attachment.CardDefs qualified as Cards
import LordOfTheRings.Attachment.Import.Lifted
import LordOfTheRings.Card (toCardDef)
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
-- ponytail: offered after any character leaves, the trait is checked as it resolves;
-- a window matcher that could read the host's traits would only offer it when it draws
instance HasAbilities HornOfTheMark where
  getAbilities (HornOfTheMark a) =
    [mkAbility a 1 $ ResponseAbility (CharacterLeftPlayWindow After AnyCard) (ExhaustCost $ toTarget a) | AttachedTo (HeroTarget _) <- [a.placement]]

instance RunMessage HornOfTheMark where
  runMessage msg s@(HornOfTheMark a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 windows -> do
      for_ [hid | AttachedTo (HeroTarget hid) <- [a.placement]] \hid -> do
        hero' <- getHero hid
        traits <- filterM (\t -> hasTrait t (toTarget hid) (toCardDef hero')) (toList $ unions [ts | Window _ (LeftPlay _ _ ts) <- windows])
        when (notNull traits) $ drawCards pid 1
      pure s
    _ -> HornOfTheMark <$> runMessage msg a
