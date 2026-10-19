module LordOfTheRings.Hero.Cards.Theodred (theodred) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Theodred = Theodred HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theodred :: HeroCard Theodred
theodred = hero Theodred Cards.theodred

instance HasAbilities Theodred where
  getAbilities (Theodred a) = [mkAbility a 1 $ response $ CommittedToQuestWindow After (toTarget a)]

instance RunMessage Theodred where
  runMessage msg h@(Theodred attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      heroes <- select CommittedHero
      chooseOneM attrs.owner do
        questionLabeled "theodred.chooseHero"
        causedBy attrs
        targets heroes \hero' -> placeResources attrs hero' 1
      pure h
    _ -> Theodred <$> runMessage msg attrs
