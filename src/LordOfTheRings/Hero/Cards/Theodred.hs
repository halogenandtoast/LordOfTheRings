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
      push
        $ questionLabelWith (ikey "label.theodred.chooseHero") (Just $ toTarget attrs) [] attrs.owner
          [targetLabel hero' [PlaceTokens (toSource attrs) (toTarget hero') Resource 1] | hero' <- heroes]
      pure h
    _ -> Theodred <$> runMessage msg attrs
