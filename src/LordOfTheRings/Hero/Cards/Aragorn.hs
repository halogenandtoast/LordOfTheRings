module LordOfTheRings.Hero.Cards.Aragorn (aragorn) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Aragorn = Aragorn HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

aragorn :: HeroCard Aragorn
aragorn = hero Aragorn Cards.aragorn

instance HasAbilities Aragorn where
  getAbilities (Aragorn a) =
    [mkAbility a 1 $ ResponseAbility (CommittedToQuestWindow After (toTarget a)) (ResourceCost (toTarget a) 1)]

instance RunMessage Aragorn where
  runMessage msg h@(Aragorn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      ready attrs
      pure h
    _ -> Aragorn <$> runMessage msg attrs
