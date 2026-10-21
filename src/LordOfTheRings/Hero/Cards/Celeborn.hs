module LordOfTheRings.Hero.Cards.Celeborn (celeborn) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Silvan))

newtype Celeborn = Celeborn HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

celeborn :: HeroCard Celeborn
celeborn = hero Celeborn Cards.celeborn

-- | Response: After a Silvan ally enters play, that ally gets +1 willpower, +1 attack and
-- +1 defense until the end of the round.
instance HasAbilities Celeborn where
  getAbilities (Celeborn a) = [mkAbility a 1 $ response $ AllyEnteredPlayWindow After Nothing (Just Silvan)]

instance RunMessage Celeborn where
  runMessage msg h@(Celeborn attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [t | Window _ (AllyEnteredPlay t _ _) <- windows] \t ->
        roundModifiers attrs t [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1]
      pure h
    _ -> Celeborn <$> runMessage msg attrs
