module LordOfTheRings.Hero.Cards.Dwalin (dwalin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Trait (Trait (Orc))

newtype Dwalin = Dwalin HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dwalin :: HeroCard Dwalin
dwalin = hero Dwalin Cards.dwalin

-- | Response: After Dwalin attacks and destroys an Orc enemy, lower your threat by 2.
instance HasAbilities Dwalin where
  getAbilities (Dwalin a) = [mkAbility a 1 $ response $ EnemyWithTraitDestroyedByAttackWindow After Orc (toTarget a)]

instance RunMessage Dwalin where
  runMessage msg h@(Dwalin attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      lowerThreat pid 2
      pure h
    _ -> Dwalin <$> runMessage msg attrs
