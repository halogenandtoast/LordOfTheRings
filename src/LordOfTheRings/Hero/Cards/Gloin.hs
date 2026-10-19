module LordOfTheRings.Hero.Cards.Gloin (gloin) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted

newtype Gloin = Gloin HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

gloin :: HeroCard Gloin
gloin = hero Gloin Cards.gloin

instance HasAbilities Gloin where
  getAbilities (Gloin a) = [mkAbility a 1 $ response $ DealtDamageWindow After (toTarget a)]

instance RunMessage Gloin where
  runMessage msg h@(Gloin attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      let damage = sum [n | Window _ (DealtDamage _ n) <- windows]
      push $ PlaceTokens (toSource attrs) (toTarget attrs) Resource damage
      pure h
    _ -> Gloin <$> runMessage msg attrs
