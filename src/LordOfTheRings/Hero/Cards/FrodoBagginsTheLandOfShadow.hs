module LordOfTheRings.Hero.Cards.FrodoBagginsTheLandOfShadow (frodoBagginsTheLandOfShadow) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Fellowship))

newtype FrodoBagginsTheLandOfShadow = FrodoBagginsTheLandOfShadow HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodoBagginsTheLandOfShadow :: HeroCard FrodoBagginsTheLandOfShadow
frodoBagginsTheLandOfShadow = hero FrodoBagginsTheLandOfShadow Cards.frodoBagginsTheLandOfShadow

-- | Action: Spend 1 fellowship resource and exhaust The One Ring to give Frodo Baggins +2
-- willpower and +2 attack until the end of the round.
instance HasAbilities FrodoBagginsTheLandOfShadow where
  getAbilities (FrodoBagginsTheLandOfShadow a) =
    [mkAbility a 1 $ ActionAbility $ Costs [SpendResourcesCost Fellowship 1, ExhaustTheOneRingCost]]

instance RunMessage FrodoBagginsTheLandOfShadow where
  runMessage msg h@(FrodoBagginsTheLandOfShadow attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      roundModifiers attrs attrs [WillpowerModifier 2, AttackModifier 2]
      pure h
    _ -> FrodoBagginsTheLandOfShadow <$> runMessage msg attrs
