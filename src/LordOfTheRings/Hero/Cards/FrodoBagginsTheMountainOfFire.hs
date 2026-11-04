module LordOfTheRings.Hero.Cards.FrodoBagginsTheMountainOfFire (frodoBagginsTheMountainOfFire) where

import LordOfTheRings.Hero.CardDefs qualified as Cards
import LordOfTheRings.Hero.Import.Lifted
import LordOfTheRings.Phase (Phase (PlanningPhase))

newtype FrodoBagginsTheMountainOfFire = FrodoBagginsTheMountainOfFire HeroAttrs
  deriving anyclass (IsHero, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

frodoBagginsTheMountainOfFire :: HeroCard FrodoBagginsTheMountainOfFire
frodoBagginsTheMountainOfFire = hero FrodoBagginsTheMountainOfFire Cards.frodoBagginsTheMountainOfFire

-- | Planning Action: Exhaust The One Ring to add 1 resource to a hero's pool.
instance HasAbilities FrodoBagginsTheMountainOfFire where
  getAbilities (FrodoBagginsTheMountainOfFire a) =
    [restricted a 1 (DuringPhase PlanningPhase) $ ActionAbility ExhaustTheOneRingCost]

instance RunMessage FrodoBagginsTheMountainOfFire where
  runMessage msg h@(FrodoBagginsTheMountainOfFire attrs) = case msg of
    UseCardAbility pid (isSource attrs -> True) 1 _ -> do
      heroes <- select AnyHero
      chooseOrRunOneM pid do
        questionLabeled "frodoBaggins.chooseHeroForResource"
        causedBy attrs
        targets heroes \hid -> placeResources attrs hid 1
      pure h
    _ -> FrodoBagginsTheMountainOfFire <$> runMessage msg attrs
