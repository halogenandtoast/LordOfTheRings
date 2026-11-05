module LordOfTheRings.Location.Cards.DimValley (dimValley) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Sphere (Sphere (Baggins))

newtype DimValley = DimValley LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dimValley :: LocationCard DimValley
dimValley = location DimValley Cards.dimValley

instance HasAbilities DimValley where
  getAbilities (DimValley a) =
    [ anyPlayer
        $ restricted a 1 (exists $ bilbo <> ReadyHero)
        $ ActionAbility (Costs [ExhaustHeroCost bilbo, SpendResourcesCost Baggins 1])
    , mkAbility a 2 $ forced RoundEndsWindow
    ]
   where
    bilbo = HeroWithTitle "Bilbo Baggins" <> HeroControlledBy ActivePlayer

instance RunMessage DimValley where
  runMessage msg l@(DimValley attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      placeProgress attrs attrs 5
      pure l
    UseCardAbility _ (isSource attrs -> True) 2 _ -> do
      eachPlayer \p -> discardRandomFromHand p 1
      pure l
    _ -> DimValley <$> runMessage msg attrs
