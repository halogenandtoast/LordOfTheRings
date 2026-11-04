module LordOfTheRings.Location.Cards.BagEnd (bagEnd) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Hobbit))

newtype BagEnd = BagEnd LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

bagEnd :: LocationCard BagEnd
bagEnd = location BagEnd Cards.bagEnd

-- | Response: After Bag End leaves play, each player draws 1 card for each Hobbit hero he
-- controls.
instance HasAbilities BagEnd where
  getAbilities (BagEnd a) = [mkAbility a 1 $ response $ LocationLeftPlayWindow After a.id]

instance RunMessage BagEnd where
  runMessage msg l@(BagEnd a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        hobbits <- length <$> select (HeroControlledBy (PlayerWithId p) <> HeroWithTrait Hobbit)
        when (hobbits > 0) $ drawCards a p hobbits
      pure l
    _ -> BagEnd <$> runMessage msg a
