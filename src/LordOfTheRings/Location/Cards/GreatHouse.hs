module LordOfTheRings.Location.Cards.GreatHouse (greatHouse) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype GreatHouse = GreatHouse LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatHouse :: LocationCard GreatHouse
greatHouse = location GreatHouse Cards.greatHouse

instance RunMessage GreatHouse where
  runMessage msg l@(GreatHouse a) = case msg of
    WhenRevealed (isTarget a -> True) -> l <$ resolveBurnDamage a
    _ -> GreatHouse <$> runMessage msg a
