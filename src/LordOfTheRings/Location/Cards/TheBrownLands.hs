module LordOfTheRings.Location.Cards.TheBrownLands (theBrownLands) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TheBrownLands = TheBrownLands LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theBrownLands :: LocationCard TheBrownLands
theBrownLands = location TheBrownLands Cards.theBrownLands

-- | Forced: After the players travel to The Brown Lands, place 1 progress token on it.
instance HasAbilities TheBrownLands where
  getAbilities (TheBrownLands a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage TheBrownLands where
  runMessage msg l@(TheBrownLands a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      push $ PlaceTokens (toSource a) (toTarget a) Progress 1
      pure l
    _ -> TheBrownLands <$> runMessage msg a
