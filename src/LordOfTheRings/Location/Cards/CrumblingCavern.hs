module LordOfTheRings.Location.Cards.CrumblingCavern (crumblingCavern) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype CrumblingCavern = CrumblingCavern LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

crumblingCavern :: LocationCard CrumblingCavern
crumblingCavern = location CrumblingCavern Cards.crumblingCavern

-- | Forced: After Crumbling Cavern becomes the active location, each player assigns X damage
-- among characters he controls, where X is the number of characters he controls.
instance HasAbilities CrumblingCavern where
  getAbilities (CrumblingCavern a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage CrumblingCavern where
  runMessage msg l@(CrumblingCavern a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> do
        x <- length <$> charactersControlledBy p
        when (x > 0) $ dealDamageDivided a p x
      pure l
    _ -> CrumblingCavern <$> runMessage msg a
