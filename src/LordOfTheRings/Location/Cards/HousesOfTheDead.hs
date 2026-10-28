module LordOfTheRings.Location.Cards.HousesOfTheDead (housesOfTheDead) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype HousesOfTheDead = HousesOfTheDead LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

housesOfTheDead :: LocationCard HousesOfTheDead
housesOfTheDead = location HousesOfTheDead Cards.housesOfTheDead

-- | Underworld 2.
-- Forced: After Houses of the Dead becomes the active location, exhaust all characters.
-- ponytail: read off the players traveling there; nothing in this scenario makes it
-- active any other way.
instance HasAbilities HousesOfTheDead where
  getAbilities (HousesOfTheDead a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage HousesOfTheDead where
  runMessage msg l@(HousesOfTheDead a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      traverse_ exhaust =<< select ReadyHero
      traverse_ exhaust =<< select ReadyAlly
      pure l
    _ -> HousesOfTheDead <$> runMessage msg a
