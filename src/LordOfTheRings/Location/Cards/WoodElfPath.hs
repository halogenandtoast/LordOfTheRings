module LordOfTheRings.Location.Cards.WoodElfPath (woodElfPath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (passGollum)

newtype WoodElfPath = WoodElfPath LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

woodElfPath :: LocationCard WoodElfPath
woodElfPath = location WoodElfPath Cards.woodElfPath

-- | Response: After the players travel to Wood Elf Path, the player guarding Gollum may
-- choose a new player to guard him.
instance HasAbilities WoodElfPath where
  getAbilities (WoodElfPath a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

instance RunMessage WoodElfPath where
  runMessage msg l@(WoodElfPath a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      passGollum True
      pure l
    _ -> WoodElfPath <$> runMessage msg a
