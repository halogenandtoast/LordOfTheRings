{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location (module LordOfTheRings.Location, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Location.Cards.BanksOfTheAnduin
import LordOfTheRings.Location.Cards.EnchantedStream
import LordOfTheRings.Location.Cards.EndlessCaverns
import LordOfTheRings.Location.Cards.ForestGate
import LordOfTheRings.Location.Cards.GladdenFields
import LordOfTheRings.Location.Cards.GreatForestWeb
import LordOfTheRings.Location.Cards.MountainsOfMirkwood
import LordOfTheRings.Location.Cards.NecromancersPass
import LordOfTheRings.Location.Cards.OldForestRoad
import LordOfTheRings.Location.Cards.TheBrownLands
import LordOfTheRings.Location.Cards.TheEastBight
import LordOfTheRings.Location.Cards.TowerGate
import LordOfTheRings.Location.Cards.TheOldFord
import LordOfTheRings.Location.Cards.TheEavesOfMirkwood
import LordOfTheRings.Location.Cards.RiverNinglor
import LordOfTheRings.Location.Cards.TheEastBank
import LordOfTheRings.Location.Cards.TheWestBank
import LordOfTheRings.Location.Cards.TheCarrock
import LordOfTheRings.Location.Cards.RiverLangflood
import LordOfTheRings.Location.Cards.BeePastures
import LordOfTheRings.Location.Cards.OakWoodGrove
import LordOfTheRings.Location.Cards.Rhosgobel
import LordOfTheRings.Location.Cards.ForestGrove
import LordOfTheRings.Location.Cards.AmonHen
import LordOfTheRings.Location.Cards.AmonLhaw
import LordOfTheRings.Location.Cards.TheEastWallOfRohan
import LordOfTheRings.Location.Cards.TheNorthStair
import LordOfTheRings.Location.Cards.RaurosFalls
import LordOfTheRings.Location.Cards.TheShoresOfNenHithoel
import LordOfTheRings.Location.Cards.TheOuterRidge
import LordOfTheRings.Location.Cards.TheHighlands
import LordOfTheRings.Location.Runner ()
import LordOfTheRings.Location.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored location is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Location where
  parseJSON v = do
    attrs <- parseJSON @LocationAttrs v
    pure $ overAttrs (const attrs) $ lookupLocation (locationCardCode attrs) (locationCardId attrs) (locationId attrs)

instance RunMessage Location where
  runMessage msg (Location a) = Location <$> runMessage msg a

createLocation :: EncounterCard -> LocationId -> Location
createLocation card = lookupLocation (toCardCode card) (ecId card)

lookupLocation :: (HasCallStack) => CardCode -> CardId -> LocationId -> Location
lookupLocation cardCode cardId ident = case Map.lookup cardCode allLocations of
  Just (SomeLocationCard builder) -> Location $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown location: " <> show cardCode

newLocation :: (MonadRandom m) => EncounterCard -> m Location
newLocation card = createLocation card <$> getRandom

allLocations :: Map CardCode SomeLocationCard
allLocations =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeLocationCard builder) <-
        [ SomeLocationCard greatForestWeb
        , SomeLocationCard mountainsOfMirkwood
        , SomeLocationCard theBrownLands
        , SomeLocationCard theEastBight
        , SomeLocationCard necromancersPass
        , SomeLocationCard enchantedStream
        , SomeLocationCard oldForestRoad
        , SomeLocationCard forestGate
        , SomeLocationCard endlessCaverns
        , SomeLocationCard towerGate
        , SomeLocationCard banksOfTheAnduin
        , SomeLocationCard gladdenFields
        , SomeLocationCard theOldFord
        , SomeLocationCard theEavesOfMirkwood
        , SomeLocationCard riverNinglor
        , SomeLocationCard theEastBank
        , SomeLocationCard theWestBank
        , SomeLocationCard theCarrock
        , SomeLocationCard riverLangflood
        , SomeLocationCard beePastures
        , SomeLocationCard oakWoodGrove
        , SomeLocationCard rhosgobel
        , SomeLocationCard forestGrove
        , SomeLocationCard amonHen
        , SomeLocationCard amonLhaw
        , SomeLocationCard theEastWallOfRohan
        , SomeLocationCard theNorthStair
        , SomeLocationCard raurosFalls
        , SomeLocationCard theShoresOfNenHithoel
        , SomeLocationCard theOuterRidge
        , SomeLocationCard theHighlands
        ]
    ]
