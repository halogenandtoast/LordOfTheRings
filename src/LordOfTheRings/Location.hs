{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Location (module LordOfTheRings.Location, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Location.Cards.BanksOfTheAnduin
import LordOfTheRings.Location.Cards.DreadfulGap
import LordOfTheRings.Location.Cards.FouledWell
import LordOfTheRings.Location.Cards.GoblinTunnels
import LordOfTheRings.Location.Cards.KneesOfTheMountain
import LordOfTheRings.Location.Cards.TurbulentWaters
import LordOfTheRings.Location.Cards.WargLair
import LordOfTheRings.Location.Cards.EastGate
import LordOfTheRings.Location.Cards.FirstHall
import LordOfTheRings.Location.Cards.BridgeOfKhazadDum
import LordOfTheRings.Location.Cards.StairsOfNain
import LordOfTheRings.Location.Cards.BranchingPaths
import LordOfTheRings.Location.Cards.LightlessPassage
import LordOfTheRings.Location.Cards.ZigilMineshaft
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
import LordOfTheRings.Location.Cards.ImpassableBog
import LordOfTheRings.Location.Cards.TheHeartOfTheMarshes
import LordOfTheRings.Location.Cards.FensAndMires
import LordOfTheRings.Location.Cards.TheSpidersRing
import LordOfTheRings.Location.Cards.DryWatercourse
import LordOfTheRings.Location.Cards.WoodmansGlade
import LordOfTheRings.Location.Cards.WoodElfPath
import LordOfTheRings.Location.Cards.TheMountainsRoots
import LordOfTheRings.Location.Cards.PlunderedArmoury
import LordOfTheRings.Location.Cards.UpperHall
import LordOfTheRings.Location.Cards.AbandonedMine
import LordOfTheRings.Location.Cards.DwarvenForge
import LordOfTheRings.Location.Cards.SilentCaverns
import LordOfTheRings.Location.Cards.TwistingPassage
import LordOfTheRings.Location.Cards.RuinedRoad
import LordOfTheRings.Location.Cards.GoblinGate
import LordOfTheRings.Location.Cards.PathlessCountry
import LordOfTheRings.Location.Cards.BarrenHills
import LordOfTheRings.Location.Cards.Caradhras
import LordOfTheRings.Location.Cards.Fanuidhol
import LordOfTheRings.Location.Cards.Celebdil
import LordOfTheRings.Location.Cards.TheDimrillStair
import LordOfTheRings.Location.Cards.RockyCrags
import LordOfTheRings.Location.Cards.DrownedTreasury
import LordOfTheRings.Location.Cards.MithrilLode
import LordOfTheRings.Location.Cards.DarkPit
import LordOfTheRings.Location.Cards.SecondHall
import LordOfTheRings.Location.Cards.SecondDeep
import LordOfTheRings.Location.Cards.DoorsOfDurin
import LordOfTheRings.Location.Cards.StairFalls
import LordOfTheRings.Location.Cards.PerilousSwamp
import LordOfTheRings.Location.Cards.MakeshiftPassage
import LordOfTheRings.Location.Cards.StagnantCreek
import LordOfTheRings.Location.Cards.TheCitadel
import LordOfTheRings.Location.Cards.TheApproach
import LordOfTheRings.Location.Cards.TheBanks
import LordOfTheRings.Location.Cards.OrcWarCamp
import LordOfTheRings.Location.Cards.IthilienRoad
import LordOfTheRings.Location.Cards.OvergrownTrail
import LordOfTheRings.Location.Cards.SecludedGlade
import LordOfTheRings.Location.Cards.TheLeapingFish
import LordOfTheRings.Location.Cards.PelargirDocks
import LordOfTheRings.Location.Cards.HarborStorehouse
import LordOfTheRings.Location.Cards.CityStreet
import LordOfTheRings.Location.Cards.MarketSquare
import LordOfTheRings.Location.Cards.Sewers
import LordOfTheRings.Location.Cards.RootsOfMindolluin
import LordOfTheRings.Location.Cards.Storehouse
import LordOfTheRings.Location.Cards.HousesOfTheDead
import LordOfTheRings.Location.Cards.TheFourthStar
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
        , SomeLocationCard dreadfulGap
        , SomeLocationCard fouledWell
        , SomeLocationCard goblinTunnels
        , SomeLocationCard kneesOfTheMountain
        , SomeLocationCard turbulentWaters
        , SomeLocationCard wargLair
        , SomeLocationCard eastGate
        , SomeLocationCard firstHall
        , SomeLocationCard bridgeOfKhazadDum
        , SomeLocationCard stairsOfNain
        , SomeLocationCard branchingPaths
        , SomeLocationCard lightlessPassage
        , SomeLocationCard zigilMineshaft
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
        , SomeLocationCard impassableBog
        , SomeLocationCard theHeartOfTheMarshes
        , SomeLocationCard fensAndMires
        , SomeLocationCard theSpidersRing
        , SomeLocationCard dryWatercourse
        , SomeLocationCard woodmansGlade
        , SomeLocationCard woodElfPath
        , SomeLocationCard theMountainsRoots
        , SomeLocationCard plunderedArmoury
        , SomeLocationCard upperHall
        , SomeLocationCard abandonedMine
        , SomeLocationCard dwarvenForge
        , SomeLocationCard silentCaverns
        , SomeLocationCard twistingPassage
        , SomeLocationCard ruinedRoad
        , SomeLocationCard goblinGate
        , SomeLocationCard pathlessCountry
        , SomeLocationCard barrenHills
        , SomeLocationCard caradhras
        , SomeLocationCard fanuidhol
        , SomeLocationCard celebdil
        , SomeLocationCard theDimrillStair
        , SomeLocationCard rockyCrags
        , SomeLocationCard drownedTreasury
        , SomeLocationCard mithrilLode
        , SomeLocationCard darkPit
        , SomeLocationCard secondHall
        , SomeLocationCard secondDeep
        , SomeLocationCard doorsOfDurin
        , SomeLocationCard stairFalls
        , SomeLocationCard perilousSwamp
        , SomeLocationCard makeshiftPassage
        , SomeLocationCard stagnantCreek
        , SomeLocationCard theCitadel
        , SomeLocationCard theApproach
        , SomeLocationCard theBanks
        , SomeLocationCard orcWarCamp
        , SomeLocationCard ithilienRoad
        , SomeLocationCard overgrownTrail
        , SomeLocationCard secludedGlade
        , SomeLocationCard theLeapingFish
        , SomeLocationCard pelargirDocks
        , SomeLocationCard harborStorehouse
        , SomeLocationCard cityStreet
        , SomeLocationCard marketSquare
        , SomeLocationCard sewers
        , SomeLocationCard rootsOfMindolluin
        , SomeLocationCard storehouse
        , SomeLocationCard housesOfTheDead
        , SomeLocationCard theFourthStar
        ]
    ]
