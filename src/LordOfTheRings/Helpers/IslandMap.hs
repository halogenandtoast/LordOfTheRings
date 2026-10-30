-- | Reading the Island Map (Temple of the Deceived): what lies where, and around what.
module LordOfTheRings.Helpers.IslandMap (locationAt, locationToThe, adjacentLocations, locationsLeftOf, activePosition) where

import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Entities (entitiesLocations)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Location.Grid
import LordOfTheRings.Location.Types (LocationAttrs)
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Prelude

laid :: HasGame m => m [LocationAttrs]
laid = getsGame (filter (isJust . (.position)) . map toAttrs . toList . entitiesLocations . gameEntities)

-- | The location in that place on the Island Map.
locationAt :: HasGame m => Pos -> m (Maybe LocationAttrs)
locationAt pos = find ((== Just pos) . (.position)) <$> laid

-- | The location next to this one in that direction, as "the location to its right" reads.
locationToThe :: HasGame m => Direction -> LocationAttrs -> m (Maybe LocationAttrs)
locationToThe d l = maybe (pure Nothing) (locationAt . toThe d) l.position

-- | The locations orthogonally adjacent to this one.
adjacentLocations :: HasGame m => LocationAttrs -> m [LocationAttrs]
adjacentLocations l = case l.position of
  Just pos -> filter (maybe False (`elem` adjacentPositions pos) . (.position)) <$> laid
  Nothing -> pure []

-- | The locations to the left of that place, in its row.
locationsLeftOf :: HasGame m => Pos -> m [LocationAttrs]
locationsLeftOf pos = filter (maybe False (\p -> p.row == pos.row && p.column < pos.column) . (.position)) <$> laid

-- | Where the active location lies on the Island Map, if it is on it.
activePosition :: HasGame m => m (Maybe Pos)
activePosition = (>>= (.position)) . find ((== ActiveLocation) . (.placement)) <$> laid
