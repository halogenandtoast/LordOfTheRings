{- | Where a location lies on a map laid out as a grid, as Temple of the Deceived's Island
Map (Arkham's 'Arkham.Location.Grid', fixed in size): columns count left to right, rows
top to bottom.
-}
module LordOfTheRings.Location.Grid (Pos (..), Direction (..), toThe, adjacentPositions, distance) where

import LordOfTheRings.Prelude

data Pos = Pos {posColumn :: Int, posRow :: Int}
  deriving stock (Show, Eq, Ord, Generic)

instance ToJSON Pos where
  toJSON (Pos c r) = object ["column" .= c, "row" .= r]

instance FromJSON Pos where
  parseJSON = withObject "Pos" \o -> Pos <$> o .: "column" <*> o .: "row"

instance HasField "column" Pos Int where
  getField = posColumn

instance HasField "row" Pos Int where
  getField = posRow

data Direction = LeftOf | RightOf | Above | Below
  deriving stock (Show, Eq, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The position next to this one in that direction.
toThe :: Direction -> Pos -> Pos
toThe d (Pos c r) = case d of
  LeftOf -> Pos (c - 1) r
  RightOf -> Pos (c + 1) r
  Above -> Pos c (r - 1)
  Below -> Pos c (r + 1)

-- | Orthogonally, not diagonally, adjacent.
adjacentPositions :: Pos -> [Pos]
adjacentPositions p = [toThe d p | d <- [minBound ..]]

-- | How many locations away, moving orthogonally.
distance :: Pos -> Pos -> Int
distance (Pos c r) (Pos c' r') = abs (c - c') + abs (r - r')
