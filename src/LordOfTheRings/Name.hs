module LordOfTheRings.Name (Name (..)) where

import Data.String (IsString (..))
import LordOfTheRings.Prelude

data Name = Name Text (Maybe Text)
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance IsString Name where
  fromString s = Name (fromString s) Nothing
