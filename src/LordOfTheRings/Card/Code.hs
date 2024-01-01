module LordOfTheRings.Card.Code (CardCode) where

import Data.String (IsString)
import LordOfTheRings.Prelude

newtype CardCode = CardCode Text
  deriving newtype (Ord, Eq, IsString, Show, ToJSON, FromJSON)
