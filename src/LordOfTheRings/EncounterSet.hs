module LordOfTheRings.EncounterSet (module LordOfTheRings.EncounterSet) where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Prelude

data EncounterSet = DolGuldurOrcs | PassageThroughMirkwood | SpidersOfMirkwood
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
