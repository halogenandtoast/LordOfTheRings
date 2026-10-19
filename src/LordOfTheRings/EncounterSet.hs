module LordOfTheRings.EncounterSet (module LordOfTheRings.EncounterSet) where

import LordOfTheRings.Prelude

data EncounterSet
  = DolGuldurOrcs
  | EscapeFromDolGuldur
  | JourneyAlongTheAnduin
  | PassageThroughMirkwood
  | PassageThroughMirkwoodCampaign
  | SauronsReach
  | SpidersOfMirkwood
  | Wilderlands
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
