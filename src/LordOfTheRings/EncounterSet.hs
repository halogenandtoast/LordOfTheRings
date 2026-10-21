module LordOfTheRings.EncounterSet (module LordOfTheRings.EncounterSet) where

import LordOfTheRings.Prelude

data EncounterSet
  = AJourneyToRhosgobel
  | ConflictAtTheCarrock
  | DolGuldurOrcs
  | EscapeFromDolGuldur
  | JourneyAlongTheAnduin
  | PassageThroughMirkwood
  | PassageThroughMirkwoodCampaign
  | ReturnToMirkwood
  | SauronsReach
  | SpidersOfMirkwood
  | TheDeadMarshes
  | TheHillsOfEmynMuil
  | TheHuntForGollum
  | Wilderlands
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
