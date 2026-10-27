module LordOfTheRings.EncounterSet (module LordOfTheRings.EncounterSet) where

import LordOfTheRings.Prelude

data EncounterSet
  = AJourneyToRhosgobel
  | ConflictAtTheCarrock
  | DeepsOfMoria
  | DolGuldurOrcs
  | EscapeFromDolGuldur
  | FlightFromMoria
  | GoblinsOfTheDeep
  | HazardsOfThePit
  | IntoThePit
  | JourneyAlongTheAnduin
  | MistyMountains
  | PassageThroughMirkwood
  | PassageThroughMirkwoodCampaign
  | PlunderingGoblins
  | ReturnToMirkwood
  | SauronsReach
  | SpidersOfMirkwood
  | TheDeadMarshes
  | TheHillsOfEmynMuil
  | TheHuntForGollum
  | TheSeventhLevel
  | TwistsAndTurns
  | Wilderlands
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
