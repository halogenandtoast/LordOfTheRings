module LordOfTheRings.EncounterSet (module LordOfTheRings.EncounterSet) where

import LordOfTheRings.Prelude

data EncounterSet
  = AJourneyToRhosgobel
  | ConflictAtTheCarrock
  | DeepsOfMoria
  | DolGuldurOrcs
  | EscapeFromDolGuldur
  | FlightFromMoria
  | FoundationsOfStone
  | GoblinsOfTheDeep
  | HazardsOfThePit
  | IntoThePit
  | JourneyAlongTheAnduin
  | MistyMountains
  | PassageThroughMirkwood
  | PassageThroughMirkwoodCampaign
  | PlunderingGoblins
  | ReturnToMirkwood
  | RoadToRivendell
  | SauronsReach
  | ShadowAndFlame
  | SpidersOfMirkwood
  | TheDeadMarshes
  | TheHillsOfEmynMuil
  | TheHuntForGollum
  | TheLongDark
  | TheRedhornGate
  | TheSeventhLevel
  | TheWatcherInTheWater
  | TwistsAndTurns
  | Wilderlands
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
