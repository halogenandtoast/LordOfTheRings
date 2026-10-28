module LordOfTheRings.EncounterSet (module LordOfTheRings.EncounterSet) where

import LordOfTheRings.Prelude

data EncounterSet
  = AJourneyToRhosgobel
  | Brigands
  | BroodingForest
  | ConflictAtTheCarrock
  | CreaturesOfTheForest
  | DeepsOfMoria
  | DolGuldurOrcs
  | EscapeFromDolGuldur
  | FlightFromMoria
  | FoundationsOfStone
  | GoblinsOfTheDeep
  | HazardsOfThePit
  | IntoIthilien
  | IntoThePit
  | JourneyAlongTheAnduin
  | MistyMountains
  | MordorElite
  | PassageThroughMirkwood
  | PassageThroughMirkwoodCampaign
  | PerilInPelargir
  | PlunderingGoblins
  | RavagingOrcs
  | ReturnToMirkwood
  | RoadToRivendell
  | SauronsReach
  | ShadowAndFlame
  | Southrons
  | SpidersOfMirkwood
  | StreetsOfGondor
  | TheDeadMarshes
  | TheHillsOfEmynMuil
  | TheHuntForGollum
  | TheLongDark
  | TheRedhornGate
  | TheSeventhLevel
  | TheSiegeOfCairAndros
  | TheWatcherInTheWater
  | TwistsAndTurns
  | Wilderlands
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
