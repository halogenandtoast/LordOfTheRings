module LordOfTheRings.EncounterSet (module LordOfTheRings.EncounterSet) where

import LordOfTheRings.Prelude

data EncounterSet
  = AJourneyToRhosgobel
  | AssaultOnOsgiliath
  | Brigands
  | BroodingForest
  | ConflictAtTheCarrock
  | CreaturesOfTheForest
  | DeepsOfMoria
  | DolGuldurOrcs
  | EncounterAtAmonDin
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
  | TheBloodOfGondor
  | TheDeadMarshes
  | TheDruadanForest
  | TheHillsOfEmynMuil
  | TheHuntForGollum
  | TheLongDark
  | TheMorgulVale
  | TheRedhornGate
  | TheSeventhLevel
  | TheSiegeOfCairAndros
  | TheStewardsFear
  | TheWatcherInTheWater
  | TwistsAndTurns
  | Wilderlands
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)
