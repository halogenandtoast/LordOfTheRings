{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Scenario (module LordOfTheRings.Scenario, module X) where

import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Id (ScenarioId)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Runner ()
import LordOfTheRings.Scenario.Scenarios.EscapeFromDolGuldur as X
import LordOfTheRings.Scenario.Scenarios.JourneyAlongTheAnduin as X
import LordOfTheRings.Scenario.Scenarios.PassageThroughMirkwood as X
import LordOfTheRings.Scenario.Scenarios.TheHuntForGollum as X
import LordOfTheRings.Scenario.Scenarios.ConflictAtTheCarrock as X
import LordOfTheRings.Scenario.Scenarios.AJourneyToRhosgobel as X
import LordOfTheRings.Scenario.Scenarios.TheHillsOfEmynMuil as X
import LordOfTheRings.Scenario.Scenarios.TheDeadMarshes as X
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood as X
import LordOfTheRings.Scenario.Scenarios.IntoThePit as X
import LordOfTheRings.Scenario.Scenarios.FlightFromMoria as X
import LordOfTheRings.Scenario.Scenarios.TheSeventhLevel as X
import LordOfTheRings.Scenario.Scenarios.TheLongDark as X
import LordOfTheRings.Scenario.Scenarios.RoadToRivendell as X
import LordOfTheRings.Scenario.Scenarios.TheRedhornGate as X
import LordOfTheRings.Scenario.Scenarios.FoundationsOfStone as X
import LordOfTheRings.Scenario.Scenarios.ShadowAndFlame as X
import LordOfTheRings.Scenario.Scenarios.TheWatcherInTheWater as X
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfCairAndros as X
import LordOfTheRings.Scenario.Scenarios.IntoIthilien as X
import LordOfTheRings.Scenario.Scenarios.PerilInPelargir as X
import LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin as X
import LordOfTheRings.Scenario.Scenarios.TheDruadanForest as X
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale as X
import LordOfTheRings.Scenario.Scenarios.TheStewardsFear as X
import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath as X
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor as X
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc as X
import LordOfTheRings.Scenario.Scenarios.IntoFangorn as X
import LordOfTheRings.Scenario.Scenarios.TheFordsOfIsen as X
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown as X
import LordOfTheRings.Scenario.Scenarios.TheDreadRealm as X
import LordOfTheRings.Scenario.Scenarios.TheDunlandTrap as X
import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad as X
import LordOfTheRings.Scenario.Scenarios.IntrudersInChetwood as X
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret as X
import LordOfTheRings.Scenario.Scenarios.TheNinInEilph as X
import LordOfTheRings.Scenario.Scenarios.TheBattleOfCarnDum as X
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors as X
import LordOfTheRings.Scenario.Scenarios.TheThreeTrials as X
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills as X
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador as X
import LordOfTheRings.Scenario.Scenarios.DeadmensDike as X
import LordOfTheRings.Scenario.Scenarios.TheTreacheryOfRhudaur as X
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram as X
import LordOfTheRings.Scenario.Scenarios.VoyageAcrossBelegaer as X
import LordOfTheRings.Scenario.Scenarios.TheFateOfNumenor as X
import LordOfTheRings.Scenario.Scenarios.RaidOnTheGreyHavens as X
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller as X
import LordOfTheRings.Scenario.Scenarios.TheThingInTheDepths as X
import LordOfTheRings.Scenario.Scenarios.TheDrownedRuins as X
import LordOfTheRings.Scenario.Scenarios.EscapeFromUmbar as X
import LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad as X
import LordOfTheRings.Scenario.Scenarios.TheMumakil as X
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros as X
import LordOfTheRings.Scenario.Scenarios.MountGundabad as X
import LordOfTheRings.Scenario.Scenarios.TheTempleOfDoom as X
import LordOfTheRings.Scenario.Scenarios.UnderTheAshMountains as X
import LordOfTheRings.Scenario.Scenarios.DangerInDorwinion as X
import LordOfTheRings.Scenario.Scenarios.TempleOfTheDeceived as X
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven as X
import LordOfTheRings.Scenario.Scenarios.TheCityOfCorsairs as X
import LordOfTheRings.Scenario.Scenarios.TheLongArmOfMordor as X
import LordOfTheRings.Scenario.Scenarios.TheFateOfWilderland as X
import LordOfTheRings.Scenario.Scenarios.JourneyUpTheAnduin as X
import LordOfTheRings.Scenario.Scenarios.TheWitheredHeath as X
import LordOfTheRings.Scenario.Scenarios.RoamAcrossRhovanion as X
import LordOfTheRings.Scenario.Scenarios.LostInMirkwood as X
import LordOfTheRings.Scenario.Scenarios.TheRiverRunning as X
import LordOfTheRings.Scenario.Scenarios.TheCityOfUlfast as X
import LordOfTheRings.Scenario.Scenarios.BeneathTheSands as X
import LordOfTheRings.Scenario.Scenarios.DesertCrossing as X
import LordOfTheRings.Scenario.Scenarios.FireInTheNight as X
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent as X
import LordOfTheRings.Scenario.Scenarios.TheGhostOfFramsburg as X
import LordOfTheRings.Scenario.Scenarios.TheDungeonsOfCirithGurat as X
import LordOfTheRings.Scenario.Scenarios.TheKingsQuest as X
import LordOfTheRings.Scenario.Scenarios.WrathAndRuin as X
import LordOfTheRings.Scenario.Scenarios.ChallengeOfTheWainriders as X
import LordOfTheRings.Scenario.Scenarios.TheFortressOfNurn as X
import LordOfTheRings.Scenario.Scenarios.TheLandOfSorrow as X
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany as X
import LordOfTheRings.Scenario.Scenarios.TheRingGoesSouth as X
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay as X
import LordOfTheRings.Scenario.Scenarios.OverTheMistyMountainsGrim as X
import LordOfTheRings.Scenario.Types as X

-- | The scenarios a game can be dealt, by id.
allScenarios :: [Scenario]
allScenarios = [passageThroughMirkwood, journeyAlongTheAnduin, escapeFromDolGuldur, theHuntForGollum, conflictAtTheCarrock, aJourneyToRhosgobel, theHillsOfEmynMuil, theDeadMarshes, returnToMirkwood, intoThePit, flightFromMoria, theSeventhLevel, theLongDark, roadToRivendell, theRedhornGate, foundationsOfStone, shadowAndFlame, theWatcherInTheWater, theSiegeOfCairAndros, intoIthilien, perilInPelargir, encounterAtAmonDin, theDruadanForest, theMorgulVale, theStewardsFear, assaultOnOsgiliath, theBloodOfGondor, toCatchAnOrc, intoFangorn, theFordsOfIsen, theAntleredCrown, theDunlandTrap, troubleInTharbad, celebrimborsSecret, theNinInEilph, theThreeTrials, theBattleOfCarnDum, intrudersInChetwood, theWeatherHills, theWastesOfEriador, acrossTheEttenmoors, deadmensDike, theDreadRealm, theTreacheryOfRhudaur, escapeFromMountGram, voyageAcrossBelegaer, theFateOfNumenor, raidOnTheGreyHavens, flightOfTheStormcaller, theThingInTheDepths, theDrownedRuins, templeOfTheDeceived, aStormOnCobasHaven, theCityOfCorsairs, beneathTheSands, desertCrossing, escapeFromUmbar, raceAcrossHarad, theBlackSerpent, theDungeonsOfCirithGurat, theMumakil, theCrossingsOfPoros, theLongArmOfMordor, theFateOfWilderland, journeyUpTheAnduin, fireInTheNight, theKingsQuest, theGhostOfFramsburg, mountGundabad, theWitheredHeath, roamAcrossRhovanion, lostInMirkwood, theRiverRunning, theTempleOfDoom, theCityOfUlfast, underTheAshMountains, dangerInDorwinion, wrathAndRuin, challengeOfTheWainriders, theFortressOfNurn, theLandOfSorrow, thePassingOfTheGreyCompany, theRingGoesSouth, weMustAwayEreBreakOfDay, overTheMistyMountainsGrim]

lookupScenario :: ScenarioId -> Maybe Scenario
lookupScenario sid = find ((== sid) . toId) allScenarios

instance FromJSON Scenario where
  parseJSON v = do
    attrs <- parseJSON @ScenarioAttrs v
    case lookupScenario (scenarioId attrs) of
      -- the nightmare deck is the scenario's own, not the saved game's
      Just s -> pure $ overAttrs (\static -> attrs {scenarioNightmareDeck = scenarioNightmareDeck static}) s
      Nothing -> fail $ "Unknown scenario: " <> show (scenarioId attrs)

instance RunMessage Scenario where
  runMessage msg (Scenario a) = Scenario <$> runMessage msg a
