{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Quest (module LordOfTheRings.Quest, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Quest.Cards.AChosenPathBeornsPath
import LordOfTheRings.Quest.Cards.EnteringTheMines
import LordOfTheRings.Quest.Cards.GoblinPatrol
import LordOfTheRings.Quest.Cards.AWayUp
import LordOfTheRings.Quest.Cards.AChosenPathDontLeaveThePath
import LordOfTheRings.Quest.Cards.AForkInTheRoad
import LordOfTheRings.Quest.Cards.AmbushOnTheShore
import LordOfTheRings.Quest.Cards.AnduinPassage
import LordOfTheRings.Quest.Cards.FliesAndSpiders
import LordOfTheRings.Quest.Cards.OutOfTheDungeons
import LordOfTheRings.Quest.Cards.TheNecromancersTower
import LordOfTheRings.Quest.Cards.ThroughTheCaverns
import LordOfTheRings.Quest.Cards.ToTheRiver
import LordOfTheRings.Quest.Cards.TheHuntBegins
import LordOfTheRings.Quest.Cards.ANewTerrorAbroad
import LordOfTheRings.Quest.Cards.OnTheTrail
import LordOfTheRings.Quest.Cards.GrimbeornsQuest
import LordOfTheRings.Quest.Cards.AgainstTheTrolls
import LordOfTheRings.Quest.Cards.TheWoundedEagle
import LordOfTheRings.Quest.Cards.RadagastsRequest
import LordOfTheRings.Quest.Cards.ReturnToRhosgobel
import LordOfTheRings.Quest.Cards.TheHillsOfEmynMuil
import LordOfTheRings.Quest.Cards.IntoTheMarshes
import LordOfTheRings.Quest.Cards.TheCapture
import LordOfTheRings.Quest.Cards.ThroughTheForest
import LordOfTheRings.Quest.Cards.EscapeAttempt
import LordOfTheRings.Quest.Cards.ToTheElvenKingsHalls
import LordOfTheRings.Quest.Cards.Ambush
import LordOfTheRings.Quest.Cards.APresenceInTheDark
import LordOfTheRings.Quest.Cards.HeadingDown
import LordOfTheRings.Quest.Cards.HeadingUp
import LordOfTheRings.Quest.Cards.AWrongTurn
import LordOfTheRings.Quest.Cards.NarrowPaths
import LordOfTheRings.Quest.Cards.HastyCouncil
import LordOfTheRings.Quest.Cards.BlockedByShadow
import LordOfTheRings.Quest.Cards.EscapeFromDarkness
import LordOfTheRings.Quest.Cards.SearchForTheChamber
import LordOfTheRings.Quest.Cards.TheFateOfBalin
import LordOfTheRings.Quest.Cards.JourneyInTheBlackPit
import LordOfTheRings.Quest.Cards.ContinuingEastward
import LordOfTheRings.Quest.Cards.AlongTheMistyMountains
import LordOfTheRings.Quest.Cards.OrcOutpost
import LordOfTheRings.Quest.Cards.ApproachingRivendell
import LordOfTheRings.Quest.Cards.UpThePass
import LordOfTheRings.Quest.Cards.Snowdrifts
import LordOfTheRings.Quest.Cards.TheMountainsPeaks
import LordOfTheRings.Quest.Cards.TheDrippingWalls
import LordOfTheRings.Quest.Cards.TheWatersEdge
import LordOfTheRings.Quest.Cards.WashedAway
import LordOfTheRings.Quest.Cards.BelowTheMines
import LordOfTheRings.Quest.Cards.OutOfTheDepths
import LordOfTheRings.Quest.Cards.NearingTheGate
import LordOfTheRings.Quest.Cards.TheRearGuard
import LordOfTheRings.Quest.Cards.LastLordOfMoria
import LordOfTheRings.Quest.Cards.ToTheWestDoor
import LordOfTheRings.Quest.Cards.TheSeethingLake
import LordOfTheRings.Quest.Cards.GatherInformation
import LordOfTheRings.Quest.Cards.DoubleBack
import LordOfTheRings.Quest.Cards.DelayTheEnemy
import LordOfTheRings.Quest.Cards.SendForAid
import LordOfTheRings.Quest.Cards.ScoutAhead
import LordOfTheRings.Quest.Cards.TheDefense
import LordOfTheRings.Quest.Cards.ReinforcingTheBanks
import LordOfTheRings.Quest.Cards.BreakthroughAtTheApproach
import LordOfTheRings.Quest.Cards.BreakthroughAtTheCitadel
import LordOfTheRings.Quest.Cards.TheLastBattle
import LordOfTheRings.Quest.Cards.AmbushInIthilien
import LordOfTheRings.Quest.Cards.SouthronCounterAttack
import LordOfTheRings.Quest.Cards.TheHiddenWay
import LordOfTheRings.Quest.Cards.ApproachingCairAndros
import LordOfTheRings.Quest.Cards.TheLeapingFishQuest
import LordOfTheRings.Quest.Cards.FightingInTheStreets
import LordOfTheRings.Quest.Cards.EscapeToTheQuays
import LordOfTheRings.Quest.Cards.SavageryOfTheOrcs
import LordOfTheRings.Quest.Cards.ProtectTheVillagers
import LordOfTheRings.Quest.Cards.ThePursuit
import LordOfTheRings.Quest.Cards.AnUntimelyEnd
import LordOfTheRings.Quest.Cards.ThePassageOut
import LordOfTheRings.Quest.Cards.TheRearguardTheMorgulVale
import LordOfTheRings.Quest.Cards.TheBetrayal
import LordOfTheRings.Quest.Cards.TheMorgulRider
import LordOfTheRings.Quest.Cards.Conspiracy
import LordOfTheRings.Quest.Cards.TheGrandDesign
import LordOfTheRings.Quest.Cards.TheConfrontation
import LordOfTheRings.Quest.Cards.RetakeTheCity
import LordOfTheRings.Quest.Cards.TheAmbush
import LordOfTheRings.Quest.Cards.Captured
import LordOfTheRings.Quest.Cards.OrdersFromOrthanc
import LordOfTheRings.Quest.Cards.SearchingForMugash
import LordOfTheRings.Quest.Cards.TheWizardsPrize
import LordOfTheRings.Quest.Cards.IntoTheWoods
import LordOfTheRings.Quest.Cards.EscapeFromFangorn
import LordOfTheRings.Quest.Cards.TheAngryForest
import LordOfTheRings.Quest.Cards.FightAtTheFords
import LordOfTheRings.Quest.Cards.DunlendingAttack
import LordOfTheRings.Quest.Cards.HoldTheFords
import LordOfTheRings.Quest.Cards.BattleForDunland
import LordOfTheRings.Quest.Cards.TheRavenClan
import LordOfTheRings.Quest.Cards.TheLastStage
import LordOfTheRings.Quest.Cards.TheCatacombsOfCarnDum
import LordOfTheRings.Quest.Cards.AngmarAwakened
import LordOfTheRings.Quest.Cards.DaechanarsFall
import LordOfTheRings.Quest.Cards.TheRoadToTharbad
import LordOfTheRings.Quest.Cards.AWellLaidTrap
import LordOfTheRings.Quest.Cards.NoWayOut
import LordOfTheRings.Quest.Cards.DoubleDealings
import LordOfTheRings.Quest.Cards.EscapeFromTharbad
import LordOfTheRings.Quest.Cards.TheRuinsOfOstInEdhil
import LordOfTheRings.Quest.Cards.TheEnemysServant
import LordOfTheRings.Quest.Cards.FleeingFromTharbad
import LordOfTheRings.Quest.Cards.ThroughTheMarshNoEndInSight
import LordOfTheRings.Quest.Cards.ThroughTheMarshAWearyPassage
import LordOfTheRings.Quest.Cards.ThroughTheMarshAForgottenLand
import LordOfTheRings.Quest.Cards.LostInTheSwanfleetImpassableMarshland
import LordOfTheRings.Quest.Cards.LostInTheSwanfleetATreacherousSwamp
import LordOfTheRings.Quest.Cards.LostInTheSwanfleetCreaturesOfAForgottenAge
import LordOfTheRings.Quest.Cards.OutOfTheSwamp
import LordOfTheRings.Quest.Cards.TheTrialsBegin
import LordOfTheRings.Quest.Cards.TheTrialOfStrength
import LordOfTheRings.Quest.Cards.TheTrialOfPerseverance
import LordOfTheRings.Quest.Cards.TheTrialOfIntuition
import LordOfTheRings.Quest.Cards.TheAntleredCrownTheThreeTrials
import LordOfTheRings.Quest.Cards.SearchTheRuins
import LordOfTheRings.Quest.Cards.TheClutchesOfCarnDum
import LordOfTheRings.Quest.Cards.MidwintersCrux
import LordOfTheRings.Quest.Cards.FuriousCharge
import LordOfTheRings.Quest.Cards.AcrossTheWastes
import LordOfTheRings.Quest.Cards.HowlingAtNight
import LordOfTheRings.Quest.Cards.BattleWithThePack
import LordOfTheRings.Quest.Cards.TheShadesOfAngmar
import LordOfTheRings.Quest.Cards.AFellWraith
import LordOfTheRings.Quest.Cards.TheShadowWorld
import LordOfTheRings.Quest.Cards.SealTheTomb
import LordOfTheRings.Quest.Cards.ThePowerOfAngmar
import LordOfTheRings.Quest.Cards.UnexpectedRescue
import LordOfTheRings.Quest.Cards.GornakhsDungeons
import LordOfTheRings.Quest.Cards.FlightFromMountGram
import LordOfTheRings.Quest.Cards.StopTheExecutioners
import LordOfTheRings.Quest.Cards.OrcRearguard
import LordOfTheRings.Quest.Cards.RescueIarion
import LordOfTheRings.Quest.Cards.OrcAmbush
import LordOfTheRings.Quest.Cards.LostInTheWilderness
import LordOfTheRings.Quest.Cards.StopTheWarParty
import LordOfTheRings.Quest.Cards.FindShelter
import LordOfTheRings.Quest.Cards.MakeCamp
import LordOfTheRings.Quest.Cards.ScatteredAmongTheHills
import LordOfTheRings.Quest.Cards.CorneredAnimals
import LordOfTheRings.Quest.Cards.IntoTheEttenmoors
import LordOfTheRings.Quest.Cards.AMiserableJourney
import LordOfTheRings.Quest.Cards.JourneysEnd
import LordOfTheRings.Quest.Cards.LieLow
import LordOfTheRings.Quest.Cards.ForageForFood
import LordOfTheRings.Quest.Cards.ScavengeForSupplies
import LordOfTheRings.Quest.Cards.SecretsOfRhudaur
import LordOfTheRings.Quest.Cards.ThaurdirsPursuit
import LordOfTheRings.Quest.Cards.QuietTheSpirits
import LordOfTheRings.Quest.Cards.SiftThroughTheDebris
import LordOfTheRings.Quest.Cards.DecipherAncientTexts
import LordOfTheRings.Quest.Cards.DepartureFromTheHavens
import LordOfTheRings.Quest.Cards.IntoTheStorm
import LordOfTheRings.Quest.Cards.CursedMists
import LordOfTheRings.Quest.Cards.CalphonsDivination
import LordOfTheRings.Quest.Cards.TheStarsGuidance
import LordOfTheRings.Quest.Cards.CorsairPursuit
import LordOfTheRings.Quest.Cards.ExploreTheIsland
import LordOfTheRings.Quest.Cards.AFatefulDiscovery
import LordOfTheRings.Quest.Cards.TheCorsairsAssault
import LordOfTheRings.Quest.Cards.SahirsAdvance
import LordOfTheRings.Quest.Cards.SwiftDeparture
import LordOfTheRings.Quest.Cards.FullSailAhead
import LordOfTheRings.Quest.Cards.TheWeatherTurnsFoul
import LordOfTheRings.Quest.Cards.TheCapeOfAndrast
import LordOfTheRings.Quest.Cards.TakeTheShip
import LordOfTheRings.Quest.Cards.ChangeOfPlans
import LordOfTheRings.Quest.Cards.DefendTheShip
import LordOfTheRings.Quest.Cards.TheGrotto
import LordOfTheRings.Quest.Cards.SahirsBetrayal
import LordOfTheRings.Quest.Cards.SearchingTheIsland
import LordOfTheRings.Quest.Cards.OutmaneuverTheEnemy
import LordOfTheRings.Quest.Cards.BattleInTheBay
import LordOfTheRings.Quest.Cards.BreakThroughTheFleet
import LordOfTheRings.Quest.Cards.TheCoastOfUmbar
import LordOfTheRings.Quest.Cards.FateOfTheDreamChaser
import LordOfTheRings.Quest.Cards.TheHavenOfUmbar
import LordOfTheRings.Quest.Cards.FinalConfrontation
import LordOfTheRings.Quest.Runner ()
import LordOfTheRings.Quest.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored quest is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Quest where
  parseJSON v = do
    attrs <- parseJSON @QuestAttrs v
    pure $ overAttrs (const attrs) $ lookupQuest (questCardCode attrs) (questCardId attrs) (questId attrs)

instance RunMessage Quest where
  runMessage msg (Quest a) = Quest <$> runMessage msg a

createQuest :: EncounterCard -> QuestId -> Quest
createQuest card = lookupQuest (toCardCode card) (ecId card)

lookupQuest :: (HasCallStack) => CardCode -> CardId -> QuestId -> Quest
lookupQuest cardCode cardId ident = case Map.lookup cardCode allQuests of
  Just (SomeQuestCard builder) -> Quest $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown quest: " <> show cardCode

newQuest :: (MonadRandom m) => EncounterCard -> m Quest
newQuest card = createQuest card <$> getRandom

-- | A player side quest, owned by the player who played it.
newSideQuest :: (MonadRandom m) => PlayerId -> PlayerCard -> m Quest
newSideQuest pid card =
  overAttrs (\a -> a {questOwner = Just pid}) . lookupQuest (toCardCode card) card.id <$> getRandom

allQuests :: Map CardCode SomeQuestCard
allQuests =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeQuestCard builder) <-
        [ SomeQuestCard fliesAndSpiders
        , SomeQuestCard enteringTheMines
        , SomeQuestCard goblinPatrol
        , SomeQuestCard aWayUp
        , SomeQuestCard aForkInTheRoad
        , SomeQuestCard aChosenPathDontLeaveThePath
        , SomeQuestCard aChosenPathBeornsPath
        , SomeQuestCard theNecromancersTower
        , SomeQuestCard throughTheCaverns
        , SomeQuestCard outOfTheDungeons
        , SomeQuestCard toTheRiver
        , SomeQuestCard anduinPassage
        , SomeQuestCard ambushOnTheShore
        , SomeQuestCard theHuntBegins
        , SomeQuestCard aNewTerrorAbroad
        , SomeQuestCard onTheTrail
        , SomeQuestCard grimbeornsQuest
        , SomeQuestCard againstTheTrolls
        , SomeQuestCard theWoundedEagle
        , SomeQuestCard radagastsRequest
        , SomeQuestCard returnToRhosgobel
        , SomeQuestCard theHillsOfEmynMuil
        , SomeQuestCard intoTheMarshes
        , SomeQuestCard theCapture
        , SomeQuestCard throughTheForest
        , SomeQuestCard escapeAttempt
        , SomeQuestCard toTheElvenKingsHalls
        , SomeQuestCard ambush
        , SomeQuestCard aPresenceInTheDark
        , SomeQuestCard headingDown
        , SomeQuestCard headingUp
        , SomeQuestCard aWrongTurn
        , SomeQuestCard narrowPaths
        , SomeQuestCard hastyCouncil
        , SomeQuestCard blockedByShadow
        , SomeQuestCard escapeFromDarkness
        , SomeQuestCard searchForTheChamber
        , SomeQuestCard theFateOfBalin
        , SomeQuestCard journeyInTheBlackPit
        , SomeQuestCard continuingEastward
        , SomeQuestCard alongTheMistyMountains
        , SomeQuestCard orcOutpost
        , SomeQuestCard approachingRivendell
        , SomeQuestCard upThePass
        , SomeQuestCard snowdrifts
        , SomeQuestCard theMountainsPeaks
        , SomeQuestCard theDrippingWalls
        , SomeQuestCard theWatersEdge
        , SomeQuestCard washedAway
        , SomeQuestCard belowTheMines
        , SomeQuestCard theEndlessCaves
        , SomeQuestCard theShiveringBank
        , SomeQuestCard oldOneLair
        , SomeQuestCard outOfTheDepths
        , SomeQuestCard nearingTheGate
        , SomeQuestCard theRearGuard
        , SomeQuestCard lastLordOfMoria
        , SomeQuestCard toTheWestDoor
        , SomeQuestCard theSeethingLake
        , SomeQuestCard gatherInformation
        , SomeQuestCard doubleBack
        , SomeQuestCard delayTheEnemy
        , SomeQuestCard sendForAid
        , SomeQuestCard scoutAhead
        , SomeQuestCard theDefense
        , SomeQuestCard reinforcingTheBanks
        , SomeQuestCard breakthroughAtTheApproach
        , SomeQuestCard breakthroughAtTheCitadel
        , SomeQuestCard theLastBattle
        , SomeQuestCard ambushInIthilien
        , SomeQuestCard southronCounterAttack
        , SomeQuestCard theHiddenWay
        , SomeQuestCard approachingCairAndros
        , SomeQuestCard theLeapingFishQuest
        , SomeQuestCard fightingInTheStreets
        , SomeQuestCard escapeToTheQuays
        , SomeQuestCard savageryOfTheOrcs
        , SomeQuestCard protectTheVillagers
        , SomeQuestCard thePursuit
        , SomeQuestCard anUntimelyEnd
        , SomeQuestCard thePassageOut
        , SomeQuestCard theRearguardTheMorgulVale
        , SomeQuestCard theBetrayal
        , SomeQuestCard theMorgulRider
        , SomeQuestCard conspiracy
        , SomeQuestCard theGrandDesign
        , SomeQuestCard theConfrontation
        , SomeQuestCard retakeTheCity
        , SomeQuestCard theAmbush
        , SomeQuestCard captured
        , SomeQuestCard ordersFromOrthanc
        , SomeQuestCard searchingForMugash
        , SomeQuestCard theWizardsPrize
        , SomeQuestCard intoTheWoods
        , SomeQuestCard escapeFromFangorn
        , SomeQuestCard theAngryForest
        , SomeQuestCard fightAtTheFords
        , SomeQuestCard dunlendingAttack
        , SomeQuestCard holdTheFords
        , SomeQuestCard battleForDunland
        , SomeQuestCard theRavenClan
        , SomeQuestCard theLastStage
        , SomeQuestCard theCatacombsOfCarnDum
        , SomeQuestCard angmarAwakened
        , SomeQuestCard daechanarsFall
        , SomeQuestCard theRoadToTharbad
        , SomeQuestCard aWellLaidTrap
        , SomeQuestCard noWayOut
        , SomeQuestCard doubleDealings
        , SomeQuestCard escapeFromTharbad
        , SomeQuestCard theRuinsOfOstInEdhil
        , SomeQuestCard theEnemysServant
        , SomeQuestCard fleeingFromTharbad
        , SomeQuestCard throughTheMarshNoEndInSight
        , SomeQuestCard throughTheMarshAWearyPassage
        , SomeQuestCard throughTheMarshAForgottenLand
        , SomeQuestCard lostInTheSwanfleetImpassableMarshland
        , SomeQuestCard lostInTheSwanfleetATreacherousSwamp
        , SomeQuestCard lostInTheSwanfleetCreaturesOfAForgottenAge
        , SomeQuestCard outOfTheSwamp
        , SomeQuestCard theTrialsBegin
        , SomeQuestCard theTrialOfStrength
        , SomeQuestCard theTrialOfPerseverance
        , SomeQuestCard theTrialOfIntuition
        , SomeQuestCard theAntleredCrownTheThreeTrials
        , SomeQuestCard searchTheRuins
        , SomeQuestCard theClutchesOfCarnDum
        , SomeQuestCard midwintersCrux
        , SomeQuestCard furiousCharge
        , SomeQuestCard acrossTheWastes
        , SomeQuestCard howlingAtNight
        , SomeQuestCard battleWithThePack
        , SomeQuestCard theShadesOfAngmar
        , SomeQuestCard aFellWraith
        , SomeQuestCard theShadowWorld
        , SomeQuestCard sealTheTomb
        , SomeQuestCard thePowerOfAngmar
        , SomeQuestCard unexpectedRescue
        , SomeQuestCard gornakhsDungeons
        , SomeQuestCard flightFromMountGram
        , SomeQuestCard stopTheExecutioners
        , SomeQuestCard orcRearguard
        , SomeQuestCard rescueIarion
        , SomeQuestCard orcAmbush
        , SomeQuestCard lostInTheWilderness
        , SomeQuestCard stopTheWarParty
        , SomeQuestCard findShelter
        , SomeQuestCard makeCamp
        , SomeQuestCard scatteredAmongTheHills
        , SomeQuestCard corneredAnimals
        , SomeQuestCard intoTheEttenmoors
        , SomeQuestCard aMiserableJourney
        , SomeQuestCard journeysEnd
        , SomeQuestCard lieLow
        , SomeQuestCard forageForFood
        , SomeQuestCard scavengeForSupplies
        , SomeQuestCard secretsOfRhudaur
        , SomeQuestCard thaurdirsPursuit
        , SomeQuestCard quietTheSpirits
        , SomeQuestCard siftThroughTheDebris
        , SomeQuestCard decipherAncientTexts
        , SomeQuestCard departureFromTheHavens
        , SomeQuestCard intoTheStorm
        , SomeQuestCard cursedMists
        , SomeQuestCard calphonsDivination
        , SomeQuestCard theStarsGuidance
        , SomeQuestCard corsairPursuit
        , SomeQuestCard exploreTheIsland
        , SomeQuestCard aFatefulDiscovery
        , SomeQuestCard theCorsairsAssault
        , SomeQuestCard sahirsAdvance
        , SomeQuestCard swiftDeparture
        , SomeQuestCard fullSailAhead
        , SomeQuestCard theWeatherTurnsFoul
        , SomeQuestCard theCapeOfAndrast
        , SomeQuestCard takeTheShip
        , SomeQuestCard changeOfPlans
        , SomeQuestCard defendTheShip
        , SomeQuestCard theGrotto
        , SomeQuestCard sahirsBetrayal
        , SomeQuestCard searchingTheIsland
        , SomeQuestCard outmaneuverTheEnemy
        , SomeQuestCard battleInTheBay
        , SomeQuestCard breakThroughTheFleet
        , SomeQuestCard theCoastOfUmbar
        , SomeQuestCard fateOfTheDreamChaser
        , SomeQuestCard theHavenOfUmbar
        , SomeQuestCard finalConfrontation
        ]
    ]
