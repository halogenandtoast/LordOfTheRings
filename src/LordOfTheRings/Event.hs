module LordOfTheRings.Event (module LordOfTheRings.Event, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Event.Cards.CommonCause
import LordOfTheRings.Event.Cards.GandalfsSearch
import LordOfTheRings.Event.Cards.WillOfTheWest
import LordOfTheRings.Event.Cards.DwarvenTomb
import LordOfTheRings.Event.Cards.FortuneOrFate
import LordOfTheRings.Event.Cards.MendorsSupport
import LordOfTheRings.Event.Cards.Determination
import LordOfTheRings.Event.Cards.LoreOfAncientArnor
import LordOfTheRings.Event.Cards.StandAndFight
import LordOfTheRings.Event.Cards.ATestOfWill
import LordOfTheRings.Event.Cards.HastyStroke
import LordOfTheRings.Event.Cards.RainOfArrows
import LordOfTheRings.Event.Cards.QuickStrike
import LordOfTheRings.Event.Cards.StandTogether
import LordOfTheRings.Event.Cards.ThicketOfSpears
import LordOfTheRings.Event.Cards.Feint
import LordOfTheRings.Event.Cards.SwiftStrike
import LordOfTheRings.Event.Cards.StrengthOfWill
import LordOfTheRings.Event.Cards.SecretPaths
import LordOfTheRings.Event.Cards.RadagastsCunning
import LordOfTheRings.Event.Cards.ALightInTheDark
import LordOfTheRings.Event.Cards.TheGaladhrimsGreeting
import LordOfTheRings.Event.Cards.BeornsHospitality
import LordOfTheRings.Event.Cards.LoriensWealth
import LordOfTheRings.Event.Cards.LoreOfImladris
import LordOfTheRings.Event.Cards.BladeMastery
import LordOfTheRings.Event.Cards.EverVigilant
import LordOfTheRings.Event.Cards.ForGondor
import LordOfTheRings.Event.Cards.GrimResolve
import LordOfTheRings.Event.Cards.SneakAttack
import LordOfTheRings.Event.Cards.ValiantSacrifice
import LordOfTheRings.Event.Cards.CampfireTales
import LordOfTheRings.Event.Cards.TheEaglesAreComing
import LordOfTheRings.Event.Cards.MusteringTheRohirrim
import LordOfTheRings.Event.Cards.StridersPath
import LordOfTheRings.Event.Cards.SecondBreakfast
import LordOfTheRings.Event.Cards.PartingGifts
import LordOfTheRings.Event.Cards.ToTheEyrie
import LordOfTheRings.Event.Cards.Infighting
import LordOfTheRings.Event.Cards.RearGuard
import LordOfTheRings.Event.Cards.MeneldorsFlight
import LordOfTheRings.Event.Cards.RideToRuin
import LordOfTheRings.Event.Cards.GildorsCounsel
import LordOfTheRings.Event.Cards.WeDoNotSleep
import LordOfTheRings.Event.Cards.DawnTakeYouAll
import LordOfTheRings.Event.Cards.RumourFromTheEarth
import LordOfTheRings.Event.Cards.ShadowOfThePast
import LordOfTheRings.Event.Cards.AstonishingSpeed
import LordOfTheRings.Event.Cards.ForthEorlingas
import LordOfTheRings.Event.Cards.ChargeOfTheRohirrim
import LordOfTheRings.Event.Cards.WaitNoLonger
import LordOfTheRings.Event.Cards.OathOfEorl
import LordOfTheRings.Event.Cards.TheMusterOfRohan
import LordOfTheRings.Event.Cards.ElrondsCounsel
import LordOfTheRings.Event.Cards.DaeronsRunes
import LordOfTheRings.Event.Cards.HandsUponTheBow
import LordOfTheRings.Event.Cards.TheTreePeople
import LordOfTheRings.Event.Cards.FeignedVoices
import LordOfTheRings.Event.Cards.PursuingTheEnemy
import LordOfTheRings.Event.Cards.HostOfGaladhrim
import LordOfTheRings.Event.Cards.DurinsSong
import LordOfTheRings.Event.Cards.LureOfMoria
import LordOfTheRings.Event.Cards.WeAreNotIdle
import LordOfTheRings.Event.Cards.HiddenCache
import LordOfTheRings.Event.Cards.AVeryGoodTale
import LordOfTheRings.Event.Cards.BehindStrongWalls
import LordOfTheRings.Event.Cards.MutualAccord
import LordOfTheRings.Event.Cards.WealthOfGondor
import LordOfTheRings.Event.Cards.LightTheBeacons
import LordOfTheRings.Event.Cards.AWatchfulPeace
import LordOfTheRings.Event.Cards.FoeHammer
import LordOfTheRings.Event.Cards.ConeyInATrap
import LordOfTheRings.Event.Cards.PillarsOfTheKings
import LordOfTheRings.Event.Cards.NeedDrivesThem
import LordOfTheRings.Event.Cards.HelmOfSecrecy
import LordOfTheRings.Event.Cards.TheMasterRing
import LordOfTheRings.Event.Cards.TheRulingRing
import LordOfTheRings.Event.Cards.TheRingOfPower
import LordOfTheRings.Event.Cards.EverOnward
import LordOfTheRings.Event.Cards.KhazadKhazad
import LordOfTheRings.Event.Cards.UntroubledByDarkness
import LordOfTheRings.Event.Cards.AncestralKnowledge
import LordOfTheRings.Event.Cards.TakingInitiative
import LordOfTheRings.Event.Cards.TimelyAid
import LordOfTheRings.Event.Cards.UnseenStrike
import LordOfTheRings.Event.Cards.RenewedFriendship
import LordOfTheRings.Event.Cards.NeedfulToKnow
import LordOfTheRings.Event.Cards.AGoodHarvest
import LordOfTheRings.Event.Cards.ElvenLight
import LordOfTheRings.Event.Cards.HeedTheDream
import LordOfTheRings.Event.Cards.HailOfStones
import LordOfTheRings.Event.Cards.OutOfTheWild
import LordOfTheRings.Event.Cards.TheEndComes
import LordOfTheRings.Event.Cards.GraveCairn
import LordOfTheRings.Event.Cards.ShortCut
import LordOfTheRings.Event.Cards.FreshTracks
import LordOfTheRings.Event.Cards.OutOfSight
import LordOfTheRings.Event.Cards.WordOfCommand
import LordOfTheRings.Event.Cards.HeavyStroke
import LordOfTheRings.Event.Cards.AElberethGilthoniel
import LordOfTheRings.Event.Cards.PeaceAndThought
import LordOfTheRings.Event.Cards.RiskSomeLight
import LordOfTheRings.Event.Cards.GainingStrength
import LordOfTheRings.Event.Cards.MithrandirsAdvice
import LordOfTheRings.Event.Cards.StrengthOfArms
import LordOfTheRings.Event.Cards.TrainedForWar
import LordOfTheRings.Event.Cards.AgainstTheShadow
import LordOfTheRings.Event.Cards.AdvanceWarning
import LordOfTheRings.Event.Cards.GondorianDiscipline
import LordOfTheRings.Event.Cards.SmallTarget
import LordOfTheRings.Event.Cards.HobbitSense
import LordOfTheRings.Event.Cards.MenOfTheWest
import LordOfTheRings.Event.Cards.ForestPatrol
import LordOfTheRings.Event.Cards.TheHammerStroke
import LordOfTheRings.Event.Cards.ChildrenOfTheSea
import LordOfTheRings.Event.Cards.WellEquipped
import LordOfTheRings.Event.Cards.LayOfNimrodel
import LordOfTheRings.Event.Cards.LegacyOfNumenor
import LordOfTheRings.Event.Cards.DeepKnowledge
import LordOfTheRings.Event.Cards.TheWizardsVoice
import LordOfTheRings.Event.Cards.PowerOfOrthanc
import LordOfTheRings.Event.Cards.TheSeeingStone
import LordOfTheRings.Event.Cards.WatersOfNimrodel
import LordOfTheRings.Event.Cards.SwiftAndSilent
import LordOfTheRings.Event.Cards.NoiselessMovement
import LordOfTheRings.Event.Cards.CourageAwakened
import LordOfTheRings.Event.Cards.IslandAmidPerils
import LordOfTheRings.Event.Cards.FollowMe
import LordOfTheRings.Event.Cards.ShadowsGiveWay
import LordOfTheRings.Event.Cards.FreeToChoose
import LordOfTheRings.Event.Cards.CloseCall
import LordOfTheRings.Event.Cards.TheWhiteCouncil
import LordOfTheRings.Event.Cards.MessageFromElrond
import LordOfTheRings.Event.Cards.TightenOurBelts
import LordOfTheRings.Event.Cards.RideThemDown
import LordOfTheRings.Event.Cards.DontBeHasty
import LordOfTheRings.Event.Cards.DoomHangsStill
import LordOfTheRings.Event.Cards.HoldYourGround
import LordOfTheRings.Event.Cards.LordsOfTheEldar
import LordOfTheRings.Event.Cards.QuickEars
import LordOfTheRings.Event.Cards.HourOfWrath
import LordOfTheRings.Event.Cards.TaleOfTinuviel
import LordOfTheRings.Event.Cards.DescendantsOfKings
import LordOfTheRings.Event.Cards.BoomedAndTrumpeted
import LordOfTheRings.Event.Cards.LeaveNoTrace
import LordOfTheRings.Event.Cards.DistantStars
import LordOfTheRings.Event.Cards.KeenAsLances
import LordOfTheRings.Event.Cards.DunedainMessage
import LordOfTheRings.Event.Cards.FairAndPerilous
import LordOfTheRings.Event.Cards.NoneReturn
import LordOfTheRings.Event.Cards.HopeRekindled
import LordOfTheRings.Event.Cards.Reinforcements
import LordOfTheRings.Event.Cards.HornsCry
import LordOfTheRings.Event.Cards.TheDoorIsClosed
import LordOfTheRings.Event.Cards.RangerSummons
import LordOfTheRings.Event.Cards.TirelessHunters
import LordOfTheRings.Event.Cards.ExpertTrackers
import LordOfTheRings.Event.Cards.RallyingCry
import LordOfTheRings.Event.Cards.SkywardVolley
import LordOfTheRings.Event.Cards.AnchorWatch
import LordOfTheRings.Event.Cards.TheEveningStar
import LordOfTheRings.Event.Cards.ElwingsFlight
import LordOfTheRings.Event.Cards.LastStand
import LordOfTheRings.Event.Cards.TidesOfFate
import LordOfTheRings.Event.Cards.VanishFromSight
import LordOfTheRings.Event.Cards.CaptainsWisdom
import LordOfTheRings.Event.Cards.Elevenses
import LordOfTheRings.Event.Cards.RevealedInWrath
import LordOfTheRings.Event.Cards.ScoutingParty
import LordOfTheRings.Event.Cards.ArrowsFromTheTrees
import LordOfTheRings.Event.Cards.BattleFury
import LordOfTheRings.Event.Cards.Interrogation
import LordOfTheRings.Event.Cards.TerribleToBehold
import LordOfTheRings.Event.Cards.KnifeWork
import LordOfTheRings.Event.Cards.TheHousesOfHealing
import LordOfTheRings.Event.Cards.JusticeShallBeDone
import LordOfTheRings.Event.Cards.HoldTheLine
import LordOfTheRings.Event.Cards.InspiringPresence
import LordOfTheRings.Event.Cards.GaldorsCounsel
import LordOfTheRings.Event.Cards.ComeTogether
import LordOfTheRings.Event.Cards.Command
import LordOfTheRings.Event.Cards.UnlikelyFriendship
import LordOfTheRings.Event.Cards.WellWarned
import LordOfTheRings.Event.Cards.DourHanded
import LordOfTheRings.Event.Cards.ProudHunters
import LordOfTheRings.Event.Cards.Backtrack
import LordOfTheRings.Event.Cards.HuntingParty
import LordOfTheRings.Event.Cards.TheFreePeoples
import LordOfTheRings.Event.Cards.BurstIntoSong
import LordOfTheRings.Event.Cards.OldToby
import LordOfTheRings.Event.Cards.FierceDefense
import LordOfTheRings.Event.Cards.HeirsOfEarendil
import LordOfTheRings.Event.Cards.OpenTheArmory
import LordOfTheRings.Event.Cards.BulwarkOfTheWest
import LordOfTheRings.Event.Cards.FlightToTheSea
import LordOfTheRings.Event.Cards.TheDamBursts
import LordOfTheRings.Event.Cards.WindFromTheSea
import LordOfTheRings.Event.Cards.ElfGuide
import LordOfTheRings.Event.Cards.DrinkingSong
import LordOfTheRings.Event.Cards.TheShirefolk
import LordOfTheRings.Event.Cards.TheKingsReturn
import LordOfTheRings.Event.Cards.TheGreatHunt
import LordOfTheRings.Event.Cards.GwaihirsDebt
import LordOfTheRings.Event.Cards.Reforged
import LordOfTheRings.Event.Cards.TrafficFromDale
import LordOfTheRings.Event.Cards.ToArms
import LordOfTheRings.Event.Cards.ValourOfTheNorth
import LordOfTheRings.Event.Cards.Bartering
import LordOfTheRings.Event.Cards.BeornsRage
import LordOfTheRings.Event.Cards.TheHiddenWay
import LordOfTheRings.Event.Cards.FlightOfTheEagles
import LordOfTheRings.Event.Cards.SouthAway
import LordOfTheRings.Event.Cards.QuickerThanSight
import LordOfTheRings.Event.Cards.ManTheWalls
import LordOfTheRings.Event.Cards.FamiliarLands
import LordOfTheRings.Event.Cards.SwiftAndStrong
import LordOfTheRings.Event.Cards.HornsHornsHorns
import LordOfTheRings.Event.Cards.FarSighted
import LordOfTheRings.Event.Cards.RecklessAttack
import LordOfTheRings.Event.Cards.DefiantChallenge
import LordOfTheRings.Event.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | The event a card is, built to be played; Nothing for events not implemented yet.
lookupEvent :: CardCode -> CardId -> PlayerId -> Maybe Event
lookupEvent cardCode cardId pid = case Map.lookup cardCode allEvents of
  Just (SomeEventCard builder) -> Just $ Event $ cbCardBuilder builder cardId pid
  Nothing -> Nothing

allEvents :: Map CardCode SomeEventCard
allEvents =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeEventCard builder) <-
        [ SomeEventCard everVigilant
        , SomeEventCard commonCause
        , SomeEventCard forGondor
        , SomeEventCard sneakAttack
        , SomeEventCard valiantSacrifice
        , SomeEventCard grimResolve
        , SomeEventCard gandalfsSearch
        , SomeEventCard willOfTheWest
        , SomeEventCard dwarvenTomb
        , SomeEventCard fortuneOrFate
        , SomeEventCard standAndFight
        , SomeEventCard mendorsSupport
        , SomeEventCard determination
        , SomeEventCard loreOfAncientArnor
        , SomeEventCard aTestOfWill
        , SomeEventCard hastyStroke
        , SomeEventCard rainOfArrows
        , SomeEventCard quickStrike
        , SomeEventCard standTogether
        , SomeEventCard thicketOfSpears
        , SomeEventCard feint
        , SomeEventCard swiftStrike
        , SomeEventCard strengthOfWill
        , SomeEventCard secretPaths
        , SomeEventCard radagastsCunning
        , SomeEventCard aLightInTheDark
        , SomeEventCard theGaladhrimsGreeting
        , SomeEventCard beornsHospitality
        , SomeEventCard loriensWealth
        , SomeEventCard loreOfImladris
        , SomeEventCard bladeMastery
        , SomeEventCard campfireTales
        , SomeEventCard elrondsCounsel
        , SomeEventCard daeronsRunes
        , SomeEventCard handsUponTheBow
        , SomeEventCard theTreePeople
        , SomeEventCard feignedVoices
        , SomeEventCard pursuingTheEnemy
        , SomeEventCard hostOfGaladhrim
        , SomeEventCard theEaglesAreComing
        , SomeEventCard musteringTheRohirrim
        , SomeEventCard stridersPath
        , SomeEventCard secondBreakfast
        , SomeEventCard partingGifts
        , SomeEventCard toTheEyrie
        , SomeEventCard infighting
        , SomeEventCard rearGuard
        , SomeEventCard meneldorsFlight
        , SomeEventCard rideToRuin
        , SomeEventCard gildorsCounsel
        , SomeEventCard weDoNotSleep
        , SomeEventCard dawnTakeYouAll
        , SomeEventCard rumourFromTheEarth
        , SomeEventCard shadowOfThePast
        , SomeEventCard astonishingSpeed
        , SomeEventCard forthEorlingas
        , SomeEventCard chargeOfTheRohirrim
        , SomeEventCard waitNoLonger
        , SomeEventCard oathOfEorl
        , SomeEventCard theMusterOfRohan
        , SomeEventCard durinsSong
        , SomeEventCard lureOfMoria
        , SomeEventCard weAreNotIdle
        , SomeEventCard hiddenCache
        , SomeEventCard aVeryGoodTale
        , SomeEventCard behindStrongWalls
        , SomeEventCard mutualAccord
        , SomeEventCard wealthOfGondor
        , SomeEventCard lightTheBeacons
        , SomeEventCard aWatchfulPeace
        , SomeEventCard foeHammer
        , SomeEventCard coneyInATrap
        , SomeEventCard pillarsOfTheKings
        , SomeEventCard needDrivesThem
        , SomeEventCard helmOfSecrecy
        , SomeEventCard theMasterRing
        , SomeEventCard theRulingRing
        , SomeEventCard theRingOfPower
        , SomeEventCard everOnward
        , SomeEventCard khazadKhazad
        , SomeEventCard untroubledByDarkness
        , SomeEventCard ancestralKnowledge
        , SomeEventCard takingInitiative
        , SomeEventCard timelyAid
        , SomeEventCard unseenStrike
        , SomeEventCard renewedFriendship
        , SomeEventCard needfulToKnow
        , SomeEventCard aGoodHarvest
        , SomeEventCard elvenLight
        , SomeEventCard heedTheDream
        , SomeEventCard hailOfStones
        , SomeEventCard outOfTheWild
        , SomeEventCard theEndComes
        , SomeEventCard graveCairn
        , SomeEventCard shortCut
        , SomeEventCard freshTracks
        , SomeEventCard outOfSight
        , SomeEventCard wordOfCommand
        , SomeEventCard heavyStroke
        , SomeEventCard aElberethGilthoniel
        , SomeEventCard peaceAndThought
        , SomeEventCard riskSomeLight
        , SomeEventCard gainingStrength
        , SomeEventCard mithrandirsAdvice
        , SomeEventCard strengthOfArms
        , SomeEventCard trainedForWar
        , SomeEventCard againstTheShadow
        , SomeEventCard advanceWarning
        , SomeEventCard gondorianDiscipline
        , SomeEventCard smallTarget
        , SomeEventCard hobbitSense
        , SomeEventCard menOfTheWest
        , SomeEventCard forestPatrol
        , SomeEventCard theHammerStroke
        , SomeEventCard childrenOfTheSea
        , SomeEventCard wellEquipped
        , SomeEventCard layOfNimrodel
        , SomeEventCard legacyOfNumenor
        , SomeEventCard deepKnowledge
        , SomeEventCard theWizardsVoice
        , SomeEventCard powerOfOrthanc
        , SomeEventCard theSeeingStone
        , SomeEventCard watersOfNimrodel
        , SomeEventCard swiftAndSilent
        , SomeEventCard noiselessMovement
        , SomeEventCard courageAwakened
        , SomeEventCard islandAmidPerils
        , SomeEventCard followMe
        , SomeEventCard shadowsGiveWay
        , SomeEventCard freeToChoose
        , SomeEventCard closeCall
        , SomeEventCard theWhiteCouncil
        , SomeEventCard messageFromElrond
        , SomeEventCard tightenOurBelts
        , SomeEventCard rideThemDown
        , SomeEventCard dontBeHasty
        , SomeEventCard doomHangsStill
        , SomeEventCard holdYourGround
        , SomeEventCard lordsOfTheEldar
        , SomeEventCard quickEars
        , SomeEventCard hourOfWrath
        , SomeEventCard taleOfTinuviel
        , SomeEventCard descendantsOfKings
        , SomeEventCard boomedAndTrumpeted
        , SomeEventCard leaveNoTrace
        , SomeEventCard distantStars
        , SomeEventCard keenAsLances
        , SomeEventCard dunedainMessage
        , SomeEventCard fairAndPerilous
        , SomeEventCard noneReturn
        , SomeEventCard hopeRekindled
        , SomeEventCard reinforcements
        , SomeEventCard hornsCry
        , SomeEventCard theDoorIsClosed
        , SomeEventCard rangerSummons
        , SomeEventCard tirelessHunters
        , SomeEventCard expertTrackers
        , SomeEventCard rallyingCry
        , SomeEventCard skywardVolley
        , SomeEventCard anchorWatch
        , SomeEventCard theEveningStar
        , SomeEventCard elwingsFlight
        , SomeEventCard lastStand
        , SomeEventCard tidesOfFate
        , SomeEventCard vanishFromSight
        , SomeEventCard captainsWisdom
        , SomeEventCard elevenses
        , SomeEventCard revealedInWrath
        , SomeEventCard scoutingParty
        , SomeEventCard arrowsFromTheTrees
        , SomeEventCard battleFury
        , SomeEventCard interrogation
        , SomeEventCard terribleToBehold
        , SomeEventCard knifeWork
        , SomeEventCard theHousesOfHealing
        , SomeEventCard justiceShallBeDone
        , SomeEventCard holdTheLine
        , SomeEventCard inspiringPresence
        , SomeEventCard galdorsCounselLeadership
        , SomeEventCard galdorsCounselTactics
        , SomeEventCard galdorsCounselSpirit
        , SomeEventCard galdorsCounselLore
        , SomeEventCard comeTogether
        , SomeEventCard commandLeadership
        , SomeEventCard commandLore
        , SomeEventCard commandSpirit
        , SomeEventCard commandTactics
        , SomeEventCard unlikelyFriendship
        , SomeEventCard wellWarned
        , SomeEventCard dourHanded
        , SomeEventCard proudHunters
        , SomeEventCard backtrack
        , SomeEventCard huntingParty
        , SomeEventCard theFreePeoples
        , SomeEventCard burstIntoSong
        , SomeEventCard oldToby
        , SomeEventCard fierceDefense
        , SomeEventCard heirsOfEarendil
        , SomeEventCard openTheArmory
        , SomeEventCard bulwarkOfTheWest
        , SomeEventCard flightToTheSea
        , SomeEventCard theDamBursts
        , SomeEventCard windFromTheSea
        , SomeEventCard elfGuide
        , SomeEventCard drinkingSong
        , SomeEventCard theShirefolk
        , SomeEventCard theKingsReturn
        , SomeEventCard theGreatHunt
        , SomeEventCard gwaihirsDebt
        , SomeEventCard reforged
        , SomeEventCard trafficFromDale
        , SomeEventCard toArms
        , SomeEventCard valourOfTheNorth
        , SomeEventCard bartering
        , SomeEventCard beornsRage
        , SomeEventCard theHiddenWay
        , SomeEventCard flightOfTheEagles
        , SomeEventCard southAway
        , SomeEventCard quickerThanSight
        , SomeEventCard manTheWalls
        , SomeEventCard familiarLands
        , SomeEventCard swiftAndStrong
        , SomeEventCard hornsHornsHorns
        , SomeEventCard farSighted
        , SomeEventCard recklessAttack
        , SomeEventCard defiantChallenge
        ]
    ]
