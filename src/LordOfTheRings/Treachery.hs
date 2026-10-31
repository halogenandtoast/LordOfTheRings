{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Treachery (module LordOfTheRings.Treachery, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Treachery.Cards.CaughtInAWeb
import LordOfTheRings.Treachery.Cards.CaveIn
import LordOfTheRings.Treachery.Cards.CrumblingRuin
import LordOfTheRings.Treachery.Cards.DarkAndDreadful
import LordOfTheRings.Treachery.Cards.SuddenPitfall
import LordOfTheRings.Treachery.Cards.WatchfulEyes
import LordOfTheRings.Treachery.Cards.SignsOfConflict
import LordOfTheRings.Treachery.Cards.ManyRoads
import LordOfTheRings.Treachery.Cards.BurningLow
import LordOfTheRings.Treachery.Cards.Despair
import LordOfTheRings.Treachery.Cards.DrivenByShadow
import LordOfTheRings.Treachery.Cards.EvilStorm
import LordOfTheRings.Treachery.Cards.EyesOfTheForest
import LordOfTheRings.Treachery.Cards.IronShackles
import LordOfTheRings.Treachery.Cards.LingeringVenom
import LordOfTheRings.Treachery.Cards.MassingAtNight
import LordOfTheRings.Treachery.Cards.PursuedByShadow
import LordOfTheRings.Treachery.Cards.TheNecromancersReach
import LordOfTheRings.Treachery.Cards.TreacherousFog
import LordOfTheRings.Treachery.Cards.UnderTheShadow
import LordOfTheRings.Treachery.Cards.FalseLead
import LordOfTheRings.Treachery.Cards.Flooding
import LordOfTheRings.Treachery.Cards.OldWivesTales
import LordOfTheRings.Treachery.Cards.AFrightenedBeast
import LordOfTheRings.Treachery.Cards.Sacked
import LordOfTheRings.Treachery.Cards.RoastedSlowly
import LordOfTheRings.Treachery.Cards.Exhaustion
import LordOfTheRings.Treachery.Cards.SwarmingInsect
import LordOfTheRings.Treachery.Cards.FesteringWounds
import LordOfTheRings.Treachery.Cards.ImpassableChasm
import LordOfTheRings.Treachery.Cards.Rockslide
import LordOfTheRings.Treachery.Cards.SlickFooting
import LordOfTheRings.Treachery.Cards.AWispOfPaleSheen
import LordOfTheRings.Treachery.Cards.Nightfall
import LordOfTheRings.Treachery.Cards.ThroughTheMist
import LordOfTheRings.Treachery.Cards.TheLightsOfTheDead
import LordOfTheRings.Treachery.Cards.GollumsAnguish
import LordOfTheRings.Treachery.Cards.GollumsBite
import LordOfTheRings.Treachery.Cards.WastedProvisions
import LordOfTheRings.Treachery.Cards.NewDevilry
import LordOfTheRings.Treachery.Cards.ShadowOfFear
import LordOfTheRings.Treachery.Cards.AFoeBeyond
import LordOfTheRings.Treachery.Cards.ChanceEncounter
import LordOfTheRings.Treachery.Cards.MassingInTheDeep
import LordOfTheRings.Treachery.Cards.UndisturbedBones
import LordOfTheRings.Treachery.Cards.BitterWind
import LordOfTheRings.Treachery.Cards.HiddenThreat
import LordOfTheRings.Treachery.Cards.Fatigue
import LordOfTheRings.Treachery.Cards.FoulAir
import LordOfTheRings.Treachery.Cards.GatheringGround
import LordOfTheRings.Treachery.Cards.VastAndIntricate
import LordOfTheRings.Treachery.Cards.SleepingSentry
import LordOfTheRings.Treachery.Cards.FollowedByNight
import LordOfTheRings.Treachery.Cards.OrcAmbush
import LordOfTheRings.Treachery.Cards.FellVoices
import LordOfTheRings.Treachery.Cards.FallenStones
import LordOfTheRings.Treachery.Cards.Snowstorm
import LordOfTheRings.Treachery.Cards.FreezingCold
import LordOfTheRings.Treachery.Cards.Avalanche
import LordOfTheRings.Treachery.Cards.DeepDeepDark
import LordOfTheRings.Treachery.Cards.LostAndAlone
import LordOfTheRings.Treachery.Cards.FiresInTheDeep
import LordOfTheRings.Treachery.Cards.InnerFlame
import LordOfTheRings.Treachery.Cards.InnerShadow
import LordOfTheRings.Treachery.Cards.WhipLash
import LordOfTheRings.Treachery.Cards.LeapingFlame
import LordOfTheRings.Treachery.Cards.CounterSpell
import LordOfTheRings.Treachery.Cards.FierySword
import LordOfTheRings.Treachery.Cards.ManyThongedWhip
import LordOfTheRings.Treachery.Cards.IllPurpose
import LordOfTheRings.Treachery.Cards.DisturbedWaters
import LordOfTheRings.Treachery.Cards.Wrapped
import LordOfTheRings.Treachery.Cards.OrcAssault
import LordOfTheRings.Treachery.Cards.TheMastersMalice
import LordOfTheRings.Treachery.Cards.ThePowerOfMordor
import LordOfTheRings.Treachery.Cards.ScourgeOfMordor
import LordOfTheRings.Treachery.Cards.BlockingWargs
import LordOfTheRings.Treachery.Cards.LostCompany
import LordOfTheRings.Treachery.Cards.WatcherInTheWoods
import LordOfTheRings.Treachery.Cards.SouthronSupport
import LordOfTheRings.Treachery.Cards.CollateralDamage
import LordOfTheRings.Treachery.Cards.LostInTheCity
import LordOfTheRings.Treachery.Cards.LocalTrouble
import LordOfTheRings.Treachery.Cards.LurkingInShadows
import LordOfTheRings.Treachery.Cards.BurntHomestead
import LordOfTheRings.Treachery.Cards.TrappedInside
import LordOfTheRings.Treachery.Cards.Panicked
import LordOfTheRings.Treachery.Cards.MenInTheDark
import LordOfTheRings.Treachery.Cards.StarsInSky
import LordOfTheRings.Treachery.Cards.LeavesOnTree
import LordOfTheRings.Treachery.Cards.ImpenetrableFog
import LordOfTheRings.Treachery.Cards.SleeplessMalice
import LordOfTheRings.Treachery.Cards.TerrorDrivesThem
import LordOfTheRings.Treachery.Cards.TheDeadCityLooms
import LordOfTheRings.Treachery.Cards.UnwelcomeDiscovery
import LordOfTheRings.Treachery.Cards.AKnifeInTheBack
import LordOfTheRings.Treachery.Cards.FalseLeadTheStewardsFear
import LordOfTheRings.Treachery.Cards.PinnedDown
import LordOfTheRings.Treachery.Cards.StreetFighting
import LordOfTheRings.Treachery.Cards.CounterAttack
import LordOfTheRings.Treachery.Cards.LyingInWait
import LordOfTheRings.Treachery.Cards.MordorLooms
import LordOfTheRings.Treachery.Cards.ConflictAtTheCrossroads
import LordOfTheRings.Treachery.Cards.OrcTerritory
import LordOfTheRings.Treachery.Cards.TheForestsMalice
import LordOfTheRings.Treachery.Cards.PillagingAndBurning
import LordOfTheRings.Treachery.Cards.DownFromTheHills
import LordOfTheRings.Treachery.Cards.IllTidings
import LordOfTheRings.Treachery.Cards.OrcHuntingParty
import LordOfTheRings.Treachery.Cards.OldHatreds
import LordOfTheRings.Treachery.Cards.WildMenOfDunland
import LordOfTheRings.Treachery.Cards.TurnedAround
import LordOfTheRings.Treachery.Cards.TakeCover
import LordOfTheRings.Treachery.Cards.LowOnProvisions
import LordOfTheRings.Treachery.Cards.OffTrack
import LordOfTheRings.Treachery.Cards.InNeedOfRest
import LordOfTheRings.Treachery.Cards.RaisingTheCry
import LordOfTheRings.Treachery.Cards.FierceFolk
import LordOfTheRings.Treachery.Cards.DrivenBack
import LordOfTheRings.Treachery.Cards.FrenziedAttack
import LordOfTheRings.Treachery.Cards.DunlendingAmbush
import LordOfTheRings.Treachery.Cards.ConstantTail
import LordOfTheRings.Treachery.Cards.ConspicuousLot
import LordOfTheRings.Treachery.Cards.Cornered
import LordOfTheRings.Treachery.Cards.GetThatDwarf
import LordOfTheRings.Treachery.Cards.Discovered
import LordOfTheRings.Treachery.Cards.DesecratedRuins
import LordOfTheRings.Treachery.Cards.SpiesFromMordor
import LordOfTheRings.Treachery.Cards.ShiftingMarshland
import LordOfTheRings.Treachery.Cards.RemnantsOfElderDays
import LordOfTheRings.Treachery.Cards.CurseOfTheWildMen
import LordOfTheRings.Treachery.Cards.TheGuardiansFury
import LordOfTheRings.Treachery.Cards.WildTenacity
import LordOfTheRings.Treachery.Cards.TheSkyDarkens
import LordOfTheRings.Treachery.Cards.VileAffliction
import LordOfTheRings.Treachery.Cards.DaechanarsWill
import LordOfTheRings.Treachery.Cards.SuddenDarkness
import LordOfTheRings.Treachery.Cards.PredatoryWolves
import LordOfTheRings.Treachery.Cards.UnnaturalFog
import LordOfTheRings.Treachery.Cards.TragicDiscovery
import LordOfTheRings.Treachery.Cards.RestlessEvil
import LordOfTheRings.Treachery.Cards.TerrorOfTheNorth
import LordOfTheRings.Treachery.Cards.DarkSorcery
import LordOfTheRings.Treachery.Cards.HeavyCurse
import LordOfTheRings.Treachery.Cards.SoundTheAlarm
import LordOfTheRings.Treachery.Cards.FeebleAndWeary
import LordOfTheRings.Treachery.Cards.CaptivesOfGornakh
import LordOfTheRings.Treachery.Cards.Interrogation
import LordOfTheRings.Treachery.Cards.SuddenAssault
import LordOfTheRings.Treachery.Cards.SurprisingSpeed
import LordOfTheRings.Treachery.Cards.PressingNeeds
import LordOfTheRings.Treachery.Cards.WeightOfResponsibility
import LordOfTheRings.Treachery.Cards.IceStorm
import LordOfTheRings.Treachery.Cards.BitingWind
import LordOfTheRings.Treachery.Cards.FreezingBlast
import LordOfTheRings.Treachery.Cards.ColdFromAngmar
import LordOfTheRings.Treachery.Cards.AradorsBane
import LordOfTheRings.Treachery.Cards.NoRest
import LordOfTheRings.Treachery.Cards.Possession
import LordOfTheRings.Treachery.Cards.AFellDread
import LordOfTheRings.Treachery.Cards.DeathAndCalamity
import LordOfTheRings.Treachery.Cards.CenturiesOfSorrow
import LordOfTheRings.Treachery.Cards.CurseOfTheYears
import LordOfTheRings.Treachery.Cards.DarkCovenant
import LordOfTheRings.Treachery.Cards.HauntingFog
import LordOfTheRings.Treachery.Cards.ThrownOffCourse
import LordOfTheRings.Treachery.Cards.SuddenStorms
import LordOfTheRings.Treachery.Cards.WindsOfWrath
import LordOfTheRings.Treachery.Cards.BoardingParty
import LordOfTheRings.Treachery.Cards.BattleHardened
import LordOfTheRings.Treachery.Cards.PutToTheTorch
import LordOfTheRings.Treachery.Cards.TheFiresSpread
import LordOfTheRings.Treachery.Cards.MysteriousFog
import LordOfTheRings.Treachery.Cards.LingeringMalevolence
import LordOfTheRings.Treachery.Cards.RuinsOfAgesPast
import LordOfTheRings.Treachery.Cards.CurseOfTheDownfallen
import LordOfTheRings.Treachery.Cards.AimlessWandering
import LordOfTheRings.Treachery.Cards.ManOverboard
import LordOfTheRings.Treachery.Cards.RagingSquall
import LordOfTheRings.Treachery.Cards.HoldSteady
import LordOfTheRings.Treachery.Cards.FromTheDepths
import LordOfTheRings.Treachery.Cards.LashingTentacles
import LordOfTheRings.Treachery.Cards.PowerfulUndertow
import LordOfTheRings.Treachery.Cards.IntoTheAbyss
import LordOfTheRings.Treachery.Cards.TanglingAndGrasping
import LordOfTheRings.Treachery.Cards.AncientDepths
import LordOfTheRings.Treachery.Cards.DeeperIntoTheRuins
import LordOfTheRings.Treachery.Cards.FuriousRainstorm
import LordOfTheRings.Treachery.Cards.DwindlingSupplies
import LordOfTheRings.Treachery.Cards.SunkenRuins
import LordOfTheRings.Treachery.Cards.RammingSpeed
import LordOfTheRings.Treachery.Cards.TakingOnWater
import LordOfTheRings.Treachery.Cards.TheRaidersAssault
import LordOfTheRings.Treachery.Cards.SerpentsTail
import LordOfTheRings.Treachery.Cards.FoesOfEcthelion
import LordOfTheRings.Treachery.Cards.UmbarPatrol
import LordOfTheRings.Treachery.Cards.FuryAndMalice
import LordOfTheRings.Treachery.Cards.TheSpidersWeb
import LordOfTheRings.Treachery.Cards.Backtrack
import LordOfTheRings.Treachery.Cards.PursuitOnTheShore
import LordOfTheRings.Treachery.Cards.TrollAttack
import LordOfTheRings.Treachery.Cards.SmokingBlood
import LordOfTheRings.Treachery.Cards.SentinelOfShadow
import LordOfTheRings.Treachery.Cards.CatacombInspection
import LordOfTheRings.Treachery.Cards.DarkInterrogation
import LordOfTheRings.Treachery.Cards.CunningWolves
import LordOfTheRings.Treachery.Cards.TorrentOfHail
import LordOfTheRings.Treachery.Cards.CruelTorment
import LordOfTheRings.Treachery.Cards.GoblinPatrolNightmare
import LordOfTheRings.Treachery.Cards.CrashingThunder
import LordOfTheRings.Treachery.Cards.BarrenAndDesolate
import LordOfTheRings.Treachery.Cards.MalevolentForces
import LordOfTheRings.Treachery.Cards.LifeDrain
import LordOfTheRings.Treachery.Cards.DeathAndDecay
import LordOfTheRings.Treachery.Cards.HowlingOfWargs
import LordOfTheRings.Treachery.Cards.BlackMare
import LordOfTheRings.Treachery.Runner ()
import LordOfTheRings.Treachery.Types as X
import LordOfTheRings.Id
import LordOfTheRings.Prelude

-- | A stored treachery is rebuilt from its card's builder, then given back its saved state.
instance FromJSON Treachery where
  parseJSON v = do
    attrs <- parseJSON @TreacheryAttrs v
    pure $ overAttrs (const attrs) $ lookupTreachery (treacheryCardCode attrs) (treacheryCardId attrs) (treacheryId attrs)

instance RunMessage Treachery where
  runMessage msg (Treachery a) = Treachery <$> runMessage msg a

createTreachery :: EncounterCard -> TreacheryId -> Treachery
createTreachery card = lookupTreachery (toCardCode card) (ecId card)

lookupTreachery :: (HasCallStack) => CardCode -> CardId -> TreacheryId -> Treachery
lookupTreachery cardCode cardId ident = case Map.lookup cardCode allTreacherys of
  Just (SomeTreacheryCard builder) -> Treachery $ cbCardBuilder builder cardId ident
  Nothing -> error $ "Unknown treachery: " <> show cardCode

newTreachery :: (MonadRandom m) => EncounterCard -> m Treachery
newTreachery card = createTreachery card <$> getRandom

allTreacherys :: Map CardCode SomeTreacheryCard
allTreacherys =
  Map.fromList
    [ (toCardCode builder, card)
    | card@(SomeTreacheryCard builder) <-
        [ SomeTreacheryCard eyesOfTheForest
        , SomeTreacheryCard caveIn
        , SomeTreacheryCard crumblingRuin
        , SomeTreacheryCard darkAndDreadful
        , SomeTreacheryCard suddenPitfall
        , SomeTreacheryCard watchfulEyes
        , SomeTreacheryCard signsOfConflict
        , SomeTreacheryCard manyRoads
        , SomeTreacheryCard burningLow
        , SomeTreacheryCard caughtInAWeb
        , SomeTreacheryCard despair
        , SomeTreacheryCard drivenByShadow
        , SomeTreacheryCard theNecromancersReach
        , SomeTreacheryCard underTheShadow
        , SomeTreacheryCard ironShackles
        , SomeTreacheryCard massingAtNight
        , SomeTreacheryCard evilStorm
        , SomeTreacheryCard pursuedByShadow
        , SomeTreacheryCard treacherousFog
        , SomeTreacheryCard lingeringVenom
        , SomeTreacheryCard falseLead
        , SomeTreacheryCard flooding
        , SomeTreacheryCard oldWivesTales
        , SomeTreacheryCard aFrightenedBeast
        , SomeTreacheryCard sacked
        , SomeTreacheryCard roastedSlowly
        , SomeTreacheryCard exhaustion
        , SomeTreacheryCard swarmingInsect
        , SomeTreacheryCard festeringWounds
        , SomeTreacheryCard impassableChasm
        , SomeTreacheryCard rockslide
        , SomeTreacheryCard slickFooting
        , SomeTreacheryCard aWispOfPaleSheen
        , SomeTreacheryCard nightfall
        , SomeTreacheryCard throughTheMist
        , SomeTreacheryCard theLightsOfTheDead
        , SomeTreacheryCard gollumsAnguish
        , SomeTreacheryCard gollumsBite
        , SomeTreacheryCard wastedProvisions
        , SomeTreacheryCard newDevilry
        , SomeTreacheryCard shadowOfFear
        , SomeTreacheryCard aFoeBeyond
        , SomeTreacheryCard chanceEncounter
        , SomeTreacheryCard massingInTheDeep
        , SomeTreacheryCard undisturbedBones
        , SomeTreacheryCard bitterWind
        , SomeTreacheryCard hiddenThreat
        , SomeTreacheryCard fatigue
        , SomeTreacheryCard foulAir
        , SomeTreacheryCard gatheringGround
        , SomeTreacheryCard vastAndIntricate
        , SomeTreacheryCard sleepingSentry
        , SomeTreacheryCard followedByNight
        , SomeTreacheryCard orcAmbush
        , SomeTreacheryCard fellVoices
        , SomeTreacheryCard fallenStones
        , SomeTreacheryCard snowstorm
        , SomeTreacheryCard freezingCold
        , SomeTreacheryCard avalanche
        , SomeTreacheryCard deepDeepDark
        , SomeTreacheryCard lostAndAlone
        , SomeTreacheryCard firesInTheDeep
        , SomeTreacheryCard innerFlame
        , SomeTreacheryCard innerShadow
        , SomeTreacheryCard whipLash
        , SomeTreacheryCard leapingFlame
        , SomeTreacheryCard counterSpell
        , SomeTreacheryCard fierySword
        , SomeTreacheryCard manyThongedWhip
        , SomeTreacheryCard illPurpose
        , SomeTreacheryCard disturbedWaters
        , SomeTreacheryCard wrapped
        , SomeTreacheryCard orcAssault
        , SomeTreacheryCard theMastersMalice
        , SomeTreacheryCard thePowerOfMordor
        , SomeTreacheryCard scourgeOfMordor
        , SomeTreacheryCard blockingWargs
        , SomeTreacheryCard lostCompany
        , SomeTreacheryCard watcherInTheWoods
        , SomeTreacheryCard southronSupport
        , SomeTreacheryCard collateralDamage
        , SomeTreacheryCard lostInTheCity
        , SomeTreacheryCard localTrouble
        , SomeTreacheryCard lurkingInShadows
        , SomeTreacheryCard burntHomestead
        , SomeTreacheryCard trappedInside
        , SomeTreacheryCard panicked
        , SomeTreacheryCard menInTheDark
        , SomeTreacheryCard starsInSky
        , SomeTreacheryCard leavesOnTree
        , SomeTreacheryCard impenetrableFog
        , SomeTreacheryCard sleeplessMalice
        , SomeTreacheryCard terrorDrivesThem
        , SomeTreacheryCard theDeadCityLooms
        , SomeTreacheryCard unwelcomeDiscovery
        , SomeTreacheryCard aKnifeInTheBack
        , SomeTreacheryCard falseLeadTheStewardsFear
        , SomeTreacheryCard pinnedDown
        , SomeTreacheryCard streetFighting
        , SomeTreacheryCard counterAttack
        , SomeTreacheryCard lyingInWait
        , SomeTreacheryCard mordorLooms
        , SomeTreacheryCard conflictAtTheCrossroads
        , SomeTreacheryCard orcTerritory
        , SomeTreacheryCard theForestsMalice
        , SomeTreacheryCard pillagingAndBurning
        , SomeTreacheryCard downFromTheHills
        , SomeTreacheryCard illTidings
        , SomeTreacheryCard orcHuntingParty
        , SomeTreacheryCard oldHatreds
        , SomeTreacheryCard wildMenOfDunland
        , SomeTreacheryCard turnedAround
        , SomeTreacheryCard takeCover
        , SomeTreacheryCard lowOnProvisions
        , SomeTreacheryCard offTrack
        , SomeTreacheryCard inNeedOfRest
        , SomeTreacheryCard raisingTheCry
        , SomeTreacheryCard fierceFolk
        , SomeTreacheryCard drivenBack
        , SomeTreacheryCard frenziedAttack
        , SomeTreacheryCard dunlendingAmbush
        , SomeTreacheryCard constantTail
        , SomeTreacheryCard conspicuousLot
        , SomeTreacheryCard cornered
        , SomeTreacheryCard getThatDwarf
        , SomeTreacheryCard discovered
        , SomeTreacheryCard desecratedRuins
        , SomeTreacheryCard spiesFromMordor
        , SomeTreacheryCard shiftingMarshland
        , SomeTreacheryCard remnantsOfElderDays
        , SomeTreacheryCard curseOfTheWildMen
        , SomeTreacheryCard theGuardiansFury
        , SomeTreacheryCard wildTenacity
        , SomeTreacheryCard theSkyDarkens
        , SomeTreacheryCard vileAffliction
        , SomeTreacheryCard daechanarsWill
        , SomeTreacheryCard suddenDarkness
        , SomeTreacheryCard predatoryWolves
        , SomeTreacheryCard unnaturalFog
        , SomeTreacheryCard tragicDiscovery
        , SomeTreacheryCard restlessEvil
        , SomeTreacheryCard terrorOfTheNorth
        , SomeTreacheryCard darkSorcery
        , SomeTreacheryCard heavyCurse
        , SomeTreacheryCard soundTheAlarm
        , SomeTreacheryCard feebleAndWeary
        , SomeTreacheryCard captivesOfGornakh
        , SomeTreacheryCard interrogation
        , SomeTreacheryCard suddenAssault
        , SomeTreacheryCard surprisingSpeed
        , SomeTreacheryCard pressingNeeds
        , SomeTreacheryCard weightOfResponsibility
        , SomeTreacheryCard iceStorm
        , SomeTreacheryCard bitingWind
        , SomeTreacheryCard freezingBlast
        , SomeTreacheryCard coldFromAngmar
        , SomeTreacheryCard aradorsBane
        , SomeTreacheryCard noRest
        , SomeTreacheryCard possession
        , SomeTreacheryCard aFellDread
        , SomeTreacheryCard deathAndCalamity
        , SomeTreacheryCard centuriesOfSorrow
        , SomeTreacheryCard curseOfTheYears
        , SomeTreacheryCard darkCovenant
        , SomeTreacheryCard hauntingFog
        , SomeTreacheryCard thrownOffCourse
        , SomeTreacheryCard suddenStorms
        , SomeTreacheryCard windsOfWrath
        , SomeTreacheryCard boardingParty
        , SomeTreacheryCard battleHardened
        , SomeTreacheryCard putToTheTorch
        , SomeTreacheryCard theFiresSpread
        , SomeTreacheryCard mysteriousFog
        , SomeTreacheryCard lingeringMalevolence
        , SomeTreacheryCard ruinsOfAgesPast
        , SomeTreacheryCard curseOfTheDownfallen
        , SomeTreacheryCard aimlessWandering
        , SomeTreacheryCard manOverboard
        , SomeTreacheryCard ragingSquall
        , SomeTreacheryCard holdSteady
        , SomeTreacheryCard fromTheDepths
        , SomeTreacheryCard lashingTentacles
        , SomeTreacheryCard powerfulUndertow
        , SomeTreacheryCard intoTheAbyss
        , SomeTreacheryCard tanglingAndGrasping
        , SomeTreacheryCard ancientDepths
        , SomeTreacheryCard deeperIntoTheRuins
        , SomeTreacheryCard furiousRainstorm
        , SomeTreacheryCard dwindlingSupplies
        , SomeTreacheryCard sunkenRuins
        , SomeTreacheryCard rammingSpeed
        , SomeTreacheryCard takingOnWater
        , SomeTreacheryCard theRaidersAssault
        , SomeTreacheryCard serpentsTail
        , SomeTreacheryCard foesOfEcthelion
        , SomeTreacheryCard umbarPatrol
        , SomeTreacheryCard furyAndMalice
        , SomeTreacheryCard theSpidersWeb
        , SomeTreacheryCard backtrack
        , SomeTreacheryCard pursuitOnTheShore
        , SomeTreacheryCard trollAttack
        , SomeTreacheryCard smokingBlood
        , SomeTreacheryCard sentinelOfShadow
        , SomeTreacheryCard catacombInspection
        , SomeTreacheryCard darkInterrogation
        , SomeTreacheryCard cunningWolves
        , SomeTreacheryCard torrentOfHail
        , SomeTreacheryCard cruelTorment
        , SomeTreacheryCard goblinPatrolNightmare
        , SomeTreacheryCard crashingThunder
        , SomeTreacheryCard barrenAndDesolate
        , SomeTreacheryCard malevolentForces
        , SomeTreacheryCard lifeDrain
        , SomeTreacheryCard deathAndDecay
        , SomeTreacheryCard howlingOfWargs
        , SomeTreacheryCard blackMare
        ]
    ]
