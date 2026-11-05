{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Treachery (module LordOfTheRings.Treachery, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Treachery.Cards.WeHatesIt
import LordOfTheRings.Treachery.Cards.WhereIsssIt
import LordOfTheRings.Treachery.Cards.LightningSplinters
import LordOfTheRings.Treachery.Cards.SwishSmackWhipCrack
import LordOfTheRings.Treachery.Cards.OneByOne
import LordOfTheRings.Treachery.Cards.NightWanesSlow
import LordOfTheRings.Treachery.Cards.MinceThemFine
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
import LordOfTheRings.Treachery.Cards.LosingTime
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
import LordOfTheRings.Treachery.Cards.OminousFate
import LordOfTheRings.Treachery.Cards.SendForHelp
import LordOfTheRings.Treachery.Cards.BedlamInTheStreets
import LordOfTheRings.Treachery.Cards.TheThievesPloy
import LordOfTheRings.Treachery.Cards.TheyreGettingAway
import LordOfTheRings.Treachery.Cards.ObsidianArrows
import LordOfTheRings.Treachery.Cards.SavageFray
import LordOfTheRings.Treachery.Cards.RuinAndWroth
import LordOfTheRings.Treachery.Cards.BrutalAssault
import LordOfTheRings.Treachery.Cards.MugashsCunning
import LordOfTheRings.Treachery.Cards.PatrollingOrcs
import LordOfTheRings.Treachery.Cards.MugashsRage
import LordOfTheRings.Treachery.Cards.StiflingWoods
import LordOfTheRings.Treachery.Cards.FangornsAnger
import LordOfTheRings.Treachery.Cards.RogueWave
import LordOfTheRings.Treachery.Cards.DreamsOfNumenor
import LordOfTheRings.Treachery.Cards.RagingSquallNightmare
import LordOfTheRings.Treachery.Cards.CorruptedFlora
import LordOfTheRings.Treachery.Cards.CalphonsFolly
import LordOfTheRings.Treachery.Cards.SuddenAssaultNightmare
import LordOfTheRings.Treachery.Cards.Infiltration
import LordOfTheRings.Treachery.Cards.AGreatHost
import LordOfTheRings.Treachery.Cards.KillThem
import LordOfTheRings.Treachery.Cards.RainWashedTracks
import LordOfTheRings.Treachery.Cards.ParalyzingFear
import LordOfTheRings.Treachery.Cards.CurseOfAngmar
import LordOfTheRings.Treachery.Cards.ShadowFall
import LordOfTheRings.Treachery.Cards.BalinsDoom
import LordOfTheRings.Treachery.Cards.BeatsAndEchoes
import LordOfTheRings.Treachery.Cards.DismayAndFear
import LordOfTheRings.Treachery.Cards.AnEvilFortune
import LordOfTheRings.Treachery.Cards.BoarClanAssault
import LordOfTheRings.Treachery.Cards.StalkedByDunlendings
import LordOfTheRings.Treachery.Cards.CursedBurden
import LordOfTheRings.Treachery.Cards.TrialOfAncestors
import LordOfTheRings.Treachery.Cards.WaylaidByAgents
import LordOfTheRings.Treachery.Cards.TharbadPatrol
import LordOfTheRings.Treachery.Cards.Quicksand
import LordOfTheRings.Treachery.Cards.RottenStench
import LordOfTheRings.Treachery.Cards.HatefulSearch
import LordOfTheRings.Treachery.Cards.SauronsAgent
import LordOfTheRings.Treachery.Cards.ClosingIn
import LordOfTheRings.Treachery.Cards.RavenTalisman
import LordOfTheRings.Treachery.Cards.GatheringForces
import LordOfTheRings.Treachery.Cards.DarkAndStormy
import LordOfTheRings.Treachery.Cards.SahirsGettingAway
import LordOfTheRings.Treachery.Cards.TumultuousWaters
import LordOfTheRings.Treachery.Cards.DeepMaelstrom
import LordOfTheRings.Treachery.Cards.DeceitfulIsland
import LordOfTheRings.Treachery.Cards.CallOfTheCurse
import LordOfTheRings.Treachery.Cards.SuddenPrecipice
import LordOfTheRings.Treachery.Cards.PitIntoTheDepths
import LordOfTheRings.Treachery.Cards.SulfurCloud
import LordOfTheRings.Treachery.Cards.BrokenRudder
import LordOfTheRings.Treachery.Cards.Grappled
import LordOfTheRings.Treachery.Cards.InspiringFury
import LordOfTheRings.Treachery.Cards.RaiseTheAlarm
import LordOfTheRings.Treachery.Cards.SlaveTrade
import LordOfTheRings.Treachery.Cards.TheEnemysTrail
import LordOfTheRings.Treachery.Cards.RestlessHunters
import LordOfTheRings.Treachery.Cards.TheTrailGoesCold
import LordOfTheRings.Treachery.Cards.TimeForSupper
import LordOfTheRings.Treachery.Cards.ShotFromTheSky
import LordOfTheRings.Treachery.Cards.NoxiousSupplies
import LordOfTheRings.Treachery.Cards.AHurryingDarkness
import LordOfTheRings.Treachery.Cards.BlastOfSavageWind
import LordOfTheRings.Treachery.Cards.HazardousTerrain
import LordOfTheRings.Treachery.Cards.RetracedSteps
import LordOfTheRings.Treachery.Cards.CandlesOfCorpses
import LordOfTheRings.Treachery.Cards.SwarmingMosquitoes
import LordOfTheRings.Treachery.Cards.DevilryOfTheDarkLand
import LordOfTheRings.Treachery.Cards.Isolation
import LordOfTheRings.Treachery.Cards.BefouledEquipment
import LordOfTheRings.Treachery.Cards.WickednessAndMischief
import LordOfTheRings.Treachery.Cards.TheTowerBeckons
import LordOfTheRings.Treachery.Cards.NoxiousFumes
import LordOfTheRings.Treachery.Cards.SurroundedByShadow
import LordOfTheRings.Treachery.Cards.SuddenOnslaught
import LordOfTheRings.Treachery.Cards.DrumsOfTheWildMen
import LordOfTheRings.Treachery.Cards.PoisonDarts
import LordOfTheRings.Treachery.Cards.TrappedVillagers
import LordOfTheRings.Treachery.Cards.RunForYourLives
import LordOfTheRings.Treachery.Cards.SaveUs
import LordOfTheRings.Treachery.Cards.ThoseNowSundered
import LordOfTheRings.Treachery.Cards.RelentlessAttack
import LordOfTheRings.Treachery.Cards.OverrunDefenses
import LordOfTheRings.Treachery.Cards.PressedOnAllSides
import LordOfTheRings.Treachery.Cards.FalseAccusations
import LordOfTheRings.Treachery.Cards.BlindingBlizzard
import LordOfTheRings.Treachery.Cards.CaradhrasTheCruel
import LordOfTheRings.Treachery.Cards.TumultuousAssault
import LordOfTheRings.Treachery.Cards.PulledUnder
import LordOfTheRings.Treachery.Cards.RipplingFromBeneath
import LordOfTheRings.Treachery.Cards.DreadfulDeep
import LordOfTheRings.Treachery.Cards.LostAndHelpless
import LordOfTheRings.Treachery.Cards.ChillOfTheRoots
import LordOfTheRings.Treachery.Cards.TerribleStrength
import LordOfTheRings.Treachery.Cards.TheDarkFire
import LordOfTheRings.Treachery.Cards.BlazingGrip
import LordOfTheRings.Treachery.Cards.MorgothsBidding
import LordOfTheRings.Treachery.Cards.FlameOfUdun
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
import LordOfTheRings.Treachery.Cards.PowerBeyondDeath
import LordOfTheRings.Treachery.Cards.DaechanarsCurse
import LordOfTheRings.Treachery.Cards.AllIsLost
import LordOfTheRings.Treachery.Cards.NowhereToHide
import LordOfTheRings.Treachery.Cards.InjuredCompanion
import LordOfTheRings.Treachery.Cards.Enfeebled
import LordOfTheRings.Treachery.Cards.Sandstorm
import LordOfTheRings.Treachery.Cards.HeatExhaustion
import LordOfTheRings.Treachery.Cards.Overgrown
import LordOfTheRings.Treachery.Cards.StampedingOliphaunt
import LordOfTheRings.Treachery.Cards.TheSavageSouth
import LordOfTheRings.Treachery.Cards.TerribleFever
import LordOfTheRings.Treachery.Cards.BitingInsects
import LordOfTheRings.Treachery.Cards.FearOfMordor
import LordOfTheRings.Treachery.Cards.SavageAttack
import LordOfTheRings.Treachery.Cards.OverrunByOrcs
import LordOfTheRings.Treachery.Cards.ForcedOffTrackBeneathTheSands
import LordOfTheRings.Treachery.Cards.GrabbedBySpiders
import LordOfTheRings.Treachery.Cards.Mirage
import LordOfTheRings.Treachery.Cards.UnbearableHeat
import LordOfTheRings.Treachery.Cards.ScorchingSun
import LordOfTheRings.Treachery.Cards.EnemyPursuit
import LordOfTheRings.Treachery.Cards.PursuersFromMordor
import LordOfTheRings.Treachery.Cards.HowlingAtTheirHeels
import LordOfTheRings.Treachery.Cards.BoldAndGrim
import LordOfTheRings.Treachery.Cards.NoQuarter
import LordOfTheRings.Treachery.Cards.ChaosInTheNight
import LordOfTheRings.Treachery.Cards.TakenPrisoner
import LordOfTheRings.Treachery.Cards.TormentedByOrcs
import LordOfTheRings.Treachery.Cards.DangerCloseBehind
import LordOfTheRings.Treachery.Cards.MountainsOfShadow
import LordOfTheRings.Treachery.Cards.RuinedSupplies
import LordOfTheRings.Treachery.Cards.LostInTheWild
import LordOfTheRings.Treachery.Cards.WeighedDown
import LordOfTheRings.Treachery.Cards.GatheringGloom
import LordOfTheRings.Treachery.Cards.SwarmOfBats
import LordOfTheRings.Treachery.Cards.AfraidOfTheDark
import LordOfTheRings.Treachery.Cards.DarkPlaces
import LordOfTheRings.Treachery.Cards.EyesInTheDark
import LordOfTheRings.Treachery.Cards.WilderlandsBane
import LordOfTheRings.Treachery.Cards.SuffocatingShadows
import LordOfTheRings.Treachery.Cards.DragonBreath
import LordOfTheRings.Treachery.Cards.DragonScales
import LordOfTheRings.Treachery.Cards.FierceAttack
import LordOfTheRings.Treachery.Cards.UrdugsCommand
import LordOfTheRings.Treachery.Cards.DarkClouds
import LordOfTheRings.Treachery.Cards.FrenziedCreature
import LordOfTheRings.Treachery.Cards.DangerousCrossing
import LordOfTheRings.Treachery.Cards.PowerfulInWrath
import LordOfTheRings.Treachery.Cards.TheDragonsFury
import LordOfTheRings.Treachery.Cards.BrightFlames
import LordOfTheRings.Treachery.Cards.DenizenOfTheDeep
import LordOfTheRings.Treachery.Cards.AbandonedRuins
import LordOfTheRings.Treachery.Cards.TerribleShadows
import LordOfTheRings.Treachery.Cards.SlammedShut
import LordOfTheRings.Treachery.Cards.HeavySnow
import LordOfTheRings.Treachery.Cards.DeadlyCold
import LordOfTheRings.Treachery.Cards.SneakingOff
import LordOfTheRings.Treachery.Cards.HarshWeather
import LordOfTheRings.Treachery.Cards.WildCreatures
import LordOfTheRings.Treachery.Cards.UnseenDanger
import LordOfTheRings.Treachery.Cards.VastnessOfMirkwood
import LordOfTheRings.Treachery.Cards.Stinker
import LordOfTheRings.Treachery.Cards.EasterlingHorse
import LordOfTheRings.Treachery.Cards.SwordOfRhun
import LordOfTheRings.Treachery.Cards.RecurveBow
import LordOfTheRings.Treachery.Cards.WearyLands
import LordOfTheRings.Treachery.Cards.PainfulFatigue
import LordOfTheRings.Treachery.Cards.Overwhelmed
import LordOfTheRings.Treachery.Cards.DarknessInTheEast
import LordOfTheRings.Treachery.Cards.UnderGuard
import LordOfTheRings.Treachery.Cards.UlchorsRage
import LordOfTheRings.Treachery.Cards.Fanaticism
import LordOfTheRings.Treachery.Cards.FearfulShadows
import LordOfTheRings.Treachery.Cards.EnemiesCloseBehind
import LordOfTheRings.Treachery.Cards.PlaceOfEvil
import LordOfTheRings.Treachery.Cards.ExtraVigil
import LordOfTheRings.Treachery.Cards.WeveBeenSeen
import LordOfTheRings.Treachery.Cards.PatrollingTheStreets
import LordOfTheRings.Treachery.Cards.RansackedSupplies
import LordOfTheRings.Treachery.Cards.BurningReek
import LordOfTheRings.Treachery.Cards.WrithingShadows
import LordOfTheRings.Treachery.Cards.Disappearances
import LordOfTheRings.Treachery.Cards.SecretCultist
import LordOfTheRings.Treachery.Cards.Fury
import LordOfTheRings.Treachery.Cards.CaughtOffGuard
import LordOfTheRings.Treachery.Cards.StreetFightingWrathAndRuin
import LordOfTheRings.Treachery.Cards.UnderSiege
import LordOfTheRings.Treachery.Cards.UlchorsRevenge
import LordOfTheRings.Treachery.Cards.WainriderChariot
import LordOfTheRings.Treachery.Cards.TheChampionsCunning
import LordOfTheRings.Treachery.Cards.DeterminedDefense
import LordOfTheRings.Treachery.Cards.TheDarkLordsReach
import LordOfTheRings.Treachery.Cards.OverseersWhip
import LordOfTheRings.Treachery.Cards.CrueltyOfMordor
import LordOfTheRings.Treachery.Cards.BrokenCaptives
import LordOfTheRings.Treachery.Cards.BloodRunsChill (bloodRunsChill)
import LordOfTheRings.Treachery.Cards.TheWayIsShut (theWayIsShut)
import LordOfTheRings.Treachery.Cards.WhispersInTheDark (whispersInTheDark)
import LordOfTheRings.Treachery.Cards.NoTurningBack (noTurningBack)
import LordOfTheRings.Treachery.Cards.RegimentsOfCrows
import LordOfTheRings.Treachery.Cards.Snowdrifts
import LordOfTheRings.Treachery.Cards.BitterCold
import LordOfTheRings.Treachery.Cards.StormOfHowls
import LordOfTheRings.Treachery.Cards.LustForTheRing
import LordOfTheRings.Treachery.Cards.TheSearchingEye
import LordOfTheRings.Treachery.Cards.CreepingMarshes
import LordOfTheRings.Treachery.Cards.BitterReek
import LordOfTheRings.Treachery.Cards.AFellLight
import LordOfTheRings.Treachery.Cards.CallOfTheRing
import LordOfTheRings.Treachery.Cards.WeHatesThem
import LordOfTheRings.Treachery.Cards.TheDarkLordsSummons
import LordOfTheRings.Treachery.Cards.MarchingUpTheRoad
import LordOfTheRings.Treachery.Cards.FierceSouthrons
import LordOfTheRings.Treachery.Cards.ArrowsThickInTheAir
import LordOfTheRings.Treachery.Cards.DrawnToTheRing
import LordOfTheRings.Treachery.Cards.DeepShadows
import LordOfTheRings.Treachery.Cards.EvilVigilance
import LordOfTheRings.Treachery.Cards.OrcishHowls
import LordOfTheRings.Treachery.Cards.ExhalationOfDecay
import LordOfTheRings.Treachery.Cards.ManyOpenings
import LordOfTheRings.Treachery.Cards.ShelobsPoison
import LordOfTheRings.Treachery.Cards.LurkingMalice
import LordOfTheRings.Treachery.Cards.RageAndHunger
import LordOfTheRings.Treachery.Cards.CaughtInWebs
import LordOfTheRings.Treachery.Cards.PoisonedBySpiders
import LordOfTheRings.Treachery.Cards.FightingAmongFriends
import LordOfTheRings.Treachery.Cards.WeighedDownWilderland
import LordOfTheRings.Treachery.Cards.HatredRekindledTheBattleOfFiveArmies
import LordOfTheRings.Treachery.Cards.GoblinsAreUponYou
import LordOfTheRings.Treachery.Cards.HeedlessOfOrder
import LordOfTheRings.Treachery.Cards.LotsOrNoneAtAll
import LordOfTheRings.Treachery.Cards.ANicePickle
import LordOfTheRings.Treachery.Cards.RoastEmOrBoilEm
import LordOfTheRings.Treachery.Cards.HungryTroll
import LordOfTheRings.Treachery.Cards.NoCampfire
import LordOfTheRings.Treachery.Cards.WindWhippedRain
import LordOfTheRings.Treachery.Cards.MoreLikeAGrocer
import LordOfTheRings.Treachery.Cards.GripGrabPinchNab
import LordOfTheRings.Treachery.Cards.GallopingBoulders
import LordOfTheRings.Treachery.Cards.GuffawingOfGiants
import LordOfTheRings.Treachery.Cards.ChaosInTheCavern
import LordOfTheRings.Treachery.Cards.ComeDownLittleBird
import LordOfTheRings.Treachery.Cards.ItLikesRiddles
import LordOfTheRings.Treachery.Cards.HidingInTheTrees
import LordOfTheRings.Treachery.Cards.WhatsInMyPocket
import LordOfTheRings.Treachery.Cards.NotFairNotFair
import LordOfTheRings.Treachery.Cards.PretendingToSleep (pretendingToSleep)
import LordOfTheRings.Treachery.Cards.KnownToAnOunce (knownToAnOunce)
import LordOfTheRings.Treachery.Cards.BelchingFire (belchingFire)
import LordOfTheRings.Treachery.Cards.DragonSpell (dragonSpell)
import LordOfTheRings.Treachery.Cards.CrawlingTowardsHim
import LordOfTheRings.Treachery.Cards.HuntingForTheRing
import LordOfTheRings.Treachery.Cards.HaveYouSeenBaggins
import LordOfTheRings.Treachery.Cards.PaleBlade
import LordOfTheRings.Treachery.Cards.PowerInTheirTerror
import LordOfTheRings.Treachery.Cards.PiercingCry
import LordOfTheRings.Treachery.Cards.BlackSteed
import LordOfTheRings.Treachery.Cards.RodeLikeAGale
import LordOfTheRings.Treachery.Cards.LureOfTheRing
import LordOfTheRings.Treachery.Cards.TheRingDrawsThem
import LordOfTheRings.Treachery.Cards.StrickenDumb
import LordOfTheRings.Treachery.Cards.TheEnemyIsUponUs
import LordOfTheRings.Treachery.Cards.PainAssailedHim
import LordOfTheRings.Treachery.Cards.TheNineAreAbroad
import LordOfTheRings.Treachery.Cards.WeightOfTheRing
import LordOfTheRings.Treachery.Cards.FearOfDiscovery
import LordOfTheRings.Treachery.Cards.EatenAlive
import LordOfTheRings.Treachery.Cards.PanickedFlightToTheFord
import LordOfTheRings.Treachery.Cards.OvercomeByTerror
import LordOfTheRings.Treachery.Cards.UnwantedAttention
import LordOfTheRings.Treachery.Cards.BlackBreath
import LordOfTheRings.Treachery.Cards.TheRammasIsBreached (theRammasIsBreached)
import LordOfTheRings.Treachery.Cards.FellBeast (fellBeast)
import LordOfTheRings.Treachery.Cards.BlackDart (blackDart)
import LordOfTheRings.Treachery.Cards.SpellsOfRuin (spellsOfRuin)
import LordOfTheRings.Treachery.Cards.DreadAndDespair (dreadAndDespair)
import LordOfTheRings.Treachery.Cards.ShadowOfMordor (shadowOfMordor)
import LordOfTheRings.Treachery.Cards.FellFolkOfUmbar (fellFolkOfUmbar)
import LordOfTheRings.Treachery.Cards.BlackSails (blackSails)
import LordOfTheRings.Treachery.Cards.RaidingTheCoasts (raidingTheCoasts)
import LordOfTheRings.Treachery.Cards.DrivenToTheBrink (drivenToTheBrink)
import LordOfTheRings.Treachery.Cards.HellHawk
import LordOfTheRings.Treachery.Cards.TheHourOfDoom
import LordOfTheRings.Treachery.Cards.JawsOfSteel
import LordOfTheRings.Treachery.Cards.RageFilledHim
import LordOfTheRings.Treachery.Cards.OnslaughtOfMordor
import LordOfTheRings.Treachery.Cards.WickedMaster
import LordOfTheRings.Treachery.Cards.TheLidlessEye
import LordOfTheRings.Treachery.Cards.FoulFumes
import LordOfTheRings.Treachery.Cards.HeavyAndTired
import LordOfTheRings.Treachery.Cards.TheyAreComing
import LordOfTheRings.Treachery.Cards.WeCannotGetOut
import LordOfTheRings.Treachery.Cards.DeepFissure
import LordOfTheRings.Treachery.Cards.FoolOfATook
import LordOfTheRings.Treachery.Cards.ShadowOfFearJourneyInTheDark
import LordOfTheRings.Treachery.Cards.PursuedByTheEnemy
import LordOfTheRings.Treachery.Cards.GrowingThreat
import LordOfTheRings.Treachery.Cards.OrcsOfTheWhiteHand
import LordOfTheRings.Treachery.Cards.BlackFeatheredArrows
import LordOfTheRings.Treachery.Cards.FallenIntoEvil
import LordOfTheRings.Treachery.Cards.FollowedByNightTheRoadDarkens
import LordOfTheRings.Treachery.Cards.IllFate
import LordOfTheRings.Treachery.Cards.BlastOfSorcery
import LordOfTheRings.Treachery.Cards.PoisonedVapour
import LordOfTheRings.Treachery.Cards.MachinesOfIsengard
import LordOfTheRings.Treachery.Cards.SarumansVoice
import LordOfTheRings.Treachery.Cards.RestByNight
import LordOfTheRings.Treachery.Cards.StrangeWeariness
import LordOfTheRings.Treachery.Cards.UnseenBarrier
import LordOfTheRings.Treachery.Cards.FightingUrukHai
import LordOfTheRings.Treachery.Cards.SarumansOrders
import LordOfTheRings.Treachery.Cards.ScalingLadders
import LordOfTheRings.Treachery.Cards.DevilryOfSaruman
import LordOfTheRings.Treachery.Cards.RecklessHate
import LordOfTheRings.Treachery.Cards.NightWithoutEnd
import LordOfTheRings.Treachery.Cards.PoisonedCounselsTheTreasonOfSaruman
import LordOfTheRings.Treachery.Cards.AGrowingThreat
import LordOfTheRings.Treachery.Cards.UrdugsCunning
import LordOfTheRings.Treachery.Cards.MisplacedTrust
import LordOfTheRings.Treachery.Cards.DragonSickness
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
        , SomeTreacheryCard weHatesIt
        , SomeTreacheryCard whereIsssIt
        , SomeTreacheryCard lightningSplinters
        , SomeTreacheryCard swishSmackWhipCrack
        , SomeTreacheryCard oneByOne
        , SomeTreacheryCard nightWanesSlow
        , SomeTreacheryCard minceThemFine
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
        , SomeTreacheryCard losingTime
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
        , SomeTreacheryCard ominousFate
        , SomeTreacheryCard sendForHelpLeadership
        , SomeTreacheryCard sendForHelpTactics
        , SomeTreacheryCard sendForHelpSpirit
        , SomeTreacheryCard sendForHelpLore
        , SomeTreacheryCard bedlamInTheStreets
        , SomeTreacheryCard theThievesPloy
        , SomeTreacheryCard theyreGettingAway
        , SomeTreacheryCard obsidianArrows
        , SomeTreacheryCard savageFray
        , SomeTreacheryCard ruinAndWroth
        , SomeTreacheryCard brutalAssault
        , SomeTreacheryCard mugashsCunning
        , SomeTreacheryCard patrollingOrcs
        , SomeTreacheryCard mugashsRage
        , SomeTreacheryCard stiflingWoods
        , SomeTreacheryCard fangornsAnger
        , SomeTreacheryCard rogueWave
        , SomeTreacheryCard dreamsOfNumenor
        , SomeTreacheryCard ragingSquallNightmare
        , SomeTreacheryCard corruptedFlora
        , SomeTreacheryCard calphonsFolly
        , SomeTreacheryCard suddenAssaultNightmare
        , SomeTreacheryCard infiltration
        , SomeTreacheryCard aGreatHost
        , SomeTreacheryCard killThem
        , SomeTreacheryCard rainWashedTracks
        , SomeTreacheryCard paralyzingFear
        , SomeTreacheryCard curseOfAngmar
        , SomeTreacheryCard shadowFall
        , SomeTreacheryCard balinsDoom
        , SomeTreacheryCard beatsAndEchoes
        , SomeTreacheryCard dismayAndFear
        , SomeTreacheryCard anEvilFortune
        , SomeTreacheryCard boarClanAssault
        , SomeTreacheryCard stalkedByDunlendings
        , SomeTreacheryCard cursedBurden
        , SomeTreacheryCard trialOfAncestors
        , SomeTreacheryCard waylaidByAgents
        , SomeTreacheryCard tharbadPatrol
        , SomeTreacheryCard quicksand
        , SomeTreacheryCard rottenStench
        , SomeTreacheryCard hatefulSearch
        , SomeTreacheryCard sauronsAgent
        , SomeTreacheryCard closingIn
        , SomeTreacheryCard ravenTalisman
        , SomeTreacheryCard gatheringForces
        , SomeTreacheryCard darkAndStormy
        , SomeTreacheryCard sahirsGettingAway
        , SomeTreacheryCard tumultuousWaters
        , SomeTreacheryCard deepMaelstrom
        , SomeTreacheryCard deceitfulIsland
        , SomeTreacheryCard callOfTheCurse
        , SomeTreacheryCard suddenPrecipice
        , SomeTreacheryCard pitIntoTheDepths
        , SomeTreacheryCard sulfurCloud
        , SomeTreacheryCard brokenRudder
        , SomeTreacheryCard grappled
        , SomeTreacheryCard inspiringFury
        , SomeTreacheryCard raiseTheAlarm
        , SomeTreacheryCard slaveTrade
        , SomeTreacheryCard theEnemysTrail
        , SomeTreacheryCard restlessHunters
        , SomeTreacheryCard theTrailGoesCold
        , SomeTreacheryCard timeForSupper
        , SomeTreacheryCard shotFromTheSky
        , SomeTreacheryCard noxiousSupplies
        , SomeTreacheryCard aHurryingDarkness
        , SomeTreacheryCard blastOfSavageWind
        , SomeTreacheryCard hazardousTerrain
        , SomeTreacheryCard retracedSteps
        , SomeTreacheryCard candlesOfCorpses
        , SomeTreacheryCard swarmingMosquitoes
        , SomeTreacheryCard devilryOfTheDarkLand
        , SomeTreacheryCard isolation
        , SomeTreacheryCard befouledEquipment
        , SomeTreacheryCard wickednessAndMischief
        , SomeTreacheryCard theTowerBeckons
        , SomeTreacheryCard noxiousFumes
        , SomeTreacheryCard surroundedByShadow
        , SomeTreacheryCard suddenOnslaught
        , SomeTreacheryCard drumsOfTheWildMen
        , SomeTreacheryCard poisonDarts
        , SomeTreacheryCard trappedVillagers
        , SomeTreacheryCard runForYourLives
        , SomeTreacheryCard saveUs
        , SomeTreacheryCard thoseNowSundered
        , SomeTreacheryCard relentlessAttack
        , SomeTreacheryCard overrunDefenses
        , SomeTreacheryCard pressedOnAllSides
        , SomeTreacheryCard falseAccusations
        , SomeTreacheryCard blindingBlizzard
        , SomeTreacheryCard caradhrasTheCruel
        , SomeTreacheryCard tumultuousAssault
        , SomeTreacheryCard pulledUnder
        , SomeTreacheryCard ripplingFromBeneath
        , SomeTreacheryCard dreadfulDeep
        , SomeTreacheryCard lostAndHelpless
        , SomeTreacheryCard chillOfTheRoots
        , SomeTreacheryCard terribleStrength
        , SomeTreacheryCard theDarkFire
        , SomeTreacheryCard blazingGrip
        , SomeTreacheryCard morgothsBidding
        , SomeTreacheryCard flameOfUdun
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
        , SomeTreacheryCard powerBeyondDeath
        , SomeTreacheryCard daechanarsCurse
        , SomeTreacheryCard allIsLost
        , SomeTreacheryCard nowhereToHide
        , SomeTreacheryCard injuredCompanion
        , SomeTreacheryCard enfeebled
        , SomeTreacheryCard sandstorm
        , SomeTreacheryCard heatExhaustion
        , SomeTreacheryCard overgrown
        , SomeTreacheryCard stampedingOliphaunt
        , SomeTreacheryCard theSavageSouth
        , SomeTreacheryCard terribleFever
        , SomeTreacheryCard bitingInsects
        , SomeTreacheryCard fearOfMordor
        , SomeTreacheryCard savageAttack
        , SomeTreacheryCard overrunByOrcs
        , SomeTreacheryCard forcedOffTrackBeneathTheSands
        , SomeTreacheryCard grabbedBySpiders
        , SomeTreacheryCard mirage
        , SomeTreacheryCard unbearableHeat
        , SomeTreacheryCard scorchingSun
        , SomeTreacheryCard enemyPursuit
        , SomeTreacheryCard pursuersFromMordor
        , SomeTreacheryCard howlingAtTheirHeels
        , SomeTreacheryCard boldAndGrim
        , SomeTreacheryCard noQuarter
        , SomeTreacheryCard chaosInTheNight
        , SomeTreacheryCard takenPrisoner
        , SomeTreacheryCard tormentedByOrcs
        , SomeTreacheryCard dangerCloseBehind
        , SomeTreacheryCard mountainsOfShadow
        , SomeTreacheryCard ruinedSupplies
        , SomeTreacheryCard lostInTheWild
        , SomeTreacheryCard weighedDown
        , SomeTreacheryCard gatheringGloom
        , SomeTreacheryCard swarmOfBats
        , SomeTreacheryCard afraidOfTheDark
        , SomeTreacheryCard darkPlaces
        , SomeTreacheryCard eyesInTheDark
        , SomeTreacheryCard wilderlandsBane
        , SomeTreacheryCard suffocatingShadows
        , SomeTreacheryCard dragonBreath
        , SomeTreacheryCard dragonScales
        , SomeTreacheryCard fierceAttack
        , SomeTreacheryCard urdugsCommand
        , SomeTreacheryCard darkClouds
        , SomeTreacheryCard frenziedCreature
        , SomeTreacheryCard dangerousCrossing
        , SomeTreacheryCard powerfulInWrath
        , SomeTreacheryCard theDragonsFury
        , SomeTreacheryCard brightFlames
        , SomeTreacheryCard denizenOfTheDeep
        , SomeTreacheryCard abandonedRuins
        , SomeTreacheryCard terribleShadows
        , SomeTreacheryCard slammedShut
        , SomeTreacheryCard heavySnow
        , SomeTreacheryCard deadlyCold
        , SomeTreacheryCard sneakingOff
        , SomeTreacheryCard harshWeather
        , SomeTreacheryCard wildCreatures
        , SomeTreacheryCard unseenDanger
        , SomeTreacheryCard vastnessOfMirkwood
        , SomeTreacheryCard stinker
        , SomeTreacheryCard easterlingHorse
        , SomeTreacheryCard swordOfRhun
        , SomeTreacheryCard recurveBow
        , SomeTreacheryCard wearyLands
        , SomeTreacheryCard painfulFatigue
        , SomeTreacheryCard overwhelmed
        , SomeTreacheryCard darknessInTheEast
        , SomeTreacheryCard underGuard
        , SomeTreacheryCard ulchorsRage
        , SomeTreacheryCard fanaticism
        , SomeTreacheryCard fearfulShadows
        , SomeTreacheryCard enemiesCloseBehind
        , SomeTreacheryCard placeOfEvil
        , SomeTreacheryCard extraVigil
        , SomeTreacheryCard weveBeenSeen
        , SomeTreacheryCard patrollingTheStreets
        , SomeTreacheryCard ransackedSupplies
        , SomeTreacheryCard burningReek
        , SomeTreacheryCard writhingShadows
        , SomeTreacheryCard disappearances
        , SomeTreacheryCard secretCultist
        , SomeTreacheryCard fury
        , SomeTreacheryCard caughtOffGuard
        , SomeTreacheryCard streetFightingWrathAndRuin
        , SomeTreacheryCard underSiege
        , SomeTreacheryCard ulchorsRevenge
        , SomeTreacheryCard wainriderChariot
        , SomeTreacheryCard theChampionsCunning
        , SomeTreacheryCard determinedDefense
        , SomeTreacheryCard theDarkLordsReach
        , SomeTreacheryCard overseersWhip
        , SomeTreacheryCard crueltyOfMordor
        , SomeTreacheryCard brokenCaptives
        , SomeTreacheryCard bloodRunsChill
        , SomeTreacheryCard theWayIsShut
        , SomeTreacheryCard whispersInTheDark
        , SomeTreacheryCard noTurningBack
        , SomeTreacheryCard regimentsOfCrows
        , SomeTreacheryCard snowdrifts
        , SomeTreacheryCard bitterCold
        , SomeTreacheryCard stormOfHowls
        , SomeTreacheryCard lustForTheRing
        , SomeTreacheryCard theSearchingEye
        , SomeTreacheryCard creepingMarshes
        , SomeTreacheryCard bitterReek
        , SomeTreacheryCard aFellLight
        , SomeTreacheryCard callOfTheRing
        , SomeTreacheryCard weHatesThem
        , SomeTreacheryCard theDarkLordsSummons
        , SomeTreacheryCard marchingUpTheRoad
        , SomeTreacheryCard fierceSouthrons
        , SomeTreacheryCard arrowsThickInTheAir
        , SomeTreacheryCard orcishHowls
        , SomeTreacheryCard drawnToTheRing
        , SomeTreacheryCard deepShadows
        , SomeTreacheryCard evilVigilance
        , SomeTreacheryCard exhalationOfDecay
        , SomeTreacheryCard manyOpenings
        , SomeTreacheryCard shelobsPoison
        , SomeTreacheryCard lurkingMalice
        , SomeTreacheryCard rageAndHunger
        , SomeTreacheryCard caughtInWebs
        , SomeTreacheryCard poisonedBySpiders
        , SomeTreacheryCard fightingAmongFriends
        , SomeTreacheryCard weighedDownWilderland
        , SomeTreacheryCard hatredRekindledTheBattleOfFiveArmies
        , SomeTreacheryCard goblinsAreUponYou
        , SomeTreacheryCard heedlessOfOrder
        , SomeTreacheryCard lotsOrNoneAtAll
        , SomeTreacheryCard aNicePickle
        , SomeTreacheryCard roastEmOrBoilEm
        , SomeTreacheryCard hungryTroll
        , SomeTreacheryCard noCampfire
        , SomeTreacheryCard windWhippedRain
        , SomeTreacheryCard moreLikeAGrocer
        , SomeTreacheryCard gripGrabPinchNab
        , SomeTreacheryCard gallopingBoulders
        , SomeTreacheryCard guffawingOfGiants
        , SomeTreacheryCard chaosInTheCavern
        , SomeTreacheryCard comeDownLittleBird
        , SomeTreacheryCard itLikesRiddles
        , SomeTreacheryCard hidingInTheTrees
        , SomeTreacheryCard whatsInMyPocket
        , SomeTreacheryCard notFairNotFair
        , SomeTreacheryCard pretendingToSleep
        , SomeTreacheryCard knownToAnOunce
        , SomeTreacheryCard belchingFire
        , SomeTreacheryCard dragonSpell
        , SomeTreacheryCard crawlingTowardsHim
        , SomeTreacheryCard huntingForTheRing
        , SomeTreacheryCard haveYouSeenBaggins
        , SomeTreacheryCard paleBlade
        , SomeTreacheryCard powerInTheirTerror
        , SomeTreacheryCard piercingCry
        , SomeTreacheryCard blackSteed
        , SomeTreacheryCard rodeLikeAGale
        , SomeTreacheryCard lureOfTheRing
        , SomeTreacheryCard theRingDrawsThem
        , SomeTreacheryCard strickenDumb
        , SomeTreacheryCard theEnemyIsUponUs
        , SomeTreacheryCard painAssailedHim
        , SomeTreacheryCard theNineAreAbroad
        , SomeTreacheryCard weightOfTheRing
        , SomeTreacheryCard fearOfDiscovery
        , SomeTreacheryCard eatenAlive
        , SomeTreacheryCard panickedFlightToTheFord
        , SomeTreacheryCard overcomeByTerror
        , SomeTreacheryCard unwantedAttention
        , SomeTreacheryCard blackBreath
        , SomeTreacheryCard theRammasIsBreached
        , SomeTreacheryCard fellBeast
        , SomeTreacheryCard blackDart
        , SomeTreacheryCard spellsOfRuin
        , SomeTreacheryCard dreadAndDespair
        , SomeTreacheryCard shadowOfMordor
        , SomeTreacheryCard fellFolkOfUmbar
        , SomeTreacheryCard blackSails
        , SomeTreacheryCard raidingTheCoasts
        , SomeTreacheryCard drivenToTheBrink
        , SomeTreacheryCard hellHawk
        , SomeTreacheryCard theHourOfDoom
        , SomeTreacheryCard jawsOfSteel
        , SomeTreacheryCard rageFilledHim
        , SomeTreacheryCard onslaughtOfMordor
        , SomeTreacheryCard wickedMaster
        , SomeTreacheryCard theLidlessEye
        , SomeTreacheryCard foulFumes
        , SomeTreacheryCard heavyAndTired
        , SomeTreacheryCard theyAreComing
        , SomeTreacheryCard weCannotGetOut
        , SomeTreacheryCard deepFissure
        , SomeTreacheryCard foolOfATook
        , SomeTreacheryCard shadowOfFearJourneyInTheDark
        , SomeTreacheryCard pursuedByTheEnemy
        , SomeTreacheryCard growingThreat
        , SomeTreacheryCard orcsOfTheWhiteHand
        , SomeTreacheryCard blackFeatheredArrows
        , SomeTreacheryCard fallenIntoEvil
        , SomeTreacheryCard followedByNightTheRoadDarkens
        , SomeTreacheryCard illFate
        , SomeTreacheryCard blastOfSorcery
        , SomeTreacheryCard poisonedVapour
        , SomeTreacheryCard machinesOfIsengard
        , SomeTreacheryCard sarumansVoice
        , SomeTreacheryCard restByNight
        , SomeTreacheryCard strangeWeariness
        , SomeTreacheryCard unseenBarrier
        , SomeTreacheryCard fightingUrukHai
        , SomeTreacheryCard sarumansOrders
        , SomeTreacheryCard scalingLadders
        , SomeTreacheryCard devilryOfSaruman
        , SomeTreacheryCard recklessHate
        , SomeTreacheryCard nightWithoutEnd
        , SomeTreacheryCard poisonedCounselsTheTreasonOfSaruman
        , SomeTreacheryCard aGrowingThreat
        , SomeTreacheryCard urdugsCunning
        , SomeTreacheryCard misplacedTrust
        , SomeTreacheryCard dragonSickness
        ]
    ]
