module LordOfTheRings.Treachery.CardDefs (module LordOfTheRings.Treachery.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Trait

allTreacheryCards :: Map CardCode CardDef
allTreacheryCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ eyesOfTheForest
      , losingTime
      , bedlamInTheStreets
      , theThievesPloy
      , theyreGettingAway
      , obsidianArrows
      , savageFray
      , ruinAndWroth
      , theSpidersWeb
      , backtrack
      , pursuitOnTheShore
      , trollAttack
      , smokingBlood
      , sentinelOfShadow
      , catacombInspection
      , darkInterrogation
      , caveIn
      , crumblingRuin
      , darkAndDreadful
      , suddenPitfall
      , watchfulEyes
      , signsOfConflict
      , manyRoads
      , burningLow
      , caughtInAWeb
      , despair
      , drivenByShadow
      , theNecromancersReach
      , underTheShadow
      , ironShackles
      , massingAtNight
      , evilStorm
      , pursuedByShadow
      , treacherousFog
      , lingeringVenom
      , falseLead
      , flooding
      , oldWivesTales
      , aFrightenedBeast
      , sacked
      , roastedSlowly
      , exhaustion
      , swarmingInsect
      , festeringWounds
      , impassableChasm
      , rockslide
      , slickFooting
      , aWispOfPaleSheen
      , nightfall
      , throughTheMist
      , theLightsOfTheDead
      , gollumsAnguish
      , gollumsBite
      , wastedProvisions
      , newDevilry
      , shadowOfFear
      , aFoeBeyond
      , chanceEncounter
      , massingInTheDeep
      , undisturbedBones
      , bitterWind
      , hiddenThreat
      , fatigue
      , foulAir
      , gatheringGround
      , vastAndIntricate
      , sleepingSentry
      , followedByNight
      , orcAmbush
      , fellVoices
      , fallenStones
      , snowstorm
      , freezingCold
      , avalanche
      , deepDeepDark
      , lostAndAlone
      , firesInTheDeep
      , innerFlame
      , innerShadow
      , whipLash
      , leapingFlame
      , counterSpell
      , fierySword
      , manyThongedWhip
      , illPurpose
      , disturbedWaters
      , wrapped
      , orcAssault
      , theMastersMalice
      , thePowerOfMordor
      , scourgeOfMordor
      , blockingWargs
      , lostCompany
      , watcherInTheWoods
      , southronSupport
      , collateralDamage
      , lostInTheCity
      , localTrouble
      , lurkingInShadows
      , burntHomestead
      , trappedInside
      , panicked
      , menInTheDark
      , starsInSky
      , leavesOnTree
      , impenetrableFog
      , sleeplessMalice
      , terrorDrivesThem
      , theDeadCityLooms
      , unwelcomeDiscovery
      , aKnifeInTheBack
      , falseLeadTheStewardsFear
      , pinnedDown
      , streetFighting
      , counterAttack
      , lyingInWait
      , mordorLooms
      , conflictAtTheCrossroads
      , orcTerritory
      , theForestsMalice
      , pillagingAndBurning
      , downFromTheHills
      , illTidings
      , orcHuntingParty
      , oldHatreds
      , wildMenOfDunland
      , turnedAround
      , takeCover
      , lowOnProvisions
      , offTrack
      , inNeedOfRest
      , raisingTheCry
      , possession
      , aFellDread
      , deathAndCalamity
      , fierceFolk
      , drivenBack
      , frenziedAttack
      , dunlendingAmbush
      , boarClanAssault
      , stalkedByDunlendings
      , cursedBurden
      , trialOfAncestors
      , waylaidByAgents
      , tharbadPatrol
      , quicksand
      , rottenStench
      , hatefulSearch
      , sauronsAgent
      , closingIn
      , ravenTalisman
      , gatheringForces
      , constantTail
      , conspicuousLot
      , cornered
      , getThatDwarf
      , discovered
      , desecratedRuins
      , spiesFromMordor
      , shiftingMarshland
      , remnantsOfElderDays
      , curseOfTheWildMen
      , theGuardiansFury
      , wildTenacity
      , theSkyDarkens
      , vileAffliction
      , daechanarsWill
      , suddenDarkness
      , predatoryWolves
      , soundTheAlarm
      , feebleAndWeary
      , captivesOfGornakh
      , interrogation
      , suddenAssault
      , surprisingSpeed
      , pressingNeeds
      , weightOfResponsibility
      , iceStorm
      , bitingWind
      , freezingBlast
      , coldFromAngmar
      , aradorsBane
      , noRest
      , unnaturalFog
      , tragicDiscovery
      , restlessEvil
      , aGreatHost
      , killThem
      , rainWashedTracks
      , paralyzingFear
      , curseOfAngmar
      , terrorOfTheNorth
      , darkSorcery
      , heavyCurse
      , centuriesOfSorrow
      , curseOfTheYears
      , darkCovenant
      , hauntingFog
      , thrownOffCourse
      , suddenStorms
      , windsOfWrath
      , boardingParty
      , battleHardened
      , mysteriousFog
      , lingeringMalevolence
      , ruinsOfAgesPast
      , curseOfTheDownfallen
      , aimlessWandering
      , putToTheTorch
      , theFiresSpread
      , manOverboard
      , ragingSquall
      , holdSteady
      , fromTheDepths
      , lashingTentacles
      , powerfulUndertow
      , intoTheAbyss
      , tanglingAndGrasping
      , ancientDepths
      , deeperIntoTheRuins
      , furiousRainstorm
      , dwindlingSupplies
      , sunkenRuins
      , rammingSpeed
      , takingOnWater
      , theRaidersAssault
      , serpentsTail
      , foesOfEcthelion
      , umbarPatrol
      , furyAndMalice
      , ominousFate
      , sendForHelpLeadership
      , sendForHelpTactics
      , sendForHelpSpirit
      , sendForHelpLore
      , brutalAssault
      , mugashsCunning
      , patrollingOrcs
      , mugashsRage
      , stiflingWoods
      , fangornsAnger
      , rogueWave
      , dreamsOfNumenor
      , ragingSquallNightmare
      , corruptedFlora
      , calphonsFolly
      , suddenAssaultNightmare
      , infiltration
      , shadowFall
      , balinsDoom
      , beatsAndEchoes
      , dismayAndFear
      , anEvilFortune
      , darkAndStormy
      , sahirsGettingAway
      , tumultuousWaters
      , deepMaelstrom
      , deceitfulIsland
      , callOfTheCurse
      , suddenPrecipice
      , pitIntoTheDepths
      , sulfurCloud
      , brokenRudder
      , grappled
      , inspiringFury
      , raiseTheAlarm
      , slaveTrade
      , theEnemysTrail
      , restlessHunters
      , theTrailGoesCold
      , timeForSupper
      , shotFromTheSky
      , noxiousSupplies
      , aHurryingDarkness
      , blastOfSavageWind
      , hazardousTerrain
      , retracedSteps
      , candlesOfCorpses
      , swarmingMosquitoes
      , devilryOfTheDarkLand
      , isolation
      , befouledEquipment
      , wickednessAndMischief
      , theTowerBeckons
      , noxiousFumes
      , surroundedByShadow
      , suddenOnslaught
      , drumsOfTheWildMen
      , poisonDarts
      , trappedVillagers
      , runForYourLives
      , saveUs
      , thoseNowSundered
      , relentlessAttack
      , overrunDefenses
      , pressedOnAllSides
      , falseAccusations
      , blindingBlizzard
      , caradhrasTheCruel
      , tumultuousAssault
      , pulledUnder
      , ripplingFromBeneath
      , dreadfulDeep
      , lostAndHelpless
      , chillOfTheRoots
      , terribleStrength
      , theDarkFire
      , blazingGrip
      , morgothsBidding
      , flameOfUdun
      , cunningWolves
      , torrentOfHail
      , cruelTorment
      , goblinPatrolNightmare
      , crashingThunder
      , barrenAndDesolate
      , malevolentForces
      , lifeDrain
      , deathAndDecay
      , howlingOfWargs
      , blackMare
      , powerBeyondDeath
      , daechanarsCurse
      , allIsLost
      , nowhereToHide
      , injuredCompanion
      , enfeebled
      , sandstorm
      , heatExhaustion
      , overgrown
      , fearOfMordor
      , savageAttack
      , overrunByOrcs
      , forcedOffTrackBeneathTheSands
      , grabbedBySpiders
      , mirage
      , unbearableHeat
      , scorchingSun
      , enemyPursuit
      , pursuersFromMordor
      , howlingAtTheirHeels
      , boldAndGrim
      , noQuarter
      , chaosInTheNight
      , takenPrisoner
      , tormentedByOrcs
      , stampedingOliphaunt
      , theSavageSouth
      , terribleFever
      , bitingInsects
      , dangerCloseBehind
      , mountainsOfShadow
      , ruinedSupplies
      , lostInTheWild
      , weighedDown
      , gatheringGloom
      , swarmOfBats
      , afraidOfTheDark
      , darkPlaces
      , eyesInTheDark
      , dragonBreath
      , dragonScales
      , fierceAttack
      , urdugsCommand
      , darkClouds
      , frenziedCreature
      , dangerousCrossing
      , powerfulInWrath
      , theDragonsFury
      , brightFlames
      , denizenOfTheDeep
      , abandonedRuins
      , terribleShadows
      , slammedShut
      , wilderlandsBane
      , suffocatingShadows
      , heavySnow
      , deadlyCold
      , sneakingOff
      , harshWeather
      , wildCreatures
      , unseenDanger
      , vastnessOfMirkwood
      , stinker
      , easterlingHorse
      , swordOfRhun
      , recurveBow
      , wearyLands
      , painfulFatigue
      , overwhelmed
      , darknessInTheEast
      , underGuard
      , ulchorsRage
      , fanaticism
      , fearfulShadows
      , enemiesCloseBehind
      , placeOfEvil
      , extraVigil
      , weveBeenSeen
      , patrollingTheStreets
      , ransackedSupplies
      , burningReek
      , writhingShadows
      , disappearances
      , secretCultist
      , fury
      , caughtOffGuard
      , streetFightingWrathAndRuin
      , underSiege
      , ulchorsRevenge
      , wainriderChariot
      , theChampionsCunning
      , determinedDefense
      , theDarkLordsReach
      , overseersWhip
      , crueltyOfMordor
      , brokenCaptives
      , caughtInWebs
      , poisonedBySpiders
      , fightingAmongFriends
      , weighedDownWilderland
      ]

deepDeepDark :: CardDef
deepDeepDark =
  (treachery "04123" "Deep Deep Dark" FoundationsOfStone 4)
    { cdKeywords = setFromList [Doomed 1, Surge]
    , cdShadowEffect = True
    }

lostAndAlone :: CardDef
lostAndAlone =
  (treachery "04124" "Lost and Alone" FoundationsOfStone 2)
    { cdShadowEffect = True
    }

signsOfConflict :: CardDef
signsOfConflict =
  (treachery "03015" "Signs of Conflict" IntoThePit 5)
    { cdKeywords = setFromList [Doomed 2, Surge]
    , cdWhenRevealed = False
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

manyRoads :: CardDef
manyRoads =
  (treachery "03039" "Many Roads" TwistsAndTurns 1)
    { cdKeywords = setFromList [Surge]
    }

burningLow :: CardDef
burningLow =
  (treachery "03040" "Burning Low" TwistsAndTurns 3)
    { cdShadowEffect = True
    }

caveIn :: CardDef
caveIn =
  (treachery "03048" "Cave In" HazardsOfThePit 3)
    { cdCardTraits = setFromList [Hazard]
    }

crumblingRuin :: CardDef
crumblingRuin =
  (treachery "03049" "Crumbling Ruin" HazardsOfThePit 2)
    { cdCardTraits = setFromList [Hazard]
    , cdEasyModeQuantity = Just 0
    }

darkAndDreadful :: CardDef
darkAndDreadful =
  (treachery "03050" "Dark and Dreadful" HazardsOfThePit 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

suddenPitfall :: CardDef
suddenPitfall =
  (treachery "03051" "Sudden Pitfall" HazardsOfThePit 1)
    { cdCardTraits = setFromList [Hazard]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

watchfulEyes :: CardDef
watchfulEyes =
  (treachery "03062" "Watchful Eyes" GoblinsOfTheDeep 3)
    { cdEasyModeQuantity = Just 0
    }

eyesOfTheForest :: CardDef
eyesOfTheForest =
  (treachery "01079" "Eyes of the Forest" SpidersOfMirkwood 1)
    { cdEasyModeQuantity = Just 0
    }

caughtInAWeb :: CardDef
caughtInAWeb =
  (treachery "01080" "Caught in a Web" SpidersOfMirkwood 2)
    { cdEasyModeQuantity = Just 0
    }

despair :: CardDef
despair =
  (treachery "01086" "Despair" Wilderlands 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

drivenByShadow :: CardDef
drivenByShadow = (treachery "01092" "Driven by Shadow" DolGuldurOrcs 1) {cdShadowEffect = True}

theNecromancersReach :: CardDef
theNecromancersReach =
  (treachery "01093" "The Necromancer's Reach" DolGuldurOrcs 3)
    { cdEasyModeQuantity = Just 1
    }

underTheShadow :: CardDef
underTheShadow = (treachery "01104" "Under the Shadow" EscapeFromDolGuldur 2) {cdShadowEffect = True}

ironShackles :: CardDef
ironShackles = (treachery "01105" "Iron Shackles" EscapeFromDolGuldur 1) {cdShadowEffect = True}

massingAtNight :: CardDef
massingAtNight =
  (treachery "01112" "Massing at Night" JourneyAlongTheAnduin 1)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

evilStorm :: CardDef
evilStorm =
  (treachery "01116" "Evil Storm" SauronsReach 3)
    { cdEasyModeQuantity = Just 2
    }

pursuedByShadow :: CardDef
pursuedByShadow = (treachery "01117" "Pursued by Shadow" SauronsReach 2) {cdShadowEffect = True}

treacherousFog :: CardDef
treacherousFog = treachery "01118" "Treacherous Fog" SauronsReach 2

lingeringVenom :: CardDef
lingeringVenom =
  (treachery "RC005" "Lingering Venom" PassageThroughMirkwoodCampaign 1)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Burden]
    , cdKeywords = setFromList [Surge]
    }

falseLead :: CardDef
falseLead = treachery "02022" "False Lead" TheHuntForGollum 2

flooding :: CardDef
flooding =
  (treachery "02023" "Flooding" TheHuntForGollum 2)
    { cdCardTraits = setFromList [Disaster]
    , cdKeywords = setFromList [Doomed 1, Surge]
    , cdShadowEffect = True
    }

oldWivesTales :: CardDef
oldWivesTales =
  (treachery "02024" "Old Wives' Tales" TheHuntForGollum 3)
    { cdCardTraits = setFromList [Gossip]
    , cdEasyModeQuantity = Just 1
    }

aFrightenedBeast :: CardDef
aFrightenedBeast =
  (treachery "02047" "A Frightened Beast" ConflictAtTheCarrock 3)
    { cdEasyModeQuantity = Just 1
    }

sacked :: CardDef
sacked =
  (treachery "02048" "Sacked!" ConflictAtTheCarrock 5)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 4
    }

roastedSlowly :: CardDef
roastedSlowly =
  (treachery "02049" "Roasted Slowly" ConflictAtTheCarrock 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

exhaustion :: CardDef
exhaustion =
  (treachery "02067" "Exhaustion" AJourneyToRhosgobel 4)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

swarmingInsect :: CardDef
swarmingInsect =
  (treachery "02068" "Swarming Insect" AJourneyToRhosgobel 4)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

festeringWounds :: CardDef
festeringWounds =
  (treachery "02069" "Festering Wounds" AJourneyToRhosgobel 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

impassableChasm :: CardDef
impassableChasm =
  (treachery "02091" "Impassable Chasm" TheHillsOfEmynMuil 4)
    { cdCardTraits = setFromList [Hazard]
    , cdEasyModeQuantity = Just 2
    }

rockslide :: CardDef
rockslide =
  (treachery "02092" "Rockslide" TheHillsOfEmynMuil 3)
    { cdCardTraits = setFromList [Hazard]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

slickFooting :: CardDef
slickFooting =
  (treachery "02093" "Slick Footing" TheHillsOfEmynMuil 3)
    { cdCardTraits = setFromList [Hazard]
    }

aWispOfPaleSheen :: CardDef
aWispOfPaleSheen =
  (treachery "02108" "A Wisp of Pale Sheen" TheDeadMarshes 3)
    { cdCardTraits = setFromList [Escape]
    , cdEscape = Just 4
    , cdEasyModeQuantity = Just 1
    }

nightfall :: CardDef
nightfall =
  (treachery "02109" "Nightfall" TheDeadMarshes 3)
    { cdCardTraits = setFromList [Escape]
    , cdEscape = Just 2
    }

throughTheMist :: CardDef
throughTheMist =
  (treachery "02110" "Through the Mist" TheDeadMarshes 3)
    { cdCardTraits = setFromList [Escape]
    , cdEscape = Just 3
    }

theLightsOfTheDead :: CardDef
theLightsOfTheDead =
  (treachery "02111" "The Lights of the Dead" TheDeadMarshes 4)
    { cdCardTraits = setFromList [Escape]
    , cdEscape = Just 5
    , cdEasyModeQuantity = Just 2
    }

gollumsAnguish :: CardDef
gollumsAnguish =
  (treachery "02135" "Gollum's Anguish" ReturnToMirkwood 2)
    { cdCardTraits = setFromList [Tantrum]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

gollumsBite :: CardDef
gollumsBite =
  (treachery "02136" "Gollum's Bite" ReturnToMirkwood 2)
    { cdCardTraits = setFromList [Tantrum]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

wastedProvisions :: CardDef
wastedProvisions =
  (treachery "02137" "Wasted Provisions" ReturnToMirkwood 3)
    { cdCardTraits = setFromList [Tantrum]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

bitterWind :: CardDef
bitterWind =
  (treachery "03056" "Bitter Wind" MistyMountains 3)
    { cdShadowEffect = True
    }

undisturbedBones :: CardDef
undisturbedBones =
  (treachery "03033" "Undisturbed Bones" PlunderingGoblins 3)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

hiddenThreat :: CardDef
hiddenThreat = treachery "03022" "Hidden Threat" TheSeventhLevel 2

chanceEncounter :: CardDef
chanceEncounter = (treachery "03045" "Chance Encounter" DeepsOfMoria 3) {cdShadowEffect = True}

massingInTheDeep :: CardDef
massingInTheDeep =
  (treachery "03046" "Massing in the Deep" DeepsOfMoria 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

newDevilry :: CardDef
newDevilry = (treachery "03026" "New Devilry" FlightFromMoria 3) {cdShadowEffect = True}

shadowOfFear :: CardDef
shadowOfFear = (treachery "03027" "Shadow of Fear" FlightFromMoria 3) {cdEasyModeQuantity = Just 1}

aFoeBeyond :: CardDef
aFoeBeyond =
  (treachery "03028" "A Foe Beyond" FlightFromMoria 4)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

fatigue :: CardDef
fatigue = (treachery "04093" "Fatigue" TheLongDark 2) {cdShadowEffect = True}

foulAir :: CardDef
foulAir = treachery "04094" "Foul Air" TheLongDark 4

gatheringGround :: CardDef
gatheringGround = treachery "04095" "Gathering Ground" TheLongDark 1

vastAndIntricate :: CardDef
vastAndIntricate = treachery "04096" "Vast and Intricate" TheLongDark 2

sleepingSentry :: CardDef
sleepingSentry =
  (treachery "04046" "Sleeping Sentry" RoadToRivendell 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

followedByNight :: CardDef
followedByNight =
  (treachery "04047" "Followed by Night" RoadToRivendell 3)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

orcAmbush :: CardDef
orcAmbush =
  (treachery "04048" "Orc Ambush" RoadToRivendell 2)
    { cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 0
    }


fellVoices :: CardDef
fellVoices = treachery "04020" "Fell Voices" TheRedhornGate 2

fallenStones :: CardDef
fallenStones =
  (treachery "04021" "Fallen Stones" TheRedhornGate 2)
    { cdShadowEffect = True
    }

snowstorm :: CardDef
snowstorm =
  (treachery "04022" "Snowstorm" TheRedhornGate 5)
    { cdCardTraits = setFromList [Snow]
    , cdShadowEffect = True
    }

freezingCold :: CardDef
freezingCold =
  (treachery "04023" "Freezing Cold" TheRedhornGate 3)
    { cdCardTraits = setFromList [Snow]
    }

avalanche :: CardDef
avalanche =
  (treachery "04024" "Avalanche!" TheRedhornGate 1)
    { cdCardTraits = setFromList [Snow]
    }


firesInTheDeep :: CardDef
firesInTheDeep =
  (treachery "04144" "Fires in the Deep" ShadowAndFlame 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

counterSpell :: CardDef
counterSpell = (treachery "04145" "Counter-Spell" ShadowAndFlame 3) {cdCardTraits = setFromList [Shadow]}

innerFlame :: CardDef
innerFlame = (treachery "04146" "Inner Flame" ShadowAndFlame 3) {cdShadowEffect = True}

innerShadow :: CardDef
innerShadow = (treachery "04147" "Inner Shadow" ShadowAndFlame 3) {cdKeywords = setFromList [Surge]}

whipLash :: CardDef
whipLash = (treachery "04148" "Whip Lash" ShadowAndFlame 2) {cdShadowEffect = True}

leapingFlame :: CardDef
leapingFlame =
  (treachery "04149" "Leaping Flame" ShadowAndFlame 3)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

fierySword :: CardDef
fierySword =
  (treachery "04152" "Fiery Sword" ShadowAndFlame 2)
    { cdCardTraits = setFromList [Weapon, Flame]
    , cdUnique = True
    }

manyThongedWhip :: CardDef
manyThongedWhip =
  (treachery "04153" "Many Thonged Whip" ShadowAndFlame 2)
    { cdCardTraits = setFromList [Weapon, Flame]
    , cdUnique = True
    }


illPurpose :: CardDef
illPurpose = (treachery "04070" "Ill Purpose" TheWatcherInTheWater 1) {cdShadowEffect = True}

disturbedWaters :: CardDef
disturbedWaters =
  (treachery "04071" "Disturbed Waters" TheWatcherInTheWater 3)
    { cdKeywords = setFromList [Doomed 5]
    , cdWhenRevealed = False
    }

-- | Its database code: Háma has 04076, his RingsDB code.
wrapped :: CardDef
wrapped =
  (treachery "the-watcher-in-the-water-076" "Wrapped!" TheWatcherInTheWater 4)
    { cdCardTraits = setFromList [Tentacle]
    }

orcAssault :: CardDef
orcAssault = (treachery "05036" "Orc Assault" TheSiegeOfCairAndros 2) {cdShadowEffect = True}

theMastersMalice :: CardDef
theMastersMalice = (treachery "05060" "The Master's Malice" MordorElite 2) {cdEasyModeQuantity = Just 0}

thePowerOfMordor :: CardDef
thePowerOfMordor = (treachery "05061" "The Power of Mordor" MordorElite 1) {cdKeywords = setFromList [Doomed 3]}

scourgeOfMordor :: CardDef
scourgeOfMordor = (treachery "05064" "Scourge of Mordor" RavagingOrcs 2) {cdShadowEffect = True}

blockingWargs :: CardDef
blockingWargs =
  (treachery "05029" "Blocking Wargs" IntoIthilien 4)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

lostCompany :: CardDef
lostCompany = (treachery "05048" "Lost Company" BroodingForest 2) {cdEasyModeQuantity = Just 0}

watcherInTheWoods :: CardDef
watcherInTheWoods =
  (treachery "05051" "Watcher in the Woods" CreaturesOfTheForest 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

southronSupport :: CardDef
southronSupport =
  (treachery "05055" "Southron Support" Southrons 2)
    { cdKeywords = setFromList [Doomed 3]
    , cdEasyModeQuantity = Just 0
    }

collateralDamage :: CardDef
collateralDamage =
  (treachery "05024" "Collateral Damage" PerilInPelargir 4)
    { cdShadowEffect = True
    }

lostInTheCity :: CardDef
lostInTheCity =
  (treachery "05040" "Lost in the City" StreetsOfGondor 1)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

localTrouble :: CardDef
localTrouble =
  (treachery "05041" "Local Trouble" StreetsOfGondor 2)
    { cdEasyModeQuantity = Just 1
    }

lurkingInShadows :: CardDef
lurkingInShadows =
  (treachery "05045" "Lurking in Shadows" Brigands 2)
    { cdShadowEffect = True
    }

burntHomestead :: CardDef
burntHomestead =
  (treachery "06078" "Burnt Homestead" EncounterAtAmonDin 2)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

trappedInside :: CardDef
trappedInside =
  (treachery "06079" "Trapped Inside" EncounterAtAmonDin 2)
    { cdKeywords = setFromList [Doomed 2]
    , cdShadowEffect = True
    }

panicked :: CardDef
panicked = (treachery "06080" "Panicked!" EncounterAtAmonDin 2) {cdShadowEffect = True}

menInTheDark :: CardDef
menInTheDark = (treachery "06053" "Men in the Dark" TheDruadanForest 3) {cdShadowEffect = True}

starsInSky :: CardDef
starsInSky =
  (treachery "06054" "Stars in Sky" TheDruadanForest 3)
    { cdKeywords = setFromList [Prowl 2]
    , cdShadowEffect = True
    }

leavesOnTree :: CardDef
leavesOnTree =
  (treachery "06055" "Leaves on Tree" TheDruadanForest 2)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

impenetrableFog :: CardDef
impenetrableFog = (treachery "06157" "Impenetrable Fog" TheMorgulVale 2) {cdEasyModeQuantity = Just 0}

sleeplessMalice :: CardDef
sleeplessMalice =
  (treachery "06158" "Sleepless Malice" TheMorgulVale 2)
    { cdKeywords = setFromList [Doomed 2]
    , cdShadowEffect = True
    }

terrorDrivesThem :: CardDef
terrorDrivesThem =
  (treachery "06159" "Terror Drives Them" TheMorgulVale 3)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

theDeadCityLooms :: CardDef
theDeadCityLooms =
  (treachery "06160" "The Dead City Looms" TheMorgulVale 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

unwelcomeDiscovery :: CardDef
unwelcomeDiscovery =
  (treachery "06023" "Unwelcome Discovery" TheStewardsFear 2)
    { cdKeywords = setFromList [Surge]
    }

aKnifeInTheBack :: CardDef
aKnifeInTheBack =
  (treachery "06024" "A Knife in the Back" TheStewardsFear 3)
    { cdEasyModeQuantity = Just 1
    }

falseLeadTheStewardsFear :: CardDef
falseLeadTheStewardsFear =
  (treachery "06025" "False Lead" TheStewardsFear 2)
    { cdShadowEffect = True
    }

pinnedDown :: CardDef
pinnedDown =
  (treachery "06104" "Pinned Down" AssaultOnOsgiliath 2)
    { cdShadowEffect = True
    }

streetFighting :: CardDef
streetFighting = treachery "06105" "Street Fighting" AssaultOnOsgiliath 2

counterAttack :: CardDef
counterAttack =
  (treachery "06106" "Counter-attack" AssaultOnOsgiliath 2)
    { cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

lyingInWait :: CardDef
lyingInWait = treachery "06129" "Lying in Wait" TheBloodOfGondor 4

mordorLooms :: CardDef
mordorLooms =
  (treachery "06130" "Mordor Looms" TheBloodOfGondor 3)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

conflictAtTheCrossroads :: CardDef
conflictAtTheCrossroads =
  (treachery "06131" "Conflict at the Crossroads" TheBloodOfGondor 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

orcTerritory :: CardDef
orcTerritory = (treachery "07030" "Orc Territory" ToCatchAnOrc 2) {cdEasyModeQuantity = Just 0}

theForestsMalice :: CardDef
theForestsMalice = (treachery "07037" "The Forest's Malice" IntoFangorn 4) {cdEasyModeQuantity = Just 1}

pillagingAndBurning :: CardDef
pillagingAndBurning =
  (treachery "07021" "Pillaging and Burning" TheFordsOfIsen 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

downFromTheHills :: CardDef
downFromTheHills = (treachery "07022" "Down from the Hills" TheFordsOfIsen 1) {cdEasyModeQuantity = Just 0}

illTidings :: CardDef
illTidings = treachery "07023" "Ill Tidings" TheFordsOfIsen 2

orcHuntingParty :: CardDef
orcHuntingParty = treachery "07041" "Orc Hunting Party" MistyMountainOrcs 2

oldHatreds :: CardDef
oldHatreds =
  (treachery "07045" "Old Hatreds" DunlandRaiders 2)
    { cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

wildMenOfDunland :: CardDef
wildMenOfDunland =
  (treachery "07049" "Wild Men of Dunland" DunlandWarriors 2)
    { cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

turnedAround :: CardDef
turnedAround = (treachery "07052" "Turned Around" AncientForest 3) {cdShadowEffect = True}

takeCover :: CardDef
takeCover =
  (treachery "07055" "Take Cover!" BrokenLands 3)
    { cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 2
    }

lowOnProvisions :: CardDef
lowOnProvisions =
  (treachery "07056" "Low on Provisions" WearyTravelers 3)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

offTrack :: CardDef
offTrack = (treachery "07057" "Off Track" WearyTravelers 2) {cdKeywords = setFromList [Surge]}

inNeedOfRest :: CardDef
inNeedOfRest = (treachery "07058" "In Need of Rest" WearyTravelers 2) {cdEasyModeQuantity = Just 1}

raisingTheCry :: CardDef
raisingTheCry = (treachery "08159" "Raising the Cry" TheAntleredCrown 3) {cdEasyModeQuantity = Just 2}

fierceFolk :: CardDef
fierceFolk =
  (treachery "08160" "Fierce Folk" TheAntleredCrown 2)
    { cdKeywords = setFromList [Doomed 3]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

drivenBack :: CardDef
drivenBack = treachery "08161" "Driven Back" TheAntleredCrown 2

frenziedAttack :: CardDef
frenziedAttack = (treachery "08023" "Frenzied Attack" TheDunlandTrap 3) {cdShadowEffect = True}

dunlendingAmbush :: CardDef
dunlendingAmbush =
  (treachery "08024" "Dunlending Ambush" TheDunlandTrap 3)
    { cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

constantTail :: CardDef
constantTail = treachery "08080" "Constant Tail" TroubleInTharbad 2

conspicuousLot :: CardDef
conspicuousLot =
  (treachery "08081" "Conspicuous Lot" TroubleInTharbad 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

cornered :: CardDef
cornered = (treachery "08082" "Cornered" TroubleInTharbad 2) {cdShadowEffect = True}

getThatDwarf :: CardDef
getThatDwarf =
  (treachery "08083" "Get That Dwarf!" TroubleInTharbad 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

discovered :: CardDef
discovered =
  (treachery "08134" "Discovered!" CelebrimborsSecret 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

desecratedRuins :: CardDef
desecratedRuins = (treachery "08135" "Desecrated Ruins" CelebrimborsSecret 3) {cdShadowEffect = True}

spiesFromMordor :: CardDef
spiesFromMordor = (treachery "08136" "Spies from Mordor" CelebrimborsSecret 2) {cdShadowEffect = True}

shiftingMarshland :: CardDef
shiftingMarshland =
  (treachery "08110" "Shifting Marshland" TheNinInEilph 3)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

remnantsOfElderDays :: CardDef
remnantsOfElderDays = (treachery "08111" "Remnants of Elder Days" TheNinInEilph 1) {cdEasyModeQuantity = Just 0}

curseOfTheWildMen :: CardDef
curseOfTheWildMen =
  (treachery "08053" "Curse of the Wild Men" TheThreeTrials 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

theGuardiansFury :: CardDef
theGuardiansFury =
  (treachery "08054" "The Guardian's Fury" TheThreeTrials 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

wildTenacity :: CardDef
wildTenacity =
  (treachery "08055" "Wild Tenacity" TheThreeTrials 3)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

theSkyDarkens :: CardDef
theSkyDarkens =
  (treachery "10136" "The Sky Darkens" TheBattleOfCarnDum 2)
    { cdCardTraits = setFromList [Sorcery]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

vileAffliction :: CardDef
vileAffliction =
  (treachery "10137" "Vile Affliction" TheBattleOfCarnDum 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdShadowEffect = True
    }

daechanarsWill :: CardDef
daechanarsWill = (treachery "10138" "Daechanar's Will" TheBattleOfCarnDum 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

suddenDarkness :: CardDef
suddenDarkness =
  (treachery "10026" "Sudden Darkness" TheWastesOfEriador 2)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

predatoryWolves :: CardDef
predatoryWolves = (treachery "10027" "Predatory Wolves" TheWastesOfEriador 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

soundTheAlarm :: CardDef
soundTheAlarm =
  (treachery "10049" "Sound the Alarm!" EscapeFromMountGram 2)
    { cdKeywords = setFromList [Doomed 3]
    , cdEasyModeQuantity = Just 1
    }

feebleAndWeary :: CardDef
feebleAndWeary = (treachery "10050" "Feeble and Weary" EscapeFromMountGram 2) {cdShadowEffect = True}

captivesOfGornakh :: CardDef
captivesOfGornakh =
  (treachery "10051" "Captives of Gornákh" EscapeFromMountGram 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

interrogation :: CardDef
interrogation = (treachery "10052" "Interrogation" EscapeFromMountGram 2) {cdShadowEffect = True}

suddenAssault :: CardDef
suddenAssault = (treachery "09021" "Sudden Assault" IntrudersInChetwood 2) {cdEasyModeQuantity = Just 0}

surprisingSpeed :: CardDef
surprisingSpeed =
  (treachery "09022" "Surprising Speed" IntrudersInChetwood 3)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

pressingNeeds :: CardDef
pressingNeeds =
  (treachery "09049" "Pressing Needs" EriadorWilds 2)
    { cdKeywords = setFromList [Doomed 2]
    , cdEasyModeQuantity = Just 1
    }

weightOfResponsibility :: CardDef
weightOfResponsibility =
  (treachery "09050" "Weight of Responsibility" EriadorWilds 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

iceStorm :: CardDef
iceStorm =
  (treachery "09031" "Ice Storm" TheWeatherHills 2)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

bitingWind :: CardDef
bitingWind =
  (treachery "09052" "Biting Wind" FoulWeather 3)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

freezingBlast :: CardDef
freezingBlast =
  (treachery "09053" "Freezing Blast" FoulWeather 2)
    { cdCardTraits = setFromList [Weather]
    , cdKeywords = setFromList [Doomed 2]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

coldFromAngmar :: CardDef
coldFromAngmar =
  (treachery "09054" "Cold from Angmar" FoulWeather 3)
    { cdCardTraits = setFromList [Weather]
    , cdEasyModeQuantity = Just 2
    }

aradorsBane :: CardDef
aradorsBane = (treachery "10079" "Arador's Bane" AcrossTheEttenmoors 2) {cdEasyModeQuantity = Just 1}

noRest :: CardDef
noRest = treachery "10080" "No Rest" AcrossTheEttenmoors 3

unnaturalFog :: CardDef
unnaturalFog =
  (treachery "09041" "Unnatural Fog" DeadmensDike 2)
    { cdCardTraits = setFromList [Sorcery]
    , cdKeywords = setFromList [Doomed 2]
    , cdEasyModeQuantity = Just 1
    }

tragicDiscovery :: CardDef
tragicDiscovery = (treachery "09058" "Tragic Discovery" RuinsOfArnor 2) {cdShadowEffect = True}

restlessEvil :: CardDef
restlessEvil = (treachery "09062" "Restless Evil" CursedDead 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

terrorOfTheNorth :: CardDef
terrorOfTheNorth =
  (treachery "09064" "Terror of the North" DarkSorcery 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

darkSorcery :: CardDef
darkSorcery =
  (treachery "09065" "Dark Sorcery" DarkSorcery 2)
    { cdCardTraits = setFromList [Sorcery]
    , cdKeywords = setFromList [Doomed 2]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

heavyCurse :: CardDef
heavyCurse =
  (treachery "09066" "Heavy Curse" DarkSorcery 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

possession :: CardDef
possession =
  (treachery "10162" "Possession" TheDreadRealm 2)
    { cdCardTraits = setFromList [Sorcery]
    , cdKeywords = setFromList [Doomed 1]
    , cdEasyModeQuantity = Just 0
    }

aFellDread :: CardDef
aFellDread = (treachery "10163" "A Fell Dread" TheDreadRealm 2) {cdCardTraits = setFromList [Sorcery]}

deathAndCalamity :: CardDef
deathAndCalamity =
  (treachery "10164" "Death and Calamity" TheDreadRealm 4)
    { cdCardTraits = setFromList [Sorcery]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

centuriesOfSorrow :: CardDef
centuriesOfSorrow =
  (treachery "10105" "Centuries of Sorrow" TheTreacheryOfRhudaur 2)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

curseOfTheYears :: CardDef
curseOfTheYears =
  (treachery "10106" "Curse of the Years" TheTreacheryOfRhudaur 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

darkCovenant :: CardDef
darkCovenant =
  (treachery "10107" "Dark Covenant" TheTreacheryOfRhudaur 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdShadowEffect = True
    }

hauntingFog :: CardDef
hauntingFog = (treachery "10108" "Haunting Fog" TheTreacheryOfRhudaur 3) {cdCardTraits = setFromList [Sorcery]}

-- * The Dream-chaser cycle: Vast Oceans, Stormy Weather, Umbar Navy, Corsair Raiders

thrownOffCourse :: CardDef
thrownOffCourse =
  (treachery "11052" "Thrown Off Course" VastOceans 2)
    { cdShadowEffect = True
    , cdSailingIcons = 1
    }

suddenStorms :: CardDef
suddenStorms =
  (treachery "11055" "Sudden Storms" StormyWeather 3)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

windsOfWrath :: CardDef
windsOfWrath =
  (treachery "11056" "Winds of Wrath" StormyWeather 2)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    , cdSailingIcons = 1
    , cdEasyModeQuantity = Just 1
    }

boardingParty :: CardDef
boardingParty =
  (treachery "11059" "Boarding Party" UmbarNavy 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

battleHardened :: CardDef
battleHardened =
  (treachery "11067" "Battle-Hardened" CorsairRaiders 2)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 0
    }

mysteriousFog :: CardDef
mysteriousFog = (treachery "11033" "Mysterious Fog" TheFateOfNumenor 2) {cdShadowEffect = True}

lingeringMalevolence :: CardDef
lingeringMalevolence = (treachery "11034" "Lingering Malevolence" TheFateOfNumenor 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

ruinsOfAgesPast :: CardDef
ruinsOfAgesPast = (treachery "11035" "Ruins of Ages Past" TheFateOfNumenor 2) {cdKeywords = setFromList [Doomed 2], cdEasyModeQuantity = Just 1}

curseOfTheDownfallen :: CardDef
curseOfTheDownfallen = (treachery "11071" "Curse of the Downfallen" DrownedDead 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 0}

aimlessWandering :: CardDef
aimlessWandering = treachery "11075" "Aimless Wandering" RuinsOfNumenor 3

putToTheTorch :: CardDef
putToTheTorch = (treachery "11048" "Put to the Torch" RaidOnTheGreyHavens 2) {cdShadowEffect = True}

theFiresSpread :: CardDef
theFiresSpread =
  (treachery "11049" "The Fires Spread" RaidOnTheGreyHavens 2)
    { cdKeywords = setFromList [Doomed 2]
    , cdShadowEffect = True
    }

-- * Flight of the Stormcaller

manOverboard :: CardDef
manOverboard = (treachery "12028" "Man Overboard!" FlightOfTheStormcaller 2) {cdShadowEffect = True}

ragingSquall :: CardDef
ragingSquall = (treachery "12029" "Raging Squall" FlightOfTheStormcaller 2) {cdKeywords = setFromList [Doomed 2], cdEasyModeQuantity = Just 1}

holdSteady :: CardDef
holdSteady = (treachery "12048" "Hold Steady!" TheStormcaller 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

fromTheDepths :: CardDef
fromTheDepths = (treachery "12055" "From the Depths" TheThingInTheDepths 3) {cdShadowEffect = True}

lashingTentacles :: CardDef
lashingTentacles = (treachery "12056" "Lashing Tentacles" TheThingInTheDepths 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 0}

powerfulUndertow :: CardDef
powerfulUndertow = (treachery "12105" "Powerful Undertow" TheDrownedRuins 3) {cdCardTraits = setFromList [Hazard], cdEasyModeQuantity = Just 1}

intoTheAbyss :: CardDef
intoTheAbyss = (treachery "12106" "Into the Abyss" TheDrownedRuins 3) {cdShadowEffect = True}

tanglingAndGrasping :: CardDef
tanglingAndGrasping =
  (treachery "12107" "Tangling and Grasping" TheDrownedRuins 4)
    { cdCardTraits = setFromList [Hazard]
    , cdShadowEffect = True
    }

ancientDepths :: CardDef
ancientDepths = (treachery "12108" "Ancient Depths" TheDrownedRuins 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

deeperIntoTheRuins :: CardDef
deeperIntoTheRuins = treachery "12078" "Deeper into the Ruins" TempleOfTheDeceived 4

furiousRainstorm :: CardDef
furiousRainstorm = (treachery "12079" "Furious Rainstorm" TempleOfTheDeceived 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

dwindlingSupplies :: CardDef
dwindlingSupplies = (treachery "12080" "Dwindling Supplies" TempleOfTheDeceived 3) {cdShadowEffect = True}

sunkenRuins :: CardDef
sunkenRuins = (treachery "12081" "Sunken Ruins" TempleOfTheDeceived 3) {cdKeywords = setFromList [Doomed 1]}

-- * A Storm on Cobas Haven

rammingSpeed :: CardDef
rammingSpeed = (treachery "12131" "Ramming Speed!" AStormOnCobasHaven 2) {cdSailingIcons = 1}

takingOnWater :: CardDef
takingOnWater = (treachery "12132" "Taking on Water" AStormOnCobasHaven 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

theRaidersAssault :: CardDef
theRaidersAssault = (treachery "12133" "The Raiders' Assault" AStormOnCobasHaven 2) {cdSailingIcons = 1}

-- * The City of Corsairs: Coast of Umbar and The City of Corsairs

serpentsTail :: CardDef
serpentsTail = treachery "12157" "Serpent's Tail" CoastOfUmbar 2

foesOfEcthelion :: CardDef
foesOfEcthelion = (treachery "12163" "Foes of Ecthelion" TheCityOfCorsairs 2) {cdEasyModeQuantity = Just 1}

umbarPatrol :: CardDef
umbarPatrol = treachery "12164" "Umbar Patrol" TheCityOfCorsairs 2

furyAndMalice :: CardDef
furyAndMalice = (treachery "12165" "Fury and Malice" TheCityOfCorsairs 3) {cdEasyModeQuantity = Just 2}

-- Dream-chaser Nightmare Decks

darkAndStormy :: CardDef
darkAndStormy =
  (treachery "flight-of-the-stormcaller-nightmare-flight-of-the-stormcaller-007" "Dark and Stormy" FlightOfTheStormcallerNightmare 3)
    { cdCardTraits = setFromList [Weather]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

sahirsGettingAway :: CardDef
sahirsGettingAway =
  (treachery "flight-of-the-stormcaller-nightmare-flight-of-the-stormcaller-008" "Sahír's Getting Away!" FlightOfTheStormcallerNightmare 3)
    { cdShadowEffect = True
    }

tumultuousWaters :: CardDef
tumultuousWaters =
  (treachery "the-thing-in-the-depths-nightmare-the-thing-in-the-depths-004" "Tumultuous Waters" TheThingInTheDepthsNightmare 3)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    }

deepMaelstrom :: CardDef
deepMaelstrom =
  (treachery "the-thing-in-the-depths-nightmare-the-thing-in-the-depths-008" "Deep Maelstrom" TheThingInTheDepthsNightmare 3)
    { cdShadowEffect = True
    }

deceitfulIsland :: CardDef
deceitfulIsland =
  (treachery "temple-of-the-deceived-nightmare-temple-of-the-deceived-007" "Deceitful Island" TempleOfTheDeceivedNightmare 2)
    { cdKeywords = setFromList [Doomed 3]
    }

callOfTheCurse :: CardDef
callOfTheCurse =
  (treachery "temple-of-the-deceived-nightmare-temple-of-the-deceived-008" "Call of the Curse" TempleOfTheDeceivedNightmare 3)
    { cdShadowEffect = True
    }

suddenPrecipice :: CardDef
suddenPrecipice =
  (treachery "temple-of-the-deceived-nightmare-temple-of-the-deceived-009" "Sudden Precipice" TempleOfTheDeceivedNightmare 3)
    { cdShadowEffect = True
    }

pitIntoTheDepths :: CardDef
pitIntoTheDepths =
  (treachery "the-drowned-ruins-nightmare-the-drowned-ruins-010" "Pit into the Depths" TheDrownedRuinsNightmare 3)
    { cdCardTraits = setFromList [Hazard]
    , cdShadowEffect = True
    }

sulfurCloud :: CardDef
sulfurCloud =
  (treachery "the-drowned-ruins-nightmare-the-drowned-ruins-011" "Sulfur Cloud" TheDrownedRuinsNightmare 2)
    { cdCardTraits = setFromList [Hazard]
    }

brokenRudder :: CardDef
brokenRudder =
  (treachery "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-009" "Broken Rudder" AStormOnCobasHavenNightmare 2)
    { cdSailingIcons = 1
    }

grappled :: CardDef
grappled =
  (treachery "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-010" "Grappled!" AStormOnCobasHavenNightmare 2)
    { cdShadowEffect = True
    }

inspiringFury :: CardDef
inspiringFury =
  (treachery "the-city-of-corsairs-nightmare-the-city-of-corsairs-006" "Inspiring Fury" CoastOfUmbarNightmare 2)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

raiseTheAlarm :: CardDef
raiseTheAlarm =
  (treachery "the-city-of-corsairs-nightmare-the-city-of-corsairs-009" "Raise the Alarm!" TheCityOfCorsairsNightmare 2)
    { cdShadowEffect = True
    }

slaveTrade :: CardDef
slaveTrade =
  (treachery "the-city-of-corsairs-nightmare-the-city-of-corsairs-010" "Slave Trade" TheCityOfCorsairsNightmare 3)
    { cdShadowEffect = True
    }

-- Core Set Nightmare Decks

theSpidersWeb :: CardDef
theSpidersWeb = treachery "core-set-nightmare-passage-through-mirkwood-006" "The Spider's Web" PassageThroughMirkwoodNightmare 3

backtrack :: CardDef
backtrack =
  (treachery "core-set-nightmare-passage-through-mirkwood-007" "Backtrack!" PassageThroughMirkwoodNightmare 2)
    { cdShadowEffect = True
    , cdKeywords = setFromList [Surge]
    }

pursuitOnTheShore :: CardDef
pursuitOnTheShore = treachery "core-set-nightmare-journey-along-the-anduin-007" "Pursuit on the Shore" JourneyAlongTheAnduinNightmare 2

trollAttack :: CardDef
trollAttack =
  (treachery "core-set-nightmare-journey-along-the-anduin-008" "Troll Attack" JourneyAlongTheAnduinNightmare 2)
    { cdShadowEffect = True
    }

smokingBlood :: CardDef
smokingBlood =
  (treachery "core-set-nightmare-journey-along-the-anduin-009" "Smoking Blood" JourneyAlongTheAnduinNightmare 2)
    { cdShadowEffect = True
    }

sentinelOfShadow :: CardDef
sentinelOfShadow = treachery "core-set-nightmare-escape-from-dol-guldur-007" "Sentinel of Shadow" EscapeFromDolGuldurNightmare 2

catacombInspection :: CardDef
catacombInspection =
  (treachery "core-set-nightmare-escape-from-dol-guldur-008" "Catacomb Inspection" EscapeFromDolGuldurNightmare 2)
    { cdShadowEffect = True
    , cdKeywords = setFromList [Surge]
    }

darkInterrogation :: CardDef
darkInterrogation =
  (treachery "core-set-nightmare-escape-from-dol-guldur-009" "Dark Interrogation" EscapeFromDolGuldurNightmare 2)
    { cdShadowEffect = True
    }

-- * The Dream-chaser Campaign Expansion

ominousFate :: CardDef
ominousFate =
  (treachery "DCC176" "Ominous Fate" DreamChaserCampaign 1)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Burden]
    , cdKeywords = setFromList [Surge]
    , cdSailingIcons = 1
    }

-- | A boon, printed as an event with an encounter card's text: set aside, Gavin shuffles
-- one into the encounter deck.
sendForHelp :: CardCode -> CardDef
sendForHelp code =
  (treachery code "Send for Help" DreamChaserCampaign 1)
    { cdShadowEffect = True
    , cdCardTraits = setFromList [Boon]
    , cdKeywords = setFromList [Surge]
    , cdSailingIcons = 1
    }

sendForHelpLeadership :: CardDef
sendForHelpLeadership = sendForHelp "DCC211"

sendForHelpTactics :: CardDef
sendForHelpTactics = sendForHelp "DCC212"

sendForHelpSpirit :: CardDef
sendForHelpSpirit = sendForHelp "DCC213"

sendForHelpLore :: CardDef
sendForHelpLore = sendForHelp "DCC214"
losingTime :: CardDef
losingTime =
  (treachery "AAC183" "Losing Time" AngmarAwakenedCampaign 5)
    { cdCardTraits = setFromList [Burden]
    , cdKeywords = setFromList [Surge]
    , cdWhenRevealed = True
    }

-- Heirs of Númenor Nightmare Decks

bedlamInTheStreets :: CardDef
bedlamInTheStreets =
  (treachery "heirs-of-numenor-nightmare-peril-in-pelargir-007" "Bedlam in the Streets" PerilInPelargirNightmare 3) {cdShadowEffect = True}

theThievesPloy :: CardDef
theThievesPloy =
  (treachery "heirs-of-numenor-nightmare-peril-in-pelargir-008" "The Thieves' Ploy" PerilInPelargirNightmare 2) {cdKeywords = setFromList [Surge]}

theyreGettingAway :: CardDef
theyreGettingAway =
  (treachery "heirs-of-numenor-nightmare-peril-in-pelargir-009" "They're Getting Away!" PerilInPelargirNightmare 2) {cdKeywords = setFromList [Surge]}

obsidianArrows :: CardDef
obsidianArrows =
  (treachery "heirs-of-numenor-nightmare-into-ithilien-008" "Obsidian Arrows" IntoIthilienNightmare 2) {cdShadowEffect = True}

savageFray :: CardDef
savageFray =
  (treachery "heirs-of-numenor-nightmare-into-ithilien-009" "Savage Fray" IntoIthilienNightmare 2) {cdKeywords = setFromList [Doomed 2]}

ruinAndWroth :: CardDef
ruinAndWroth =
  (treachery "heirs-of-numenor-nightmare-the-siege-of-cair-andros-008" "Ruin and Wroth" TheSiegeOfCairAndrosNightmare 3)
    { cdShadowEffect = True
    , cdKeywords = setFromList [Doomed 2]
    }

-- The Voice of Isengard Nightmare Decks

brutalAssault :: CardDef
brutalAssault =
  (treachery "the-voice-of-isengard-nightmare-the-fords-of-isen-008" "Brutal Assault" TheFordsOfIsenNightmare 2)
    { cdShadowEffect = True
    }

mugashsCunning :: CardDef
mugashsCunning =
  (treachery "the-voice-of-isengard-nightmare-to-catch-an-orc-007" "Mugash's Cunning" ToCatchAnOrcNightmare 2)
    { cdShadowEffect = True
    }

patrollingOrcs :: CardDef
patrollingOrcs =
  (treachery "the-voice-of-isengard-nightmare-to-catch-an-orc-008" "Patrolling Orcs" ToCatchAnOrcNightmare 2)
    { cdKeywords = setFromList [Surge, Doomed 1]
    }

mugashsRage :: CardDef
mugashsRage =
  (treachery "the-voice-of-isengard-nightmare-to-catch-an-orc-009" "Mugash's Rage" ToCatchAnOrcNightmare 2)
    { cdShadowEffect = True
    }

stiflingWoods :: CardDef
stiflingWoods =
  (treachery "the-voice-of-isengard-nightmare-into-fangorn-008" "Stifling Woods" IntoFangornNightmare 2)
    { cdShadowEffect = True
    }

fangornsAnger :: CardDef
fangornsAnger =
  (treachery "the-voice-of-isengard-nightmare-into-fangorn-009" "Fangorn's Anger" IntoFangornNightmare 2)
    { cdShadowEffect = True
    }

-- The Grey Havens Nightmare Decks

rogueWave :: CardDef
rogueWave =
  (treachery "the-grey-havens-nightmare-voyage-across-belegaer-008" "Rogue Wave" VoyageAcrossBelegaerNightmare 3)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    }

-- The Lost Realm Nightmare Decks

aGreatHost :: CardDef
aGreatHost = treachery "the-lost-realm-nightmare-invaders-in-chetwood-007" "A Great Host" IntrudersInChetwoodNightmare 2

killThem :: CardDef
killThem = treachery "the-lost-realm-nightmare-invaders-in-chetwood-008" "Kill Them!" IntrudersInChetwoodNightmare 3

rainWashedTracks :: CardDef
rainWashedTracks =
  (treachery "the-lost-realm-nightmare-the-weather-hills-017" "Rain-washed Tracks" TheWeatherHillsNightmare 3)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    }

-- The Wastes of Eriador Nightmare

cunningWolves :: CardDef
cunningWolves = (treachery "the-wastes-of-eriador-nightmare-the-wastes-of-eriador-005" "Cunning Wolves" TheWastesOfEriadorNightmare 3) {cdShadowEffect = True}

torrentOfHail :: CardDef
torrentOfHail =
  (treachery "the-wastes-of-eriador-nightmare-the-wastes-of-eriador-006" "Torrent of Hail" TheWastesOfEriadorNightmare 3)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    }

dreamsOfNumenor :: CardDef
dreamsOfNumenor =
  (treachery "the-grey-havens-nightmare-voyage-across-belegaer-009" "Dreams of Númenor" VoyageAcrossBelegaerNightmare 2)
    { cdSailingIcons = 1
    , cdShadowEffect = True
    }

ragingSquallNightmare :: CardDef
ragingSquallNightmare =
  (treachery "the-grey-havens-nightmare-voyage-across-belegaer-010" "Raging Squall" VoyageAcrossBelegaerNightmare 2)
    { cdCardTraits = setFromList [Weather]
    , cdKeywords = setFromList [Surge]
    , cdSailingIcons = 1
    , cdWhenRevealed = False
    }

corruptedFlora :: CardDef
corruptedFlora =
  (treachery "the-grey-havens-nightmare-the-fate-of-numenor-011" "Corrupted Flora" TheFateOfNumenorNightmare 3) {cdCardTraits = setFromList [Hazard]}

calphonsFolly :: CardDef
calphonsFolly =
  (treachery "the-grey-havens-nightmare-the-fate-of-numenor-012" "Calphon's Folly" TheFateOfNumenorNightmare 2) {cdShadowEffect = True}

suddenAssaultNightmare :: CardDef
suddenAssaultNightmare =
  (treachery "the-grey-havens-nightmare-raid-on-the-grey-havens-010" "Sudden Assault" RaidOnTheGreyHavensNightmare 3) {cdShadowEffect = True}

infiltration :: CardDef
infiltration =
  (treachery "the-grey-havens-nightmare-raid-on-the-grey-havens-011" "Infiltration" RaidOnTheGreyHavensNightmare 2) {cdShadowEffect = True}

paralyzingFear :: CardDef
paralyzingFear =
  (treachery "the-lost-realm-nightmare-deadmens-dike-025" "Paralyzing Fear" DeadmensDikeNightmare 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdKeywords = setFromList [Surge]
    }

-- Escape from Mount Gram Nightmare

cruelTorment :: CardDef
cruelTorment = (treachery "escape-from-mount-gram-nightmare-escape-from-mount-gram-006" "Cruel Torment" EscapeFromMountGramNightmare 3) {cdKeywords = setFromList [Surge]}

goblinPatrolNightmare :: CardDef
goblinPatrolNightmare = (treachery "escape-from-mount-gram-nightmare-escape-from-mount-gram-007" "Goblin Patrol" EscapeFromMountGramNightmare 2) {cdShadowEffect = True}

-- Across the Ettenmoors Nightmare

crashingThunder :: CardDef
crashingThunder =
  (treachery "across-the-ettenmoors-nightmare-across-the-ettenmoors-008" "Crashing Thunder" AcrossTheEttenmoorsNightmare 3)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    }

barrenAndDesolate :: CardDef
barrenAndDesolate = (treachery "across-the-ettenmoors-nightmare-across-the-ettenmoors-009" "Barren and Desolate" AcrossTheEttenmoorsNightmare 3) {cdKeywords = setFromList [Surge]}

-- The Treachery of Rhudaur Nightmare

malevolentForces :: CardDef
malevolentForces = (treachery "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-006" "Malevolent Forces" TheTreacheryOfRhudaurNightmare 2) {cdShadowEffect = True}

-- | Of the Decipher Ancient Texts encounter set, as its icon shows.
lifeDrain :: CardDef
lifeDrain =
  (treachery "the-treachery-of-rhudaur-nightmare-the-treachery-of-rhudaur-009" "Life Drain" TheTreacheryOfRhudaurNightmare 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdShadowEffect = True
    }

-- The Battle of Carn Dûm Nightmare

deathAndDecay :: CardDef
deathAndDecay = (treachery "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-006" "Death and Decay" TheBattleOfCarnDumNightmare 3) {cdCardTraits = setFromList [Sorcery]}

howlingOfWargs :: CardDef
howlingOfWargs = (treachery "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-007" "Howling of Wargs" TheBattleOfCarnDumNightmare 2) {cdShadowEffect = True}

blackMare :: CardDef
blackMare =
  unique
    (treachery "the-battle-of-carn-dum-nightmare-the-battle-of-carn-dum-008" "Black Mare" TheBattleOfCarnDumNightmare 1)
      { cdCardTraits = setFromList [Mount]
      , cdShadowEffect = True
      }

-- The Dread Realm Nightmare

powerBeyondDeath :: CardDef
powerBeyondDeath =
  (treachery "the-dread-realm-nightmare-the-dread-realm-006" "Power Beyond Death" TheDreadRealmNightmare 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

daechanarsCurse :: CardDef
daechanarsCurse =
  (treachery "the-dread-realm-nightmare-the-dread-realm-007" "Daechanar's Curse" TheDreadRealmNightmare 3)
    { cdCardTraits = setFromList [Sorcery]
    , cdKeywords = setFromList [Surge]
    }

curseOfAngmar :: CardDef
curseOfAngmar =
  (treachery "the-lost-realm-nightmare-deadmens-dike-026" "Curse of Angmar" DeadmensDikeNightmare 2)
    { cdCardTraits = setFromList [Sorcery]
    , cdKeywords = setFromList [Doomed 1]
    }

-- Khazad-dûm Nightmare Decks

shadowFall :: CardDef
shadowFall =
  (treachery "khazad-dum-nightmare-into-the-pit-008" "Shadow-fall" IntoThePitNightmare 3)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

balinsDoom :: CardDef
balinsDoom =
  (treachery "khazad-dum-nightmare-the-seventh-level-009" "Balin's Doom" TheSeventhLevelNightmare 3)
    { cdKeywords = setFromList [Surge, Doomed 1]
    , cdShadowEffect = True
    }

beatsAndEchoes :: CardDef
beatsAndEchoes = treachery "khazad-dum-nightmare-the-seventh-level-010" "Beats and Echoes" TheSeventhLevelNightmare 2

dismayAndFear :: CardDef
dismayAndFear = (treachery "khazad-dum-nightmare-flight-from-moria-008" "Dismay and Fear" FlightFromMoriaNightmare 3) {cdShadowEffect = True}

anEvilFortune :: CardDef
anEvilFortune =
  (treachery "khazad-dum-nightmare-flight-from-moria-009" "An Evil Fortune" FlightFromMoriaNightmare 3)
    { cdCardTraits = setFromList [Shadow, Flame]
    , cdKeywords = setFromList [Surge, Doomed 1]
    , cdShadowEffect = True
    , cdVictoryPoints = Just 1
    }

-- The Ring-maker Nightmare Decks

boarClanAssault :: CardDef
boarClanAssault =
  (treachery "the-dunland-trap-nightmare-the-dunland-trap-007" "Boar Clan Assault" TheDunlandTrapNightmare 2)
    { cdShadowEffect = True
    }

stalkedByDunlendings :: CardDef
stalkedByDunlendings =
  (treachery "the-dunland-trap-nightmare-the-dunland-trap-008" "Stalked by Dunlendings" TheDunlandTrapNightmare 2)
    { cdShadowEffect = True
    }

cursedBurden :: CardDef
cursedBurden =
  (treachery "the-three-trials-nightmare-the-three-trials-007" "Cursed Burden" TheThreeTrialsNightmare 3)
    { cdKeywords = setFromList [Surge]
    }

trialOfAncestors :: CardDef
trialOfAncestors =
  (treachery "the-three-trials-nightmare-the-three-trials-008" "Trial of Ancestors" TheThreeTrialsNightmare 2)

waylaidByAgents :: CardDef
waylaidByAgents =
  (treachery "trouble-in-tharbad-nightmare-trouble-in-tharbad-007" "Waylaid by Agents" TroubleInTharbadNightmare 2)
    { cdShadowEffect = True
    }

tharbadPatrol :: CardDef
tharbadPatrol =
  (treachery "trouble-in-tharbad-nightmare-trouble-in-tharbad-008" "Tharbad Patrol" TroubleInTharbadNightmare 3)
    { cdKeywords = setFromList [Surge]
    }

quicksand :: CardDef
quicksand =
  (treachery "the-nin-in-eilph-nightmare-the-nin-in-eilph-006" "Quicksand" TheNinInEilphNightmare 2)
    { cdShadowEffect = True
    }

rottenStench :: CardDef
rottenStench =
  (treachery "the-nin-in-eilph-nightmare-the-nin-in-eilph-007" "Rotten Stench" TheNinInEilphNightmare 2)
    { cdShadowEffect = True
    }

hatefulSearch :: CardDef
hatefulSearch =
  (treachery "celebrimbors-secret-nightmare-celebrimbors-secret-006" "Hateful Search" CelebrimborsSecretNightmare 3)
    { cdShadowEffect = True
    }

sauronsAgent :: CardDef
sauronsAgent =
  (treachery "celebrimbors-secret-nightmare-celebrimbors-secret-007" "Sauron's Agent" CelebrimborsSecretNightmare 2)
    { cdShadowEffect = True
    }

closingIn :: CardDef
closingIn =
  (treachery "celebrimbors-secret-nightmare-celebrimbors-secret-008" "Closing In" CelebrimborsSecretNightmare 2)
    { cdShadowEffect = True
    , cdKeywords = setFromList [Surge]
    }

ravenTalisman :: CardDef
ravenTalisman =
  (treachery "the-antlered-crown-nightmare-the-antlered-crown-007" "Raven Talisman" TheAntleredCrownNightmare 2)
    { cdShadowEffect = True
    }

gatheringForces :: CardDef
gatheringForces =
  (treachery "the-antlered-crown-nightmare-the-antlered-crown-008" "Gathering Forces" TheAntleredCrownNightmare 2)
    { cdShadowEffect = True
    }

-- Shadows of Mirkwood Nightmare Decks

theEnemysTrail :: CardDef
theEnemysTrail =
  (treachery "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-007" "The Enemy's Trail" TheHuntForGollumNightmare 2)
    { cdShadowEffect = True
    }

restlessHunters :: CardDef
restlessHunters =
  (treachery "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-008" "Restless Hunters" TheHuntForGollumNightmare 3)
    { cdShadowEffect = True
    , cdKeywords = setFromList [Surge]
    }

theTrailGoesCold :: CardDef
theTrailGoesCold = treachery "the-hunt-for-gollum-nightmare-the-hunt-for-gollum-009" "The Trail Goes Cold" TheHuntForGollumNightmare 2

timeForSupper :: CardDef
timeForSupper = treachery "conflict-at-the-carrock-nightmare-conflict-at-the-carrock-010" "Time for Supper" ConflictAtTheCarrockNightmare 3

shotFromTheSky :: CardDef
shotFromTheSky =
  (treachery "a-journey-to-rhosgobel-nightmare-a-journey-to-rhosgobel-007" "Shot from the Sky" AJourneyToRhosgobelNightmare 3)
    { cdShadowEffect = True
    , cdKeywords = setFromList [Surge]
    }

noxiousSupplies :: CardDef
noxiousSupplies =
  (treachery "a-journey-to-rhosgobel-nightmare-a-journey-to-rhosgobel-008" "Noxious Supplies" AJourneyToRhosgobelNightmare 3)
    { cdKeywords = setFromList [Surge]
    }

aHurryingDarkness :: CardDef
aHurryingDarkness =
  (treachery "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-006" "A Hurrying Darkness" TheHillsOfEmynMuilNightmare 2)
    { cdCardTraits = setFromList [Hazard]
    , cdShadowEffect = True
    }

blastOfSavageWind :: CardDef
blastOfSavageWind =
  (treachery "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-007" "Blast of Savage Wind" TheHillsOfEmynMuilNightmare 3)
    { cdCardTraits = setFromList [Hazard]
    , cdShadowEffect = True
    , cdKeywords = setFromList [Doomed 2]
    }

hazardousTerrain :: CardDef
hazardousTerrain =
  (treachery "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-008" "Hazardous Terrain" TheHillsOfEmynMuilNightmare 2)
    { cdShadowEffect = True
    , cdKeywords = setFromList [Surge, Doomed 2]
    }

retracedSteps :: CardDef
retracedSteps =
  (treachery "the-hills-of-emyn-muil-nightmare-the-hills-of-emyn-muil-009" "Retraced Steps" TheHillsOfEmynMuilNightmare 2)
    { cdKeywords = setFromList [Surge]
    }

candlesOfCorpses :: CardDef
candlesOfCorpses =
  (treachery "the-dead-marshes-nightmare-the-dead-marshes-008" "Candles of Corpses" TheDeadMarshesNightmare 1)
    { cdCardTraits = setFromList [Escape]
    , cdEscape = Just 5
    }

swarmingMosquitoes :: CardDef
swarmingMosquitoes =
  (treachery "the-dead-marshes-nightmare-the-dead-marshes-009" "Swarming Mosquitoes" TheDeadMarshesNightmare 2)
    { cdCardTraits = setFromList [Escape]
    , cdEscape = Just 3
    }

devilryOfTheDarkLand :: CardDef
devilryOfTheDarkLand =
  (treachery "the-dead-marshes-nightmare-the-dead-marshes-010" "Devilry of the Dark Land" TheDeadMarshesNightmare 1)
    { cdCardTraits = setFromList [Corruption]
    , cdEscape = Just 10
    }

isolation :: CardDef
isolation =
  (treachery "return-to-mirkwood-nightmare-return-to-mirkwood-007" "Isolation" ReturnToMirkwoodNightmare 2)
    { cdKeywords = setFromList [Surge]
    }

befouledEquipment :: CardDef
befouledEquipment =
  (treachery "return-to-mirkwood-nightmare-return-to-mirkwood-008" "Befouled Equipment" ReturnToMirkwoodNightmare 2)
    { cdCardTraits = setFromList [Tantrum]
    , cdShadowEffect = True
    }

wickednessAndMischief :: CardDef
wickednessAndMischief =
  (treachery "return-to-mirkwood-nightmare-return-to-mirkwood-009" "Wickedness and Mischief" ReturnToMirkwoodNightmare 2)
    { cdCardTraits = setFromList [Tantrum]
    , cdShadowEffect = True
    }

-- The Morgul Vale Nightmare

theTowerBeckons :: CardDef
theTowerBeckons = (treachery "the-morgul-vale-nightmare-the-morgul-vale-008" "The Tower Beckons" TheMorgulValeNightmare 2) {cdShadowEffect = True}

noxiousFumes :: CardDef
noxiousFumes = (treachery "the-morgul-vale-nightmare-the-morgul-vale-009" "Noxious Fumes" TheMorgulValeNightmare 2) {cdShadowEffect = True}

-- The Blood of Gondor Nightmare

surroundedByShadow :: CardDef
surroundedByShadow = (treachery "the-blood-of-gondor-nightmare-the-blood-of-gondor-007" "Surrounded by Shadow" TheBloodOfGondorNightmare 3) {cdKeywords = setFromList [Doomed 2]}

suddenOnslaught :: CardDef
suddenOnslaught = (treachery "the-blood-of-gondor-nightmare-the-blood-of-gondor-008" "Sudden Onslaught" TheBloodOfGondorNightmare 2) {cdShadowEffect = True}

-- The Drúadan Forest Nightmare

drumsOfTheWildMen :: CardDef
drumsOfTheWildMen = treachery "the-druadan-forest-nightmare-the-druadan-forest-008" "Drums of the Wild Men" TheDruadanForestNightmare 2

poisonDarts :: CardDef
poisonDarts =
  (treachery "the-druadan-forest-nightmare-the-druadan-forest-009" "Poison-darts" TheDruadanForestNightmare 3)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Surge]
    }

-- Encounter at Amon Dîn Nightmare

trappedVillagers :: CardDef
trappedVillagers = (treachery "encounter-at-amon-din-nightmare-encounter-at-amon-din-006" "Trapped Villagers" EncounterAtAmonDinNightmare 3) {cdShadowEffect = True}

runForYourLives :: CardDef
runForYourLives = (treachery "encounter-at-amon-din-nightmare-encounter-at-amon-din-007" "Run for your Lives!" EncounterAtAmonDinNightmare 2) {cdShadowEffect = True}

saveUs :: CardDef
saveUs = (treachery "encounter-at-amon-din-nightmare-encounter-at-amon-din-008" "Save Us!" EncounterAtAmonDinNightmare 2) {cdShadowEffect = True}

thoseNowSundered :: CardDef
thoseNowSundered =
  (treachery "encounter-at-amon-din-nightmare-encounter-at-amon-din-009" "Those Now Sundered" EncounterAtAmonDinNightmare 2)
    { cdCardTraits = setFromList [Panic]
    , cdKeywords = setFromList [Surge, Doomed 1]
    }

-- Assault on Osgiliath Nightmare

relentlessAttack :: CardDef
relentlessAttack = (treachery "assault-on-osgiliath-nightmare-assault-on-osgiliath-008" "Relentless Attack" AssaultOnOsgiliathNightmare 2) {cdKeywords = setFromList [Doomed 1]}

overrunDefenses :: CardDef
overrunDefenses = (treachery "assault-on-osgiliath-nightmare-assault-on-osgiliath-009" "Overrun Defenses" AssaultOnOsgiliathNightmare 2) {cdShadowEffect = True}

pressedOnAllSides :: CardDef
pressedOnAllSides = (treachery "assault-on-osgiliath-nightmare-assault-on-osgiliath-010" "Pressed on All Sides" AssaultOnOsgiliathNightmare 2) {cdShadowEffect = True}

-- The Steward's Fear Nightmare

falseAccusations :: CardDef
falseAccusations =
  (treachery "the-stewards-fear-nightmare-the-stewards-fear-009" "False Accusations" TheStewardsFearNightmare 2)
    { cdCardTraits = setFromList [Scheme]
    , cdShadowEffect = True
    }

blindingBlizzard :: CardDef
blindingBlizzard =
  (treachery "the-redhorn-gate-nightmare-the-redhorn-gate-007" "Blinding Blizzard" TheRedhornGateNightmare 2)
    { cdCardTraits = setFromList [Snow]
    }

caradhrasTheCruel :: CardDef
caradhrasTheCruel = treachery "the-redhorn-gate-nightmare-the-redhorn-gate-008" "Caradhras the Cruel" TheRedhornGateNightmare 2

tumultuousAssault :: CardDef
tumultuousAssault = treachery "road-to-rivendell-nightmare-road-to-rivendell-009" "Tumultuous Assault" RoadToRivendellNightmare 2

pulledUnder :: CardDef
pulledUnder =
  (treachery "the-watcher-in-the-water-nightmare-the-watcher-in-the-water-007" "Pulled Under" TheWatcherInTheWaterNightmare 2)
    { cdShadowEffect = True
    }

ripplingFromBeneath :: CardDef
ripplingFromBeneath =
  (treachery "the-watcher-in-the-water-nightmare-the-watcher-in-the-water-008" "Rippling From Beneath" TheWatcherInTheWaterNightmare 3)
    { cdKeywords = setFromList [Surge]
    }

dreadfulDeep :: CardDef
dreadfulDeep =
  (treachery "the-long-dark-nightmare-the-long-dark-007" "Dreadful Deep" TheLongDarkNightmare 3)
    { cdShadowEffect = True
    }

lostAndHelpless :: CardDef
lostAndHelpless =
  (treachery "the-long-dark-nightmare-the-long-dark-008" "Lost and Helpless" TheLongDarkNightmare 2)
    { cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

chillOfTheRoots :: CardDef
chillOfTheRoots =
  (treachery "foundations-of-stone-nightmare-foundations-of-stone-008" "Chill of the Roots" FoundationsOfStoneNightmare 3)
    { cdShadowEffect = True
    }

-- | The Attack cards make up Shadow and Flame's Balrog deck in nightmare mode: shadow
-- effects only.
terribleStrength :: CardDef
terribleStrength =
  (treachery "shadow-and-flame-nightmare-shadow-and-flame-002" "Terrible Strength" ShadowAndFlameNightmare 2)
    { cdCardTraits = setFromList [Attack]
    , cdWhenRevealed = False
    , cdShadowEffect = True
    }

theDarkFire :: CardDef
theDarkFire =
  (treachery "shadow-and-flame-nightmare-shadow-and-flame-003" "The Dark Fire" ShadowAndFlameNightmare 2)
    { cdCardTraits = setFromList [Attack]
    , cdWhenRevealed = False
    , cdShadowEffect = True
    }

blazingGrip :: CardDef
blazingGrip =
  (treachery "shadow-and-flame-nightmare-shadow-and-flame-004" "Blazing Grip" ShadowAndFlameNightmare 2)
    { cdCardTraits = setFromList [Attack]
    , cdWhenRevealed = False
    , cdShadowEffect = True
    , cdShadowCannotBeCanceled = True
    }

morgothsBidding :: CardDef
morgothsBidding =
  (treachery "shadow-and-flame-nightmare-shadow-and-flame-005" "Morgoth's Bidding" ShadowAndFlameNightmare 2)
    { cdCardTraits = setFromList [Attack]
    , cdWhenRevealed = False
    , cdShadowEffect = True
    }

flameOfUdun :: CardDef
flameOfUdun =
  (treachery "shadow-and-flame-nightmare-shadow-and-flame-006" "Flame of Udûn" ShadowAndFlameNightmare 2)
    { cdCardTraits = setFromList [Attack]
    , cdWhenRevealed = False
    , cdShadowEffect = True
    }

allIsLost :: CardDef
allIsLost =
  (treachery "the-dread-realm-nightmare-the-dread-realm-008" "All is Lost" TheDreadRealmNightmare 2)
    { cdCardTraits = setFromList [Sorcery]
    , cdShadowEffect = True
    }

-- * The Sands of Harad: the shared encounter sets (Harad Soldiers, Harad Territory, Desert
-- Sands, Jungle Forest, Mordor Orcs). Easy mode copy counts from Hall of Beorn's scenario
-- pages.

nowhereToHide :: CardDef
nowhereToHide = (treachery "16044" "Nowhere to Hide" HaradSoldiers 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

injuredCompanion :: CardDef
injuredCompanion = (treachery "16046" "Injured Companion" HaradTerritory 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

enfeebled :: CardDef
enfeebled = (treachery "16047" "Enfeebled" HaradTerritory 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

sandstorm :: CardDef
sandstorm =
  (treachery "16051" "Sandstorm" DesertSands 2)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

heatExhaustion :: CardDef
heatExhaustion = (treachery "16052" "Heat Exhaustion" DesertSands 2) {cdShadowEffect = True}

overgrown :: CardDef
overgrown = (treachery "16059" "Overgrown" JungleForest 2) {cdKeywords = setFromList [Surge], cdEasyModeQuantity = Just 1}

fearOfMordor :: CardDef
fearOfMordor = (treachery "16065" "Fear of Mordor" MordorOrcs 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

savageAttack :: CardDef
savageAttack = (treachery "16038" "Savage Attack" TheLongArmOfMordor 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

overrunByOrcs :: CardDef
overrunByOrcs = (treachery "16039" "Overrun by Orcs" TheLongArmOfMordor 3) {cdShadowEffect = True}
-- * Beneath the Sands

forcedOffTrackBeneathTheSands :: CardDef
forcedOffTrackBeneathTheSands = (treachery "17073" "Forced Off Track" BeneathTheSands 3) {cdShadowEffect = True}

grabbedBySpiders :: CardDef
grabbedBySpiders = (treachery "17074" "Grabbed by Spiders" BeneathTheSands 3) {cdKeywords = setFromList [Surge, Doomed 1]}

-- * Desert Crossing (The Sands of Harad). Easy mode copy counts from Hall of Beorn's
-- scenario page.

mirage :: CardDef
mirage = (treachery "16028" "Mirage" DesertCrossing 2) {cdCardTraits = setFromList [Weather]}

unbearableHeat :: CardDef
unbearableHeat = (treachery "16029" "Unbearable Heat" DesertCrossing 2) {cdCardTraits = setFromList [Weather], cdShadowEffect = True}

scorchingSun :: CardDef
scorchingSun =
  (treachery "16030" "Scorching Sun" DesertCrossing 2)
    { cdCardTraits = setFromList [Weather]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- * Escape from Umbar

enemyPursuit :: CardDef
enemyPursuit = (treachery "16021" "Enemy Pursuit" EscapeFromUmbar 2) {cdShadowEffect = True}

-- * Race Across Harad

pursuersFromMordor :: CardDef
pursuersFromMordor = (treachery "17045" "Pursuers from Mordor" RaceAcrossHarad 2) {cdKeywords = setFromList [Doomed 2], cdShadowEffect = True}

howlingAtTheirHeels :: CardDef
howlingAtTheirHeels = (treachery "17046" "Howling at their Heels" RaceAcrossHarad 3) {cdShadowEffect = True}

-- * The Black Serpent. Easy mode copy counts from Hall of Beorn's scenario page.

boldAndGrim :: CardDef
boldAndGrim =
  (treachery "17101" "Bold and Grim" TheBlackSerpent 3)
    { cdKeywords = setFromList [Surge, Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

noQuarter :: CardDef
noQuarter = (treachery "17102" "No Quarter" TheBlackSerpent 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

chaosInTheNight :: CardDef
chaosInTheNight = (treachery "17103" "Chaos in the Night" TheBlackSerpent 2) {cdShadowEffect = True}

-- The Dungeons of Cirith Gurat (17129-17130)

takenPrisoner :: CardDef
takenPrisoner = (treachery "17129" "Taken Prisoner" TheDungeonsOfCirithGurat 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

tormentedByOrcs :: CardDef
tormentedByOrcs = (treachery "17130" "Tormented by Orcs" TheDungeonsOfCirithGurat 3) {cdShadowEffect = True}

-- The Mûmakil

stampedingOliphaunt :: CardDef
stampedingOliphaunt = (treachery "17021" "Stampeding Oliphaunt" TheMumakil 2) {cdKeywords = setFromList [Doomed 1]}

theSavageSouth :: CardDef
theSavageSouth = (treachery "17022" "The Savage South" TheMumakil 2) {cdShadowEffect = True}

-- | Counts as a Condition attachment.
terribleFever :: CardDef
terribleFever =
  (treachery "17023" "Terrible Fever" TheMumakil 2)
    { cdCardTraits = setFromList [Condition]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

bitingInsects :: CardDef
bitingInsects = (treachery "17024" "Biting Insects" TheMumakil 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

-- * The Crossings of Poros: its two encounter sets, The Crossings of Poros and Mountains of
-- Shadow. Easy mode copy counts from Hall of Beorn's scenario page.

dangerCloseBehind :: CardDef
dangerCloseBehind =
  (treachery "17152" "Danger Close Behind" TheCrossingsOfPoros 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

mountainsOfShadow :: CardDef
mountainsOfShadow = (treachery "17155" "Mountains of Shadow" MountainsOfShadow 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

-- * The Wilds of Rhovanion's shared encounter sets

ruinedSupplies :: CardDef
ruinedSupplies =
  (treachery "18025" "Ruined Supplies" LostInWilderland 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- * Journey Up the Anduin

frenziedCreature :: CardDef
frenziedCreature = (treachery "18018" "Frenzied Creature" JourneyUpTheAnduin 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

dangerousCrossing :: CardDef
dangerousCrossing = (treachery "18019" "Dangerous Crossing" JourneyUpTheAnduin 3) {cdShadowEffect = True}

lostInTheWild :: CardDef
lostInTheWild = (treachery "18026" "Lost in the Wild" LostInWilderland 2) {cdEasyModeQuantity = Just 1}

weighedDown :: CardDef
weighedDown = (treachery "18027" "Weighed Down" LostInWilderland 2) {cdEasyModeQuantity = Just 1}

gatheringGloom :: CardDef
gatheringGloom =
  (treachery "18040" "Gathering Gloom" GatheringGloom 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- | Printed "Swarm of Bats", as its own text names it.
swarmOfBats :: CardDef
swarmOfBats = (treachery "18041" "Swarm of Bats" GatheringGloom 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

afraidOfTheDark :: CardDef
afraidOfTheDark = (treachery "18048" "Afraid of the Dark" AfraidOfTheDark 2) {cdEasyModeQuantity = Just 1}

darkPlaces :: CardDef
darkPlaces = (treachery "18056" "Dark Places" DeepUnderground 3) {cdShadowEffect = True}

eyesInTheDark :: CardDef
eyesInTheDark = (treachery "18057" "Eyes in the Dark" AfraidOfTheDark 3) {cdKeywords = setFromList [Doomed 1], cdShadowEffect = True, cdEasyModeQuantity = Just 2}

dragonBreath :: CardDef
dragonBreath = (treachery "18058" "Dragon Breath" DragonMight 2) {cdCardTraits = setFromList [Dragon], cdKeywords = setFromList [Surge], cdEasyModeQuantity = Just 1}

dragonScales :: CardDef
dragonScales = (treachery "18059" "Dragon Scales" DragonMight 2) {cdCardTraits = setFromList [Dragon], cdKeywords = setFromList [Surge], cdEasyModeQuantity = Just 1}

-- * The Fate of Wilderland

fierceAttack :: CardDef
fierceAttack =
  (treachery "19165" "Fierce Attack" TheFateOfWilderland 3)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    }

urdugsCommand :: CardDef
urdugsCommand = (treachery "19166" "Urdug's Command" TheFateOfWilderland 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

darkClouds :: CardDef
darkClouds = (treachery "19167" "Dark Clouds" TheFateOfWilderland 2) {cdShadowEffect = True}
-- * Fire in the Night

powerfulInWrath :: CardDef
powerfulInWrath =
  (treachery "19080" "Powerful in Wrath" FireInTheNight 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    }

-- * The Ghost of Framsburg (19094-19111)

abandonedRuins :: CardDef
abandonedRuins = (treachery "19106" "Abandoned Ruins" TheGhostOfFramsburg 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

terribleShadows :: CardDef
terribleShadows =
  (treachery "19107" "Terrible Shadows" TheGhostOfFramsburg 2)
    { cdCardTraits = setFromList [Hazard]
    , cdShadowEffect = True
    }

theDragonsFury :: CardDef
theDragonsFury = (treachery "19081" "The Dragon's Fury" FireInTheNight 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

brightFlames :: CardDef
brightFlames = (treachery "19082" "Bright Flames" FireInTheNight 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}
denizenOfTheDeep :: CardDef
denizenOfTheDeep = (treachery "18047" "Denizen of the Deep" TheKingsQuest 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}
slammedShut :: CardDef
slammedShut =
  (treachery "19108" "Slammed Shut" TheGhostOfFramsburg 3)
    { cdCardTraits = setFromList [Hazard]
    , cdKeywords = setFromList [Doomed 1]
    }
-- * Mount Gundabad

wilderlandsBane :: CardDef
wilderlandsBane = (treachery "19132" "Wilderland's Bane" MountGundabad 2) {cdShadowEffect = True}

suffocatingShadows :: CardDef
suffocatingShadows = (treachery "19133" "Suffocating Shadows" MountGundabad 2) {cdShadowEffect = True}
-- * The Withered Heath

heavySnow :: CardDef
heavySnow =
  (treachery "19020" "Heavy Snow" TheWitheredHeath 2)
    { cdCardTraits = setFromList [Weather]
    , cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

deadlyCold :: CardDef
deadlyCold = (treachery "19021" "Deadly Cold" TheWitheredHeath 3) {cdCardTraits = setFromList [Weather], cdShadowEffect = True, cdEasyModeQuantity = Just 2}
-- * Roam Across Rhovanion

sneakingOff :: CardDef
sneakingOff = (treachery "19044" "Sneaking Off" RoamAcrossRhovanion 3) {cdEasyModeQuantity = Just 1}

harshWeather :: CardDef
harshWeather =
  (treachery "19045" "Harsh Weather" RoamAcrossRhovanion 3)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    }

-- | The treachery, not the Wild Creatures encounter set.
wildCreatures :: CardDef
wildCreatures = (treachery "19046" "Wild Creatures" RoamAcrossRhovanion 2)
unseenDanger :: CardDef
unseenDanger = (treachery "18036" "Unseen Danger" LostInMirkwood 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

vastnessOfMirkwood :: CardDef
vastnessOfMirkwood = (treachery "18037" "Vastness of Mirkwood" LostInMirkwood 3)

-- * A Shadow in the East

-- | Shuffled into the encounter deck by Sméagol's Setup, from outside any player's deck.
stinker :: CardDef
stinker =
  (treachery "21071" "Stinker" GollumAndSmeagol 2)
    { cdCardTraits = setFromList [Gollum]
    , cdKeywords = setFromList [Surge]
    , cdShadowEffect = True
    }

-- * A Shadow in the East's shared encounter sets

easterlingHorse :: CardDef
easterlingHorse = (treachery "21019" "Easterling Horse" RidersOfRhun 3) {cdCardTraits = setFromList [Easterling]}

swordOfRhun :: CardDef
swordOfRhun =
  (treachery "21022" "Sword of Rhûn" EasterlingRaiders 2)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

recurveBow :: CardDef
recurveBow =
  (treachery "21023" "Recurve Bow" EasterlingRaiders 2)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

wearyLands :: CardDef
wearyLands =
  (treachery "21027" "Weary Lands" RollingPlains 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

painfulFatigue :: CardDef
painfulFatigue = (treachery "21028" "Painful Fatigue" RollingPlains 2) {cdShadowEffect = True}

overwhelmed :: CardDef
overwhelmed =
  (treachery "21042" "Overwhelmed" ServantsOfSauron 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

darknessInTheEast :: CardDef
darknessInTheEast = (treachery "21048" "Darkness in the East" CityOfRhun 2) {cdShadowEffect = True}

underGuard :: CardDef
underGuard = (treachery "21051" "Under Guard" UnderGuard 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

ulchorsRage :: CardDef
ulchorsRage = (treachery "21059" "Ulchor's Rage" UlchorsGuard 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

fanaticism :: CardDef
fanaticism = (treachery "21060" "Fanaticism" UlchorsGuard 2) {cdCardTraits = setFromList [Easterling]}

fearfulShadows :: CardDef
fearfulShadows =
  (treachery "21064" "Fearful Shadows" TwistedTunnels 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

-- * The River Running

enemiesCloseBehind :: CardDef
enemiesCloseBehind = (treachery "21016" "Enemies Close Behind" TheRiverRunning 2) {cdShadowEffect = True}
placeOfEvil :: CardDef
placeOfEvil = (treachery "21057" "Place of Evil" TheTempleOfDoom 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}
-- * The City of Ulfast

extraVigil :: CardDef
extraVigil = (treachery "22045" "Extra Vigil" TheCityOfUlfast 2) {cdShadowEffect = True}

weveBeenSeen :: CardDef
weveBeenSeen = (treachery "22046" "We've Been Seen" TheCityOfUlfast 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

patrollingTheStreets :: CardDef
patrollingTheStreets = (treachery "22047" "Patrolling the Streets" TheCityOfUlfast 3) {cdShadowEffect = True}
-- * Under the Ash Mountains

ransackedSupplies :: CardDef
ransackedSupplies = (treachery "22103" "Ransacked Supplies" UnderTheAshMountains 3) {cdShadowEffect = True}

burningReek :: CardDef
burningReek =
  (treachery "22104" "Burning Reek" UnderTheAshMountains 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

fury :: CardDef
fury =
  (treachery "22019" "Fury" WrathAndRuin 2)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

-- * The Land of Sorrow

overseersWhip :: CardDef
overseersWhip =
  (treachery "22131" "Overseer's Whip" TheLandOfSorrow 2)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    }

caughtOffGuard :: CardDef
caughtOffGuard = (treachery "22020" "Caught Off Guard" WrathAndRuin 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

-- | Wrath and Ruin's, not Assault on Osgiliath's.
streetFightingWrathAndRuin :: CardDef
streetFightingWrathAndRuin = (treachery "22021" "Street Fighting" WrathAndRuin 2) {cdShadowEffect = True}

underSiege :: CardDef
underSiege = (treachery "22022" "Under Siege" WrathAndRuin 2) {cdShadowEffect = True}

ulchorsRevenge :: CardDef
ulchorsRevenge =
  (treachery "22023" "Ulchor's Revenge" WrathAndRuin 2)
    { cdKeywords = setFromList [Doomed 1]
    , cdShadowEffect = True
    , cdEasyModeQuantity = Just 1
    }

writhingShadows :: CardDef
writhingShadows = (treachery "22105" "Writhing Shadows" UnderTheAshMountains 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}
-- * Danger in Dorwinion

disappearances :: CardDef
disappearances = (treachery "21038" "Disappearances" DangerInDorwinion 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

secretCultist :: CardDef
secretCultist = (treachery "21039" "Secret Cultist" DangerInDorwinion 2) {cdEasyModeQuantity = Just 1}
-- * Challenge of the Wainriders

-- | Both cards are printed number 73: The Champion's Cunning keeps the code, as the card
-- images name them.
wainriderChariot :: CardDef
wainriderChariot =
  (treachery "22073-chariot" "Wainrider Chariot" ChallengeOfTheWainriders 2)
    { cdCardTraits = setFromList [Easterling]
    , cdKeywords = setFromList [Surge]
    , cdEasyModeQuantity = Just 1
    , cdRacingResult = Just Straight
    }

theChampionsCunning :: CardDef
theChampionsCunning =
  (treachery "22073" "The Champion's Cunning" ChallengeOfTheWainriders 3)
    { cdShadowEffect = True
    , cdEasyModeQuantity = Just 2
    , cdRacingResult = Just Wild
    }
-- * The Fortress of Nurn

determinedDefense :: CardDef
determinedDefense = (treachery "22156" "Determined Defense" TheFortressOfNurn 3) {cdShadowEffect = True}

theDarkLordsReach :: CardDef
theDarkLordsReach = (treachery "22157" "The Dark Lord's Reach" TheFortressOfNurn 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}
crueltyOfMordor :: CardDef
crueltyOfMordor = (treachery "22132" "Cruelty of Mordor" TheLandOfSorrow 3) {cdShadowEffect = True, cdEasyModeQuantity = Just 2}

brokenCaptives :: CardDef
brokenCaptives = (treachery "22133" "Broken Captives" TheLandOfSorrow 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}

-- * Flies and Spiders

caughtInWebs :: CardDef
caughtInWebs = treachery "132033" "Caught in Webs" FliesAndSpiders 2

poisonedBySpiders :: CardDef
poisonedBySpiders = (treachery "132034" "Poisoned by Spiders" FliesAndSpiders 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 0}

-- * Wilderland

fightingAmongFriends :: CardDef
fightingAmongFriends = (treachery "132037" "Fighting Among Friends" Wilderland 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 0}

weighedDownWilderland :: CardDef
weighedDownWilderland = (treachery "132038" "Weighed Down" Wilderland 2) {cdShadowEffect = True, cdEasyModeQuantity = Just 1}
