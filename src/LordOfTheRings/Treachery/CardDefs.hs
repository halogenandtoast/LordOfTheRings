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
      , deeperIntoTheRuins
      , furiousRainstorm
      , dwindlingSupplies
      , sunkenRuins
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
    }

windsOfWrath :: CardDef
windsOfWrath =
  (treachery "11056" "Winds of Wrath" StormyWeather 2)
    { cdCardTraits = setFromList [Weather]
    , cdShadowEffect = True
    , cdSailingIcons = 1
    }

boardingParty :: CardDef
boardingParty =
  (treachery "11059" "Boarding Party" UmbarNavy 2)
    { cdShadowEffect = True
    }

battleHardened :: CardDef
battleHardened =
  (treachery "11067" "Battle-Hardened" CorsairRaiders 2)
    { cdShadowEffect = True
    }

mysteriousFog :: CardDef
mysteriousFog = (treachery "11033" "Mysterious Fog" TheFateOfNumenor 2) {cdShadowEffect = True}

lingeringMalevolence :: CardDef
lingeringMalevolence = (treachery "11034" "Lingering Malevolence" TheFateOfNumenor 2) {cdShadowEffect = True}

ruinsOfAgesPast :: CardDef
ruinsOfAgesPast = (treachery "11035" "Ruins of Ages Past" TheFateOfNumenor 2) {cdKeywords = setFromList [Doomed 2]}

curseOfTheDownfallen :: CardDef
curseOfTheDownfallen = (treachery "11071" "Curse of the Downfallen" DrownedDead 2) {cdShadowEffect = True}

aimlessWandering :: CardDef
aimlessWandering = treachery "11075" "Aimless Wandering" RuinsOfNumenor 3

deeperIntoTheRuins :: CardDef
deeperIntoTheRuins = treachery "12078" "Deeper into the Ruins" TempleOfTheDeceived 4

furiousRainstorm :: CardDef
furiousRainstorm = (treachery "12079" "Furious Rainstorm" TempleOfTheDeceived 3) {cdShadowEffect = True}

dwindlingSupplies :: CardDef
dwindlingSupplies = (treachery "12080" "Dwindling Supplies" TempleOfTheDeceived 3) {cdShadowEffect = True}

sunkenRuins :: CardDef
sunkenRuins = (treachery "12081" "Sunken Ruins" TempleOfTheDeceived 3) {cdKeywords = setFromList [Doomed 1]}
