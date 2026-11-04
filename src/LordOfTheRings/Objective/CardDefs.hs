module LordOfTheRings.Objective.CardDefs (module LordOfTheRings.Objective.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Trait
import LordOfTheRings.Name (Name)

allObjectiveCards :: Map CardCode CardDef
allObjectiveCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ gandalfsMap
      , arnorRavaged
      , thaurdirsLegacy
      , thaurdirsMajesty
      , thaurdirsSpite
      , caveTorch
      , dungeonTorch
      , shadowKey
      , mendor
      , scarred
      , signsOfGollum
      , grimbeornTheOld
      , athelas
      , wilyador
      , gollumTheDeadMarshes
      , gollumReturnToMirkwood
      , abandonedTools
      , bookOfMazarbul
      , arwenUndomielTheRedhornGate
      , arwenUndomielRoadToRivendell
      , durinsGreaves
      , durinsAxe
      , durinsHelm
      , celador
      , ithilienGuardian
      , alcaronsScroll
      , rescuedVillagers
      , deadVillagers
      , lordAlcaron
      , toTheTower
      , aScrapOfHistory
      , secretMap
      , aPrisoner
      , unholyAlliance
      , upInFlames
      , poisonedCounsels
      , faramirObjective
      , lordAlcaronTheBloodOfGondor
      , mugashObjective
      , grimaTheFordsOfIsen
      , chiefTurchTheAntleredCrown
      , nalirTroubleInTharbad
      , theOrcsSearch
      , celebrimborsMould
      , nalirTheNinInEilph
      , keyOfTheBoar
      , keyOfTheWolf
      , keyOfTheRaven
      , daybreak
      , nightfall
      , amarthiul
      , iarion
      , huntingTheOrcs
      , savageCounterattack
      , captiveVillager
      , amarthiulAcrossTheEttenmoors
      , amarthiulTheTreacheryOfRhudaur
      , daechanarsBrand
      , heirloomOfIarchon
      , ordersFromAngmar
      , dreamChaser
      , dawnStar
      , narelenya
      , silverWing
      , calphon
      , captainSahirObjective
      , naasiyahObjective
      , theHavensBurn
      , gateKey
      , dolAmrothWarship
      , aearlang
      , docksOfDolAmroth
      , theCursedSword
      , corsairSkirmisherShip
      , seaWardTower
      , towerOfTheGull
      , theBeacon
      , towerOfTheHeron
      , blackKeyBurden
      , blackKeyBurdenUpgraded
      , theBlackSword
      , calaerion
      , calaerionUpgraded
      , gavin
      , gateKeyUpgraded
      , dreamChaserUpgraded
      , dawnStarUpgraded
      , narelenyaUpgraded
      , silverWingUpgraded
      , riseToPower
      , assassination
      , theCabalsChampion
      , kahliel
      , firyal
      , yazan
      , jubayr
      , offTrack
      , onTrack
      , seizeThem
      , tamedMumak
      , bannerOfTheSerpent
      , theSerpentsGarb
      , summonsFromMordor
      , theCaptivesOfCirithGurat
      , abaan
      , horseHairLasso
      , poisonedSpear
      , pitTrap
      , nooseOfVines
      , porosGarrison
      , haldan
      , theGoblinsAssault
      , theHeroesDefense
      , cryptKey
      , oldLantern
      , scathasTeeth
      , wormsbane
      , wormsbaneReforged
      , dragonSign
      , durinsKey
      , urdugsHorn
      , urdug
      , tiny
      , searchingForAWayOut
      , underTheWatchfulEye
      , theArmiesOfMordor
      , coveredInDarkness
      , easterlingPursuit
      , bribes
      , submitOrDie
      , sauronWorship
      , evidenceOfTheCult
      , theWainriders
      , theChallengers
      , edrahil
      , arador
      , farin
      , overcomeByFear
      , armyOfTheDeadBoon
      , theOneRingTheBlackRiders
      , theOneRingTheRoadDarkens
      , theOneRingTheLandOfShadow
      , theOneRingTheMountainOfFire
      , smeagolTheLandOfShadow
      , mistyMountainEagle
      , trollPurse
      , trollKey
      , aSmellySack
      , aStrongSack
      , aToughSack
      , aLargeSack
      , aDeepSack
      , aFoulSack
      , aWornSack
      , bilbosMagicRing
      , aBarePatch
      , gildorInglorionObjective
      , gandalfsDelay
      , anEvilWound
      , theCorsairFleet
      ]

durinsAxe :: CardDef
durinsAxe =
  unique
    (objective "04119" "Durin's Axe" FoundationsOfStone 1)
      { cdCardTraits = setFromList [Weapon, Artifact]
      , cdKeywords = setFromList [Surge, Restricted]
      }

durinsHelm :: CardDef
durinsHelm =
  unique
    (objective "04120" "Durin's Helm" FoundationsOfStone 1)
      { cdCardTraits = setFromList [Armor, Artifact]
      , cdKeywords = setFromList [Surge]
      }

caveTorch :: CardDef
caveTorch =
  (objective "03041" "Cave Torch" TwistsAndTurns 1)
    { cdCardTraits = setFromList [Light]
    , cdKeywords = setFromList [Restricted]
    }

gandalfsMap :: CardDef
gandalfsMap =
  (objective "01108" "Gandalf's Map" EscapeFromDolGuldur 1)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Guarded, Restricted]
    }

dungeonTorch :: CardDef
dungeonTorch =
  (objective "01109" "Dungeon Torch" EscapeFromDolGuldur 1)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Guarded, Restricted]
    }

shadowKey :: CardDef
shadowKey =
  (objective "01110" "Shadow Key" EscapeFromDolGuldur 1)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Guarded, Restricted]
    }

-- | A burden of the Revised Core campaign: Permanent, limit 1 per hero.
scarred :: CardDef
scarred =
  (objective "RC008" "Scarred" JourneyAlongTheAnduinCampaign 4)
    { cdCardTraits = setFromList [Burden]
    , cdLimitOnePerHost = True
    }

mendor :: CardDef
mendor =
  ((objective "RC002" "Mendor" PassageThroughMirkwoodCampaign 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 1, cdDefense = Just 1, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Scout, Silvan, Boon]
    , cdKeywords = setFromList [Ranged]
    }

signsOfGollum :: CardDef
signsOfGollum =
  (objective "02014" "Signs of Gollum" TheHuntForGollum 4)
    { cdCardTraits = setFromList [Clue]
    , cdKeywords = setFromList [Guarded]
    }

grimbeornTheOld :: CardDef
grimbeornTheOld =
  ((objective "02037" "Grimbeorn the Old" ConflictAtTheCarrock 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 4, cdDefense = Just 3, cdHitPoints = Just 10 })
    { cdUnique = True
    }

athelas :: CardDef
athelas =
  (objective "02063" "Athelas" AJourneyToRhosgobel 4)
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Guarded]
    }

wilyador :: CardDef
wilyador =
  ((objective "02064" "Wilyador" AJourneyToRhosgobel 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 1, cdDefense = Just 1, cdHitPoints = Just 20 })
    { cdUnique = True
    , cdCardTraits = setFromList [Creature, Eagle]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

gollumTheDeadMarshes :: CardDef
gollumTheDeadMarshes =
  (objective "02107" "Gollum" TheDeadMarshes 1)
    { cdUnique = True
    , cdCardTraits = setFromList [Gollum]
    }

gollumReturnToMirkwood :: CardDef
gollumReturnToMirkwood =
  ((objective "02130" "Gollum" ReturnToMirkwood 1) {cdHitPoints = Just 5})
    { cdUnique = True
    , cdCardTraits = setFromList [Creature]
    }

bookOfMazarbul :: CardDef
bookOfMazarbul =
  (objective "03024" "Book of Mazarbul" TheSeventhLevel 1)
    { cdCardTraits = setFromList [Item, Artifact]
    , cdKeywords = setFromList [Restricted]
    }

abandonedTools :: CardDef
abandonedTools =
  (objective "03029" "Abandoned Tools" FlightFromMoria 1)
    { cdCardTraits = setFromList [Tools]
    , cdKeywords = setFromList [Guarded, Restricted]
    }

-- | The Redhorn Gate's and Road to Rivendell's Arwen, the same card in each set.
arwenUndomiel :: CardCode -> EncounterSet -> CardDef
arwenUndomiel code set =
  ((objective code "Arwen Undómiel" set 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 0, cdDefense = Just 1, cdHitPoints = Just 2 })
    { cdUnique = True
    , cdCardTraits = setFromList [Noldor, Noble]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

arwenUndomielTheRedhornGate :: CardDef
arwenUndomielTheRedhornGate = arwenUndomiel "04014" TheRedhornGate

arwenUndomielRoadToRivendell :: CardDef
arwenUndomielRoadToRivendell = arwenUndomiel "04041" RoadToRivendell

durinsGreaves :: CardDef
durinsGreaves =
  unique
    $ (objective "04088" "Durin's Greaves" TheLongDark 1)
      { cdCardTraits = setFromList [Artifact, Armor]
      , cdWhenRevealed = True
      }

celador :: CardDef
celador =
  ((objective "05025" "Celador" IntoIthilien 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 2, cdDefense = Just 2, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Ranger]
    }

ithilienGuardian :: CardDef
ithilienGuardian =
  ((objective "05026" "Ithilien Guardian" IntoIthilien 2)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 2, cdDefense = Just 1, cdHitPoints = Just 2 })
    { cdCardTraits = setFromList [Gondor, Ranger]
    , -- its surge is its When Revealed's, not printed
      cdWhenRevealed = True
    , cdShadowEffect = True
    }

alcaronsScroll :: CardDef
alcaronsScroll =
  unique
    (objective "05019" "Alcaron's Scroll" PerilInPelargir 1)
      { cdCardTraits = setFromList [Scroll]
      }

rescuedVillagers :: CardDef
rescuedVillagers = objective "06068" "Rescued Villagers" EncounterAtAmonDin 1

deadVillagers :: CardDef
deadVillagers = objective "06069" "Dead Villagers" EncounterAtAmonDin 1

lordAlcaron :: CardDef
lordAlcaron =
  ((objective "06070" "Lord Alcaron" EncounterAtAmonDin 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 2, cdDefense = Just 2, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Noble]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

toTheTower :: CardDef
toTheTower =
  unique
    (objective "06147" "To the Tower" TheMorgulVale 1)
      { cdCardTraits = setFromList [Captured]
      }

clue :: CardCode -> Name -> CardDef
clue code name =
  (objective code name TheStewardsFear 1)
    { cdCardTraits = setFromList [Clue]
    , cdVictoryPoints = Just 1
    }

aScrapOfHistory :: CardDef
aScrapOfHistory = clue "06026" "A Scrap of History"

secretMap :: CardDef
secretMap = clue "06027" "Secret Map"

aPrisoner :: CardDef
aPrisoner = clue "06028" "A Prisoner"

plot :: CardCode -> Name -> CardDef
plot code name = (objective code name TheStewardsFear 1) {cdCardTraits = setFromList [Plot]}

unholyAlliance :: CardDef
unholyAlliance = plot "06029" "Unholy Alliance"

upInFlames :: CardDef
upInFlames = plot "06030" "Up in Flames"

poisonedCounsels :: CardDef
poisonedCounsels = plot "06031" "Poisoned Counsels"

faramirObjective :: CardDef
faramirObjective =
  ((objective "06132" "Faramir" TheBloodOfGondor 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 3, cdDefense = Just 1, cdHitPoints = Just 4 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Noble, Ranger]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

lordAlcaronTheBloodOfGondor :: CardDef
lordAlcaronTheBloodOfGondor =
  ((objective "06133" "Lord Alcaron" TheBloodOfGondor 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 2, cdDefense = Just 2, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Noble]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

mugashObjective :: CardDef
mugashObjective =
  (objective "07031" "Mugash" IntoFangorn 1)
    { cdCardTraits = setFromList [Orc, Uruk]
    , cdKeywords = setFromList [Guarded]
    }

-- | The Fords of Isen's objective ally, not the hero.
grimaTheFordsOfIsen :: CardDef
grimaTheFordsOfIsen =
  ((objective "07016" "Gríma" TheFordsOfIsen 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 1, cdDefense = Just 2, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Rohan]
    }

chiefTurchTheAntleredCrown :: CardDef
chiefTurchTheAntleredCrown =
  ((objective "08150" "Chief Turch" TheAntleredCrown 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 3, cdDefense = Just 2, cdHitPoints = Just 5 })
    { cdUnique = True
    , cdCardTraits = setFromList [Dunland, BoarClan]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

nalirTroubleInTharbad :: CardDef
nalirTroubleInTharbad =
  ((objective "08068" "Nalir" TroubleInTharbad 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 0, cdAttack = Just 1, cdDefense = Just 2, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

theOrcsSearch :: CardDef
theOrcsSearch = objective "08125" "The Orcs' Search" CelebrimborsSecret 1

celebrimborsMould :: CardDef
celebrimborsMould =
  unique
    (objective "08127" "Celebrimbor's Mould" CelebrimborsSecret 1)
      { cdCardTraits = setFromList [Artifact, Item]
      }

nalirTheNinInEilph :: CardDef
nalirTheNinInEilph =
  ((objective "08102" "Nalir" TheNinInEilph 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 0, cdAttack = Just 1, cdDefense = Just 2, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Dwarf]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

-- | A Key of The Three Trials: the first player claims it as soon as it is unattached.
threeTrialsKey :: CardCode -> Name -> Trait -> CardDef
threeTrialsKey code name trait =
  (objective code name TheThreeTrials 1)
    { cdCardTraits = setFromList [Item, Key, trait]
    , cdShadowEffect = True
    }

keyOfTheBoar :: CardDef
keyOfTheBoar = threeTrialsKey "08043" "Key of the Boar" Boar

keyOfTheWolf :: CardDef
keyOfTheWolf = threeTrialsKey "08044" "Key of the Wolf" Wolf

keyOfTheRaven :: CardDef
keyOfTheRaven = threeTrialsKey "08045" "Key of the Raven" Raven

-- | The Time objective, Daybreak faceup: flipped, it is 'nightfall'.
daybreak :: CardDef
daybreak = (objective "10011" "Daybreak" TheWastesOfEriador 1) {cdCardTraits = setFromList [TimeTrait]}

-- | The Time objective's other side: no copy of it is gathered, Daybreak turns into it.
nightfall :: CardDef
nightfall = (objective "10012" "Nightfall" TheWastesOfEriador 0) {cdCardTraits = setFromList [TimeTrait]}

amarthiul :: CardDef
amarthiul =
  ((objective "10016" "Amarthiúl" TheWastesOfEriador 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 3, cdDefense = Just 3, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Dunedain, Ranger, Warrior]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

-- | X willpower, attack and defense, X the number of quest cards in play: gained from its
-- own modifiers.
iarion :: CardDef
iarion =
  ((objective "09016" "Iârion" TheLostRealm 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 0, cdAttack = Just 0, cdDefense = Just 0, cdHitPoints = Just 4 })
    { cdUnique = True
    , cdCardTraits = setFromList [Dunedain, Ranger]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

huntingTheOrcs :: CardDef
huntingTheOrcs = (objective "09071" "Hunting the Orcs" TheWeatherHills 1) {cdCardTraits = setFromList [Mission]}

-- | Hunting the Orcs' other side: no copy of its own in the encounter set, it comes into
-- play as Hunting the Orcs flips.
savageCounterattack :: CardDef
savageCounterattack = (objective "09072" "Savage Counter-attack" TheWeatherHills 0) {cdCardTraits = setFromList [Mission]}

amarthiulAcrossTheEttenmoors :: CardDef
amarthiulAcrossTheEttenmoors =
  ((objective "10067" "Amarthiúl" AcrossTheEttenmoors 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 3, cdDefense = Just 3, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Dunedain, Ranger, Warrior]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

amarthiulTheTreacheryOfRhudaur :: CardDef
amarthiulTheTreacheryOfRhudaur =
  ((objective "10096" "Amarthiúl" TheTreacheryOfRhudaur 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 3, cdDefense = Just 3, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Dunedain, Ranger, Warrior]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

-- | The other side of Quiet the Spirits, never in the encounter deck.
daechanarsBrand :: CardDef
daechanarsBrand = (objective "10110" "Daechanar's Brand" TheTreacheryOfRhudaur 0) {cdCardTraits = setFromList [Clue]}

-- | The other side of Sift through the Debris, never in the encounter deck.
heirloomOfIarchon :: CardDef
heirloomOfIarchon = (objective "10112" "Heirloom of Iârchon" TheTreacheryOfRhudaur 0) {cdCardTraits = setFromList [Clue]}

-- | The other side of Decipher Ancient Texts, never in the encounter deck.
ordersFromAngmar :: CardDef
ordersFromAngmar = (objective "10114" "Orders from Angmar" TheTreacheryOfRhudaur 0) {cdCardTraits = setFromList [Clue]}

-- * The Dream-chaser's Fleet: Ship objectives, immune to player card effects by their own
-- modifiers, under the players' control from setup

dreamChaser :: CardDef
dreamChaser =
  ((objective "11080" "Dream-chaser" TheDreamChasersFleet 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 5, cdAttack = Just 4, cdDefense = Just 4, cdHitPoints = Just 18 })
    { cdUnique = True
    , cdCardTraits = setFromList [GreyHavens, Ship]
    , cdKeywords = setFromList [Sentinel]
    }

dawnStar :: CardDef
dawnStar =
  ((objective "11081" "Dawn Star" TheDreamChasersFleet 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 3, cdAttack = Just 3, cdDefense = Just 4, cdHitPoints = Just 16 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Ship]
    , cdKeywords = setFromList [Sentinel]
    }

narelenya :: CardDef
narelenya =
  ((objective "11082" "Nárelenya" TheDreamChasersFleet 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 3, cdAttack = Just 3, cdDefense = Just 5, cdHitPoints = Just 16 })
    { cdUnique = True
    , cdCardTraits = setFromList [GreyHavens, Ship]
    , cdKeywords = setFromList [Sentinel]
    }

silverWing :: CardDef
silverWing =
  ((objective "11083" "Silver Wing" TheDreamChasersFleet 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 4, cdDefense = Just 2, cdHitPoints = Just 14 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Ship]
    , cdKeywords = setFromList [Sentinel, Ranged]
    }

calphon :: CardDef
calphon =
  ((objective "11028" "Calphon" TheFateOfNumenor 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 0, cdDefense = Just 1, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Noble]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

-- | The objective-ally side of Captain Sahír: no copy is gathered, his enemy side turns
-- into it, or his scenarios put him into play this side faceup.
captainSahirObjective :: CardDef
captainSahirObjective =
  ((objective "11077" "Captain Sahír" TheStormcallerElite 0)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 0, cdAttack = Just 0, cdDefense = Just 2, cdHitPoints = Just 5 })
    { cdUnique = True
    , cdCardTraits = setFromList [Corsair, Raider]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    , cdOtherSide = Just "11076"
    }

-- | The objective-ally side of Na'asiyah.
naasiyahObjective :: CardDef
naasiyahObjective =
  ((objective "11079" "Na'asiyah" TheStormcallerElite 0)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 2, cdDefense = Just 2, cdHitPoints = Just 4 })
    { cdUnique = True
    , cdCardTraits = setFromList [Corsair, Raider]
    , cdKeywords = setFromList [LastPlayerGainsControl]
    , cdOtherSide = Just "11078"
    }

theHavensBurn :: CardDef
theHavensBurn = objective "11038" "The Havens Burn" RaidOnTheGreyHavens 1

gateKey :: CardDef
gateKey = unique (objective "12071" "Gate Key" TempleOfTheDeceived 1) {cdCardTraits = setFromList [Item, Artifact]}

-- * A Storm on Cobas Haven

-- | Set aside at setup; The Beacon gives one to a player.
dolAmrothWarship :: CardDef
dolAmrothWarship =
  (objective "12123" "Dol Amroth Warship" AStormOnCobasHaven 2)
    { cdCardType = ObjectiveAllyType
    , cdWillpower = Just 2
    , cdAttack = Just 2
    , cdDefense = Just 2
    , cdHitPoints = Just 8
    , cdCardTraits = setFromList [Gondor, Ship]
    }

-- | Corsair Skirmisher taken by the players as a Ship objective, with willpower equal to
-- its threat: no copy is gathered, the enemy turns into it.
corsairSkirmisherShip :: CardDef
corsairSkirmisherShip =
  ((objective "12125-ship" "Corsair Skirmisher" AStormOnCobasHaven 0)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 3, cdAttack = Just 4, cdDefense = Just 4, cdHitPoints = Just 6 })
    { cdCardTraits = setFromList [Corsair, Ship]
    , cdOtherSide = Just "12125"
    }

dolAmrothTower :: CardCode -> Name -> CardDef
dolAmrothTower code name =
  (objective code name AStormOnCobasHaven 1)
    { cdUnique = True
    , cdCardTraits = setFromList [DolAmroth]
    , cdKeywords = setFromList [Guarded]
    }

seaWardTower :: CardDef
seaWardTower = dolAmrothTower "12134" "Sea-ward Tower"

towerOfTheGull :: CardDef
towerOfTheGull = dolAmrothTower "12135" "Tower of the Gull"

theBeacon :: CardDef
theBeacon = dolAmrothTower "12136" "The Beacon"

towerOfTheHeron :: CardDef
towerOfTheHeron = dolAmrothTower "12137" "Tower of the Heron"

-- * The Dream-chaser Campaign Expansion

upgradedObjective :: CardDef -> CardDef
upgradedObjective def = def {cdCardCode = CardCode (unCardCode (cdCardCode def) <> "U"), cdCardTraits = insertSet Upgraded (cdCardTraits def)}

-- | A burden attached to an enemy, as the campaign card's setup says.
blackKeyBurden :: CardDef
blackKeyBurden = unique (objective "DCC192" "Black Key" DreamChaserCampaign 0) {cdCardTraits = setFromList [Item, Artifact, Burden]}

blackKeyBurdenUpgraded :: CardDef
blackKeyBurdenUpgraded = upgradedObjective blackKeyBurden

theBlackSword :: CardDef
theBlackSword = unique (objective "DCC216" "The Black Sword" DreamChaserCampaign 0) {cdCardTraits = setFromList [Item, Weapon, Burden]}

calaerion :: CardDef
calaerion =
  ((objective "DCC193" "Calaerion" DreamChaserCampaign 0)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 1, cdDefense = Just 1, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Noldor, Scout, Boon]
    }

calaerionUpgraded :: CardDef
calaerionUpgraded = (upgradedObjective calaerion) {cdWillpower = Just 2, cdAttack = Just 2, cdDefense = Just 2, cdHitPoints = Just 4}

gavin :: CardDef
gavin =
  ((objective "DCC210" "Gavin" DreamChaserCampaign 0)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 1, cdDefense = Just 0, cdHitPoints = Just 2 })
    { cdUnique = True
    , cdCardTraits = setFromList [Creature, Scout, Boon]
    , cdKeywords = setFromList [Surge, Ranged]
    , cdWhenRevealed = True
    , cdSailingIcons = 1
    }

gateKeyUpgraded :: CardDef
gateKeyUpgraded = (upgradedObjective gateKey) {cdCardCode = "DCC078U"}

dreamChaserUpgraded :: CardDef
dreamChaserUpgraded = (upgradedObjective dreamChaser) {cdCardCode = "DCC136U", cdWillpower = Just 6, cdAttack = Just 5, cdDefense = Just 5, cdHitPoints = Just 19}

dawnStarUpgraded :: CardDef
dawnStarUpgraded = (upgradedObjective dawnStar) {cdCardCode = "DCC137U", cdWillpower = Just 4, cdAttack = Just 4, cdDefense = Just 5, cdHitPoints = Just 18}

narelenyaUpgraded :: CardDef
narelenyaUpgraded = (upgradedObjective narelenya) {cdCardCode = "DCC140U", cdWillpower = Just 4, cdAttack = Just 4, cdDefense = Just 6, cdHitPoints = Just 17}

silverWingUpgraded :: CardDef
silverWingUpgraded = (upgradedObjective silverWing) {cdCardCode = "DCC141U", cdWillpower = Just 3, cdAttack = Just 6, cdDefense = Just 3, cdHitPoints = Just 16}
-- The Angmar Awakened campaign's burdens

-- | Protect the Innocent's other side.
arnorRavaged :: CardDef
arnorRavaged = (objective "AAC158b" "Arnor Ravaged" AngmarAwakenedCampaign 1) {cdCardTraits = setFromList [Burden]}

thaurdirsLegacy :: CardDef
thaurdirsLegacy = (unique $ objective "AAC168" "Thaurdir's Legacy" AngmarAwakenedCampaign 1) {cdCardTraits = setFromList [Burden]}

thaurdirsMajesty :: CardDef
thaurdirsMajesty = (unique $ objective "AAC169" "Thaurdir's Majesty" AngmarAwakenedCampaign 1) {cdCardTraits = setFromList [Burden]}

thaurdirsSpite :: CardDef
thaurdirsSpite = (unique $ objective "AAC170" "Thaurdir's Spite" AngmarAwakenedCampaign 1) {cdCardTraits = setFromList [Burden]}
-- The Lost Realm Nightmare Decks

-- | Shuffled into the Orc deck in Nightmare mode, it comes into play guarded by the Orc
-- deck's top card: free of encounters, the first player gains control of it.
captiveVillager :: CardDef
captiveVillager =
  ((objective "the-lost-realm-nightmare-the-weather-hills-012" "Captive Villager" TheWeatherHillsNightmare 3)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 0, cdAttack = Just 0, cdDefense = Just 1, cdHitPoints = Just 2 })
    { cdCardTraits = setFromList [Bree]
    }

-- Dream-chaser Nightmare Decks

aearlang :: CardDef
aearlang =
  unique
    (objective "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-002" "Aearlang" AStormOnCobasHavenNightmare 1)
      { cdCardTraits = setFromList [DolAmroth, Besieged]
      , cdKeywords = setFromList [Guarded]
      , cdSailingIcons = 1
      }

docksOfDolAmroth :: CardDef
docksOfDolAmroth =
  unique
    (objective "a-storm-on-cobas-haven-nightmare-a-storm-on-cobas-haven-003" "Docks of Dol Amroth" AStormOnCobasHavenNightmare 1)
      { cdCardTraits = setFromList [DolAmroth, Besieged]
      , cdKeywords = setFromList [Guarded]
      , cdSailingIcons = 1
      }

theCursedSword :: CardDef
theCursedSword =
  unique
    (objective "the-city-of-corsairs-nightmare-the-city-of-corsairs-007" "The Cursed Sword" TheCityOfCorsairsNightmare 1)
      { cdCardTraits = setFromList [Artifact, Item, Cursed]
      }

-- The Steward's Fear Nightmare

riseToPower :: CardDef
riseToPower = (objective "the-stewards-fear-nightmare-the-stewards-fear-002" "Rise to Power" TheStewardsFearNightmare 1) {cdCardTraits = setFromList [Plot]}

assassination :: CardDef
assassination = (objective "the-stewards-fear-nightmare-the-stewards-fear-003" "Assassination" TheStewardsFearNightmare 1) {cdCardTraits = setFromList [Plot]}

theCabalsChampion :: CardDef
theCabalsChampion =
  (objective "the-stewards-fear-nightmare-the-stewards-fear-010" "The Cabal's Champion" TheStewardsFearNightmare 3)
    { cdCardTraits = setFromList [Title]
    , cdKeywords = setFromList [Surge, Doomed 1]
    }

-- The Long Arm of Mordor

-- | An objective-hero of Kahliel's tribe: a neutral hero with no threat cost.
objectiveHero :: CardCode -> Name -> Int -> Int -> Int -> Int -> CardDef
objectiveHero code name willpower attack defense hitPoints =
  unique
    (objective code name TheLongArmOfMordor 1)
      { cdCardType = ObjectiveHeroType
      , cdSphere = Just Neutral
      , cdWillpower = Just willpower
      , cdAttack = Just attack
      , cdDefense = Just defense
      , cdHitPoints = Just hitPoints
      }

kahliel :: CardDef
kahliel = (objectiveHero "16031" "Kahliel" 2 2 2 3) {cdCardTraits = setFromList [Harad, Noble]}

firyal :: CardDef
firyal = (objectiveHero "16032" "Firyal" 3 1 2 3) {cdCardTraits = setFromList [Harad, Scout]}

yazan :: CardDef
yazan = (objectiveHero "16033" "Yazan" 2 3 1 3) {cdCardTraits = setFromList [Harad, Ranger], cdKeywords = setFromList [Ranged]}

jubayr :: CardDef
jubayr = (objectiveHero "16034" "Jubayr" 1 2 3 3) {cdCardTraits = setFromList [Harad, Warrior], cdKeywords = setFromList [Sentinel]}
-- * Beneath the Sands

-- | The Search objective, Off Track faceup as it is set up: one card, On Track on its other
-- side, the side faceup kept as its card code.
offTrack :: CardDef
offTrack = (objective "17079" "Off Track" BeneathTheSands 1) {cdCardTraits = setFromList [SearchTrait]}

-- | The Search objective's other side: no copy of it is gathered, Off Track turns into it.
onTrack :: CardDef
onTrack = (objective "17080" "On Track" BeneathTheSands 0) {cdCardTraits = setFromList [SearchTrait]}

-- * Escape from Umbar

seizeThem :: CardDef
seizeThem = objective "16014" "Seize Them!" EscapeFromUmbar 1

-- * Race Across Harad

tamedMumak :: CardDef
tamedMumak =
  ((objective "17038" "Tamed Mumak" RaceAcrossHarad 4)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 5, cdDefense = Just 2, cdHitPoints = Just 8 })
    { cdCardTraits = setFromList [Creature]
    }

-- * The Black Serpent: the Harad objectives, each Guarded (X) (MEC59 rules)

bannerOfTheSerpent :: CardDef
bannerOfTheSerpent =
  (objective "17091" "Banner of the Serpent" TheBlackSerpent 1)
    { cdCardTraits = setFromList [Item, Harad]
    , cdKeywords = setFromList [GuardedBy [EnemyType, LocationType]]
    }

theSerpentsGarb :: CardDef
theSerpentsGarb =
  (objective "17092" "The Serpent's Garb" TheBlackSerpent 1)
    { cdCardTraits = setFromList [Item, Harad]
    , cdKeywords = setFromList [GuardedBy [EnemyType]]
    }

summonsFromMordor :: CardDef
summonsFromMordor =
  (objective "17093" "Summons from Mordor" TheBlackSerpent 1)
    { cdCardTraits = setFromList [Item, Harad]
    , cdKeywords = setFromList [GuardedBy [LocationType]]
    }

-- The Dungeons of Cirith Gurat (17119-17120). Abaan's stats from Hall of Beorn.

theCaptivesOfCirithGurat :: CardDef
theCaptivesOfCirithGurat = objective "17119" "The Captives of Cirith Gurat" TheDungeonsOfCirithGurat 1

abaan :: CardDef
abaan =
  ((objective "17120" "Abaan" TheDungeonsOfCirithGurat 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 1, cdDefense = Just 1, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Harad]
    }

-- The Mûmakil

-- | Capture objectives: set aside, then attached to a Wild Mûmak by Capture the Oliphaunts.
captureObjective :: CardCode -> Name -> CardDef
captureObjective code name = (objective code name TheMumakil 1) {cdCardTraits = setFromList [CaptureTrait], cdVictoryPoints = Just 1}

horseHairLasso :: CardDef
horseHairLasso = captureObjective "17011" "Horse-hair Lasso"

poisonedSpear :: CardDef
poisonedSpear = captureObjective "17012" "Poisoned Spear"

pitTrap :: CardDef
pitTrap = captureObjective "17013" "Pit Trap"

nooseOfVines :: CardDef
nooseOfVines = captureObjective "17014" "Noose of Vines"

-- * The Crossings of Poros: its two encounter sets, The Crossings of Poros and Mountains of
-- Shadow. Easy mode copy counts from Hall of Beorn's scenario page.

porosGarrison :: CardDef
porosGarrison =
  ((objective "17145" "Poros Garrison" TheCrossingsOfPoros 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 3, cdAttack = Just 4, cdDefense = Just 3, cdHitPoints = Just 6 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

-- | The objective-ally side of Woodmen Village (18069A), shared by Journey Up the Anduin and
-- Lost in Mirkwood: no copy is gathered; Woodmen Village turns into it, or Lost in Mirkwood
-- puts him into play this side faceup.
haldan :: CardDef
haldan =
  unique
    ((objective "18069B" "Haldan" JourneyUpTheAnduin 0)
        { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 3, cdDefense = Just 1, cdHitPoints = Just 4 })
      { cdCardTraits = setFromList [Woodman, Scout]
      , cdKeywords = setFromList [FirstPlayerGainsControl]
      , cdOtherSide = Just "18069A"
      }

-- * The Fate of Wilderland

theGoblinsAssault :: CardDef
theGoblinsAssault = (objective "19155" "The Goblins' Assault" TheFateOfWilderland 1) {cdCardTraits = setFromList [Assault]}

theHeroesDefense :: CardDef
theHeroesDefense = (objective "19156" "The Heroes' Defense" TheFateOfWilderland 1) {cdCardTraits = setFromList [Defense]}
-- * The Ghost of Framsburg (19094-19111)

cryptKey :: CardDef
cryptKey =
  unique
    (objective "19094" "Crypt Key" TheGhostOfFramsburg 1)
      { cdCardTraits = setFromList [Artifact, Item]
      , cdKeywords = setFromList [Loot]
      }

oldLantern :: CardDef
oldLantern =
  unique
    (objective "19095" "Old Lantern" TheGhostOfFramsburg 1)
      { cdCardTraits = setFromList [Artifact, Item]
      , cdKeywords = setFromList [Loot]
      }

scathasTeeth :: CardDef
scathasTeeth =
  unique
    (objective "19096" "Scatha's Teeth" TheGhostOfFramsburg 1)
      { cdCardTraits = setFromList [Artifact, Item]
      , cdKeywords = setFromList [Loot]
      }

-- | Mount Gundabad's Wormsbane, attached to a hero at setup on its Broken side; The First
-- Forge turns it to its Reforged side ('wormsbaneReforged'). Hall of Beorn prints it as an
-- objective. Neither side is gathered into the encounter deck.
wormsbane :: CardDef
wormsbane =
  unique
    (objective "19143A" "Wormsbane" MountGundabad 1)
      { cdCardTraits = setFromList [Artifact, Weapon, Broken]
      , cdKeywords = setFromList [Restricted]
      , cdOtherSide = Just "19143B"
      }

wormsbaneReforged :: CardDef
wormsbaneReforged =
  unique
    (objective "19143B" "Wormsbane" MountGundabad 1)
      { cdCardTraits = setFromList [Artifact, Weapon, Reforged]
      , cdKeywords = setFromList [Restricted]
      , cdOtherSide = Just "19143A"
      }
-- * The Withered Heath

dragonSign :: CardDef
dragonSign =
  (objective "19012" "Dragon Sign" TheWitheredHeath 4)
    { cdCardTraits = setFromList [Clue]
    , cdKeywords = setFromList [GuardedBy [EnemyType]]
    }
-- * Roam Across Rhovanion

durinsKey :: CardDef
durinsKey = unique (objective "19036" "Durin's Key" RoamAcrossRhovanion 1) {cdCardTraits = setFromList [Artifact, Item]}

urdugsHorn :: CardDef
urdugsHorn = unique (objective "19037" "Urdug's Horn" RoamAcrossRhovanion 1) {cdCardTraits = setFromList [Artifact, Item]}

-- | The objective-ally side of Urdug; his enemy side is 19053 (Hall of Beorn's numbering).
urdug :: CardDef
urdug =
  unique
    ((objective "19052" "Urdug" RoamAcrossRhovanion 1)
        { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 3, cdDefense = Just 2, cdHitPoints = Just 5 })
      { cdCardTraits = setFromList [Goblin, Orc]
      , cdKeywords = setFromList [FirstPlayerGainsControl]
      , cdOtherSide = Just "19053"
      }

-- | The objective-ally side of Tiny; his enemy side is 19055.
tiny :: CardDef
tiny =
  unique
    ((objective "19054" "Tiny" RoamAcrossRhovanion 1)
        { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 6, cdDefense = Just 3, cdHitPoints = Just 9 })
      { cdCardTraits = setFromList [Troll]
      , cdKeywords = setFromList [FirstPlayerGainsControl]
      , cdOtherSide = Just "19055"
      }
-- | Lost in Mirkwood's objective, added to the staging area as stage 1A is set up: never
-- in the encounter deck.
searchingForAWayOut :: CardDef
searchingForAWayOut = objective "18028" "Searching for a Way Out" LostInMirkwood 0

-- * The Power of Mordor: the Power of Mordor deck's, not the encounter deck's

underTheWatchfulEye :: CardDef
underTheWatchfulEye = (objective "21068" "Under the Watchful Eye" ThePowerOfMordor 1) {cdCardTraits = setFromList [Mordor], cdWhenRevealed = True}

theArmiesOfMordor :: CardDef
theArmiesOfMordor = (objective "21069" "The Armies of Mordor" ThePowerOfMordor 1) {cdCardTraits = setFromList [Mordor]}

coveredInDarkness :: CardDef
coveredInDarkness = (objective "21070" "Covered in Darkness" ThePowerOfMordor 1) {cdCardTraits = setFromList [Mordor], cdKeywords = setFromList [Doomed 5]}

-- * The River Running

-- | Added to the staging area as stage 1A is set up.
easterlingPursuit :: CardDef
easterlingPursuit = objective "21012" "Easterling Pursuit" TheRiverRunning 1
-- * Danger in Dorwinion

bribes :: CardDef
bribes = (objective "21033" "Bribes" DangerInDorwinion 1) {cdCardTraits = setFromList [Plot]}

submitOrDie :: CardDef
submitOrDie = (objective "21034" "Submit or Die" DangerInDorwinion 1) {cdCardTraits = setFromList [Plot]}

sauronWorship :: CardDef
sauronWorship = (objective "21035" "Sauron Worship" DangerInDorwinion 1) {cdCardTraits = setFromList [Plot]}

evidenceOfTheCult :: CardDef
evidenceOfTheCult =
  (objective "21036" "Evidence of the Cult" DangerInDorwinion 3)
    { cdCardTraits = setFromList [Clue]
    , cdKeywords = setFromList [GuardedBy [EnemyType]]
    , cdVictoryPoints = Just 1
    }
-- * Challenge of the Wainriders

theWainriders :: CardDef
theWainriders = objective "22064" "The Wainriders" ChallengeOfTheWainriders 1

theChallengers :: CardDef
theChallengers = objective "22065" "The Challengers" ChallengeOfTheWainriders 1

-- * The Land of Sorrow

-- | A Captive objective ally, guarded by a Camp location from stage 1A until the first
-- player gains control of it.
captive :: CardCode -> Name -> Trait -> Int -> Int -> Int -> Int -> CardDef
captive code name trait willpower attack defense hitPoints =
  unique
    ((objective code name TheLandOfSorrow 1)
        { cdCardType = ObjectiveAllyType, cdWillpower = Just willpower, cdAttack = Just attack, cdDefense = Just defense, cdHitPoints = Just hitPoints })
      { cdCardTraits = setFromList [Captive, trait]
      }

edrahil :: CardDef
edrahil = captive "22124" "Edrahil" Silvan 3 1 0 3

arador :: CardDef
arador = captive "22125" "Arador" Gondor 0 1 3 5

farin :: CardDef
farin = captive "22126" "Farin" Dwarf 1 3 0 4

-- * The Passing of the Grey Company

-- | A burden of The Passing of the Grey Company's burden set: never in an encounter
-- deck. Stage 1A attaches a copy to each player's threat dial; won with copies still
-- attached, the campaign pool records them for those players, whose dials they go onto as
-- each later scenario is set up. Limit 1 per threat dial.
overcomeByFear :: CardDef
overcomeByFear =
  (objective "145016" "Overcome by Fear" ThePassingOfTheGreyCompany 4)
    { cdEncounterSet = Nothing
    , cdSetIcon = Just ThePassingOfTheGreyCompany
    , cdCardTraits = setFromList [Fear, Burden]
    }

-- | The boon side of Army of the Dead, earned by defeating its enemy side in campaign mode
-- (The Flame of the West rules insert, "Army of the Dead").
armyOfTheDeadBoon :: CardDef
armyOfTheDeadBoon =
  ((objective "145068" "Army of the Dead" ThePassingOfTheGreyCompany 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 4, cdAttack = Just 6, cdDefense = Just 2, cdHitPoints = Just 8 })
    { cdUnique = True
    , cdEncounterSet = Nothing
    , cdSetIcon = Just ThePassingOfTheGreyCompany
    , cdCardTraits = setFromList [Undead, Boon]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    , cdOtherSide = Just "145067"
    }

-- * The Lord of the Rings saga

{- | A box's One Ring: put into play attached to the Ring-bearer as the scenario is set up
(see 'LordOfTheRings.Message.Lifted.setUpFellowship'), never gathered with an encounter
set.
-}
sagaOneRing :: CardCode -> CardDef
sagaOneRing code =
  unique
    (emptyCardDef code "The One Ring" ObjectiveType)
      { cdCardTraits = setFromList [Artifact, Item, Ring]
      }

theOneRingTheLandOfShadow :: CardDef
theOneRingTheLandOfShadow = sagaOneRing "144063"

theOneRingTheMountainOfFire :: CardDef
theOneRingTheMountainOfFire = sagaOneRing "146065"

theOneRingTheBlackRiders :: CardDef
theOneRingTheBlackRiders = sagaOneRing "141085"

theOneRingTheRoadDarkens :: CardDef
theOneRingTheRoadDarkens = sagaOneRing "142091"

-- * Gollum (The Land of Shadow)

-- | Gollum's objective-ally side: no copy is gathered, his enemy side turns into it, or his
-- scenarios put him into play this side faceup.
smeagolTheLandOfShadow :: CardDef
smeagolTheLandOfShadow =
  ((objective "144076" "Sméagol" GollumSaga 0)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 2, cdAttack = Just 2, cdDefense = Just 2, cdHitPoints = Just 5 })
    { cdUnique = True
    , cdCardTraits = setFromList [Gollum]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    , cdOtherSide = Just "144075"
    }


-- * The Battle of Five Armies

mistyMountainEagle :: CardDef
mistyMountainEagle =
  ( (objective "132052" "Misty Mountain Eagle" TheBattleOfFiveArmies 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 3, cdAttack = Just 3, cdDefense = Just 3, cdHitPoints = Just 2 }
  )
    { cdCardTraits = setFromList [Creature, Eagle]
    , cdShadowEffect = True
    }

-- * We Must Away, Ere Break of Day

trollPurse :: CardDef
trollPurse = unique (objective "131042" "Troll Purse" WeMustAwayEreBreakOfDay 1) {cdCardTraits = setFromList [Item]}

trollKey :: CardDef
trollKey = unique (objective "131043" "Troll Key" WeMustAwayEreBreakOfDay 1) {cdCardTraits = setFromList [Item]}

-- | A Sack card: the scenario sets it in the sack deck, never the encounter deck.
sackCard :: CardCode -> Name -> CardDef
sackCard code name = (objective code name WeMustAwayEreBreakOfDay 1) {cdCardTraits = setFromList [Sack]}

aSmellySack :: CardDef
aSmellySack = sackCard "131044" "A Smelly Sack"

aStrongSack :: CardDef
aStrongSack = sackCard "131045" "A Strong Sack"

aToughSack :: CardDef
aToughSack = sackCard "131046" "A Tough Sack"

aLargeSack :: CardDef
aLargeSack = sackCard "131047" "A Large Sack"

aDeepSack :: CardDef
aDeepSack = sackCard "131048" "A Deep Sack"

aFoulSack :: CardDef
aFoulSack = sackCard "131049" "A Foul Sack"

aWornSack :: CardDef
aWornSack = sackCard "131050" "A Worn Sack"

-- * Dungeons Deep and Caverns Dim

bilbosMagicRing :: CardDef
bilbosMagicRing = unique (objective "131079" "Bilbo's Magic Ring" DungeonsDeepAndCavernsDim 1) {cdCardTraits = setFromList [Artifact, Ring]}

-- | Set aside by stage 1A, until stage 3A adds it to the staging area.
aBarePatch :: CardDef
aBarePatch = (objective "132039" "A Bare Patch" TheLonelyMountain 1) {cdCardTraits = setFromList [Information]}

-- * A Shadow of the Past

-- | A boon with an encounter card back: earned, it is shuffled into the encounter deck
-- (The Black Riders rules insert, "Boons & Burdens").
gildorInglorionObjective :: CardDef
gildorInglorionObjective =
  ((objective "141077" "Gildor Inglorion" AShadowOfThePast 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 3, cdAttack = Just 2, cdDefense = Just 3, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdEncounterSet = Nothing
    , cdSetIcon = Just AShadowOfThePast
    , cdCardTraits = setFromList [Noldor, Boon]
    , cdKeywords = setFromList [FirstPlayerGainsControl]
    }

-- | A burden: "Setup: Add Gandalf's Delay to the staging area."
gandalfsDelay :: CardDef
gandalfsDelay =
  (objective "141078" "Gandalf's Delay" AShadowOfThePast 1)
    { cdEncounterSet = Nothing
    , cdSetIcon = Just AShadowOfThePast
    , cdCardTraits = setFromList [Burden]
    }


-- * A Shadow of the Past

-- * Flight to the Ford

-- | Attach to the Ring-bearer: stage 1A's setup attaches it.
anEvilWound :: CardDef
anEvilWound = (objective "141057" "An Evil Wound" FlightToTheFord 1) {cdCardTraits = setFromList [Morgul]}

-- * The Siege of Gondor

-- | Its Corsair side: set aside by the scenario, added to the staging area by stage 1A. Its
-- Gondor side (145072) is played in The Battle of the Pelennor Fields.
theCorsairFleet :: CardDef
theCorsairFleet = unique (objective "145071" "The Corsair Fleet" TheSiegeOfGondor 1) {cdCardTraits = setFromList [Corsair]}
