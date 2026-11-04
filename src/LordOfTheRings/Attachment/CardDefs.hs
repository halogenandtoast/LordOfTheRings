module LordOfTheRings.Attachment.CardDefs (module LordOfTheRings.Attachment.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType (CardType (..))
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet (EncounterSet (FliesAndSpiders, TheLonelyMountain, WeMustAwayEreBreakOfDay))
import LordOfTheRings.Keyword
import LordOfTheRings.Name (Name)
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere
import LordOfTheRings.Trait

allAttachmentCards :: Map CardCode CardDef
allAttachmentCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ stewardOfGondor
      , iarionsPendant
      , cunning
      , hardy
      , resolute
      , ruthless
      , stalwart
      , amarthiulsCourage
      , chieftainsBrooch
      , mailOfEarnur
      , silverthorn
      , daechanarsBrand
      , heirloomOfIarchon
      , ordersFromAngmar
      , raimentOfTheSecondAge
      , valor
      , appointedByFate
      , celebriansStone
      , bladeOfGondolin
      , citadelPlate
      , dwarvenAxe
      , hornOfGondor
      , theFavorOfTheLady
      , powerInTheEarth
      , unexpectedCourage
      , forestSnare
      , protectorOfLorien
      , darkKnowledge
      , selfPreservation
      , dunedainMark
      , ancientMathom
      , narvisBelt
      , legacyOfDurin
      , pathOfNeed
      , healingHerbs
      , vilya
      , rangerSpikes
      , spearOfTheCitadel
      , gondorianShield
      , ithilienPit
      , visionaryLeadership
      , spearOfTheMark
      , steedOfTheMark
      , rohanWarhorse
      , firefoot
      , oLorien
      , lembas
      , bowOfTheGaladhrim
      , wingfoot
      , heirOfMardil
      , nenya
      , bloodOfNumenor
      , gondorianFire
      , mirrorOfGaladriel
      , captainOfGondor
      , cram
      , throrsMap
      , kingUnderTheMountain
      , dwarfPipe
      , theRedArrow
      , magicRing
      , warAxe
      , hornOfTheMark
      , valiantSword
      , theOneRing
      , innerStrength
      , strengthAndCourage
      , powerOfCommand
      , wellPreserved
      , goldenBelt
      , sarumansStaff
      , stoneOfElostirion
      , songOfKings
      , dunedainWarning
      , bornAloft
      , norAmIAStranger
      , aBurningBrand
      , songOfWisdom
      , dunedainQuest
      , songOfTravel
      , dunedainCache
      , songOfMocking
      , fastHitch
      , songOfBattle
      , dunedainSignal
      , supportOfTheEagles
      , dwarrowdelfAxe
      , bootsFromErebor
      , keepingCount
      , goodMeal
      , rivendellBlade
      , songOfEarendil
      , swordThatWasBroken
      , rivendellBow
      , resourceful
      , ringMail
      , everMyHeartRises
      , loveOfTales
      , lightOfValinor
      , asfaloth
      , hardyLeadership
      , miruvor
      , ringOfBarahir
      , mightyProwess
      , lordOfMorthond
      , bookOfEldacar
      , swordOfMorthond
      , mapOfEarnil
      , rangerBow
      , palantir
      , tomeOfAtanatar
      , poisonedStakes
      , scrollOfIsildur
      , keysOfOrthanc
      , silverLamp
      , elvenMail
      , cloakOfLorien
      , leafBrooch
      , defenderOfTheWest
      , theFallOfGilGalad
      , wardenOfArnor
      , theDaysRising
      , theLongDefeat
      , favorOfTheValar
      , swordOfNumenor
      , weatherStainedCloak
      , swordThain
      , rangerProvisions
      , steedOfImladris
      , elvenSpear
      , silverHarp
      , elfFriend
      , heirOfValandil
      , athelas
      , secretVigil
      , starBrooch
      , ravenWingedHelm
      , hobbitPony
      , grapplingHook
      , marinersCompass
      , explorersAlmanac
      , toTheSeaToTheSea
      , narya
      , rodOfTheSteward
      , inServiceOfTheSteward
      , raimentOfWar
      , guardedCeaselessly
      , armoredDestrier
      , entanglingNets
      , dunedainRemedy
      , hithlain
      , strider
      , runeMaster
      , vigilantGuard
      , windfola
      , rangerSpear
      , princeOfDolAmroth
      , armorPlating
      , armorPlatingUpgraded
      , ballista
      , ballistaUpgraded
      , enlargedQuarters
      , enlargedQuartersUpgraded
      , extraSails
      , extraSailsUpgraded
      , spyglass
      , spyglassUpgraded
      , blackKey
      , blackKeyUpgraded
      , parryingCutlass
      , parryingCutlassUpgraded
      , throwingAxe
      , throwingAxeUpgraded
      , cirdansBlessingLeadership
      , cirdansBlessingTactics
      , cirdansBlessingSpirit
      , cirdansBlessingLore
      , dwarvenShield
      , mirkwoodLongKnife
      , theRoadGoesEverOn
      , kahlielsHeaddress
      , steedOfTheNorth
      , mightyWarrior
      , haradrimSpear
      , firesideSong
      , doughtyRanger
      , songOfHope
      , fearlessScout
      , dunedainPipe
      , diligentNoble
      , legacyBlade
      , followed
      , sting
      , roundShield
      , armorOfErebor
      , mithrilShirt
      , radagastsStaff
      , hauberkOfMail
      , kingOfDale
      , bowOfYew
      , mapOfRhovanion
      , necklaceOfGirion
      , squiresHelm
      , woodmensClearing
      , greyCloak
      , theArkenstone
      , ancestralArmor
      , glamdring
      , woodmensPath
      , theElvenking
      , orcrist
      , leatherBoots
      , outmatched
      , warriorSword
      , valiantDetermination
      , ringOfThror
      , putOffPursuit
      , durinsAxeTactics
      , silverCirclet
      , keenLongbow
      , redBookOfWestmarch
      , songOfHealing
      , sparePipe
      , shiningShield
      , tirelessThoroughbred
      , thorongil
      , spareHoodAndCloak
      , greatYewBow
      , blackArrow
      , throrsKey
      , expertTreasureHunter
      , stingTreasure
      , orcristTreasure
      , glamdringTreasure
      , bilbosMagicRing
      , theArkenstoneTreasure
      , mithrilShirtTreasure
      , throrsBattleAxe
      , throrsHuntingBow
      , throrsGoldenCup
      ]

-- | A boon of the Revised Core campaign: Permanent, limit 1 per hero.
valor :: CardDef
valor =
  (attachment "RC007" "Valor" Neutral (StaticCost 0))
    { cdCardTraits = setFromList [Condition, Boon]
    , cdLimitOnePerHost = True
    }

-- | A boon of the Revised Core campaign, attached at Escape from Dol Guldur's setup.
appointedByFate :: CardDef
appointedByFate =
  (attachment "RC010" "Appointed by Fate" Neutral (StaticCost 0))
    { cdCardTraits = setFromList [Condition, Boon]
    }

stewardOfGondor :: CardDef
stewardOfGondor =
  (attachment "01026" "Steward of Gondor" Leadership (StaticCost 2))
    { cdUnique = True
    , cdCardTraits = setFromList [Gondor, Title]
    }

celebriansStone :: CardDef
celebriansStone =
  (attachment "01027" "Celebrían's Stone" Leadership (StaticCost 2))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    , cdKeywords = setFromList [Restricted]
    }

bladeOfGondolin :: CardDef
bladeOfGondolin =
  (attachment "01039" "Blade of Gondolin" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

citadelPlate :: CardDef
citadelPlate =
  (attachment "01040" "Citadel Plate" Tactics (StaticCost 4))
    { cdCardTraits = setFromList [Armor, Item]
    , cdKeywords = setFromList [Restricted]
    }

dwarvenAxe :: CardDef
dwarvenAxe =
  (attachment "01041" "Dwarven Axe" Tactics (StaticCost 2))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

hornOfGondor :: CardDef
hornOfGondor =
  (attachment "01042" "Horn of Gondor" Tactics (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    , cdKeywords = setFromList [Restricted]
    }

theFavorOfTheLady :: CardDef
theFavorOfTheLady =
  (attachment "01055" "The Favor of the Lady" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Condition]
    }

powerInTheEarth :: CardDef
powerInTheEarth =
  (attachment "01056" "Power in the Earth" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Condition]
    }

unexpectedCourage :: CardDef
unexpectedCourage =
  (attachment "01057" "Unexpected Courage" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Condition]
    }

forestSnare :: CardDef
forestSnare =
  (attachment "01069" "Forest Snare" Lore (StaticCost 3))
    { cdCardTraits = setFromList [Item, Trap]
    }

protectorOfLorien :: CardDef
protectorOfLorien =
  (attachment "01070" "Protector of Lórien" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Title]
    }

darkKnowledge :: CardDef
darkKnowledge =
  (attachment "01071" "Dark Knowledge" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Condition]
    }

selfPreservation :: CardDef
selfPreservation =
  (attachment "01072" "Self Preservation" Lore (StaticCost 3))
    { cdCardTraits = setFromList [Skill]
    }

dunedainMark :: CardDef
dunedainMark =
  (attachment "02002" "Dúnedain Mark" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Signal]
    }

ancientMathom :: CardDef
ancientMathom =
  (attachment "02056" "Ancient Mathom" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Mathom]
    }

narvisBelt :: CardDef
narvisBelt =
  (attachment "03003" "Narvi's Belt" Leadership (StaticCost 2))
    { cdUnique = True
    , cdCardTraits = setFromList [Item]
    }

legacyOfDurin :: CardDef
legacyOfDurin =
  (attachment "04061" "Legacy of Durin" Lore (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Condition]
    }

pathOfNeed :: CardDef
pathOfNeed =
  (attachment "04103" "Path of Need" Leadership (StaticCost 4))
    { cdCardTraits = setFromList [Condition]
    , cdDeckLimit = 1
    }

healingHerbs :: CardDef
healingHerbs =
  (attachment "04109" "Healing Herbs" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Item]
    }

vilya :: CardDef
vilya =
  (attachment "04137" "Vilya" Neutral (StaticCost 2))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Ring]
    }

spearOfTheCitadel :: CardDef
spearOfTheCitadel =
  (attachment "05009" "Spear of the Citadel" Tactics (StaticCost 2))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

rangerSpikes :: CardDef
rangerSpikes =
  (attachment "05017" "Ranger Spikes" Lore (StaticCost 2))
    { cdCardTraits = setFromList [Trap]
    }

gondorianShield :: CardDef
gondorianShield =
  (attachment "06005" "Gondorian Shield" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Armor, Item]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

ithilienPit :: CardDef
ithilienPit =
  (attachment "06064" "Ithilien Pit" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Trap]
    }

visionaryLeadership :: CardDef
visionaryLeadership =
  (attachment "06136" "Visionary Leadership" Leadership (StaticCost 2))
    { cdUnique = True
    , cdCardTraits = setFromList [Skill]
    }

spearOfTheMark :: CardDef
spearOfTheMark =
  (attachment "06137" "Spear of the Mark" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

steedOfTheMark :: CardDef
steedOfTheMark =
  (attachment "06139" "Steed of the Mark" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Mount]
    }

rohanWarhorse :: CardDef
rohanWarhorse =
  (attachment "07008" "Rohan Warhorse" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Mount]
    , cdKeywords = setFromList [Restricted]
    }

firefoot :: CardDef
firefoot =
  (attachment "08004" "Firefoot" Tactics (StaticCost 2))
    { cdUnique = True
    , cdCardTraits = setFromList [Mount]
    , cdKeywords = setFromList [Restricted]
    }

oLorien :: CardDef
oLorien =
  (attachment "08058" "O Lórien!" Leadership (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Song]
    }

lembas :: CardDef
lembas =
  (attachment "08064" "Lembas" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    }

bowOfTheGaladhrim :: CardDef
bowOfTheGaladhrim =
  (attachment "08088" "Bow of the Galadhrim" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

wingfoot :: CardDef
wingfoot =
  (attachment "08092" "Wingfoot" Lore (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Title]
    }

heirOfMardil :: CardDef
heirOfMardil =
  (attachment "08113" "Heir of Mardil" Leadership (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Title]
    }

nenya :: CardDef
nenya =
  (attachment "08121" "Nenya" Neutral (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Ring]
    }

captainOfGondor :: CardDef
captainOfGondor =
  (attachment "08140" "Captain of Gondor" Tactics (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Title]
    }

cram :: CardDef
cram =
  (attachment "131011" "Cram" Leadership (StaticCost 0))
    { cdCardTraits = setFromList [Item]
    }

throrsMap :: CardDef
throrsMap =
  (attachment "131013" "Thrór's Map" Lore (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    }

kingUnderTheMountain :: CardDef
kingUnderTheMountain =
  (attachment "132018" "King Under the Mountain" Leadership (StaticCost 2))
    { cdUnique = True
    , cdCardTraits = setFromList [Title]
    }

dwarfPipe :: CardDef
dwarfPipe =
  (attachment "17007" "Dwarf Pipe" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Item, Pipe]
    , cdLimitOnePerHost = True
    }

theRedArrow :: CardDef
theRedArrow =
  (attachment "17058" "The Red Arrow" Tactics (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    , cdDeckLimit = 1
    }

magicRing :: CardDef
magicRing =
  (attachment "17143" "Magic Ring" Neutral (StaticCost 2))
    { cdCardTraits = setFromList [Artifact, Item, Ring]
    , cdDeckLimit = 1
    , cdLimitOnePerHost = True
    }

warAxe :: CardDef
warAxe =
  (attachment "22032" "War Axe" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

hornOfTheMark :: CardDef
hornOfTheMark =
  (attachment "22035" "Horn of the Mark" Spirit (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    , cdKeywords = setFromList [Restricted]
    }

valiantSword :: CardDef
valiantSword =
  (attachment "22083" "Valiant Sword" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

-- | The One Ring: no sphere and no cost to play, as it is never played, but attached by its
-- Setup text (A Shadow in the East rules, "The One Ring & The Master Trait").
theOneRing :: CardDef
theOneRing =
  unique
    (attachment "21001" "The One Ring" Neutral (StaticCost 0))
      { cdCardTraits = setFromList [Artifact, Ring, Master]
      , cdKeywords = setFromList [Restricted]
      }

innerStrength :: CardDef
innerStrength =
  (attachment "22009" "Inner Strength" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Master]
    , cdLimitOnePerHost = True
    }

strengthAndCourage :: CardDef
strengthAndCourage =
  (attachment "22033" "Strength and Courage" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Master]
    , cdLimitOnePerHost = True
    }

powerOfCommand :: CardDef
powerOfCommand =
  (attachment "22055" "Power of Command" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Master]
    , cdLimitOnePerHost = True
    }

wellPreserved :: CardDef
wellPreserved =
  (attachment "22085" "Well Preserved" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Master]
    , cdLimitOnePerHost = True
    }

goldenBelt :: CardDef
goldenBelt =
  (attachment "22060" "Golden Belt" Tactics (StaticCost 0))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

sarumansStaff :: CardDef
sarumansStaff =
  unique
    (attachment "22063" "Saruman's Staff" Neutral (StaticCost 2))
      { cdCardTraits = setFromList [Artifact, Item, Staff]
      , cdKeywords = setFromList [Restricted]
      }

stoneOfElostirion :: CardDef
stoneOfElostirion =
  unique
    (attachment "22084" "Stone of Elostirion" Lore (StaticCost 1))
      { cdCardTraits = setFromList [Artifact, Item]
      , cdKeywords = setFromList [GuardedBy [LocationType], Restricted]
      }

songOfKings :: CardDef
songOfKings =
  (attachment "02010" "Song of Kings" Neutral (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

dunedainWarning :: CardDef
dunedainWarning =
  (attachment "02026" "Dúnedain Warning" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Signal]
    }

bornAloft :: CardDef
bornAloft =
  (attachment "02029" "Born Aloft" Tactics (StaticCost 0))
    { cdCardTraits = setFromList [Condition]
    }

norAmIAStranger :: CardDef
norAmIAStranger =
  (attachment "02031" "Nor am I a Stranger" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Title]
    }

aBurningBrand :: CardDef
aBurningBrand =
  (attachment "02033" "A Burning Brand" Lore (StaticCost 2))
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Restricted]
    }

songOfWisdom :: CardDef
songOfWisdom =
  (attachment "02034" "Song of Wisdom" Neutral (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

dunedainQuest :: CardDef
dunedainQuest =
  (attachment "02051" "Dúnedain Quest" Leadership (StaticCost 2))
    { cdCardTraits = setFromList [Signal]
    }

songOfTravel :: CardDef
songOfTravel =
  (attachment "02081" "Song of Travel" Neutral (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

dunedainCache :: CardDef
dunedainCache =
  (attachment "02097" "Dúnedain Cache" Leadership (StaticCost 2))
    { cdCardTraits = setFromList [Item]
    }

songOfMocking :: CardDef
songOfMocking =
  (attachment "02099" "Song of Mocking" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

fastHitch :: CardDef
fastHitch =
  (attachment "02103" "Fast Hitch" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Skill]
    }

songOfBattle :: CardDef
songOfBattle =
  (attachment "02104" "Song of Battle" Neutral (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

dunedainSignal :: CardDef
dunedainSignal =
  (attachment "02117" "Dúnedain Signal" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Signal]
    }

supportOfTheEagles :: CardDef
supportOfTheEagles =
  (attachment "02120" "Support of the Eagles" Tactics (StaticCost 3))
    { cdCardTraits = setFromList [Boon]
    }

dwarrowdelfAxe :: CardDef
dwarrowdelfAxe =
  (attachment "03007" "Dwarrowdelf Axe" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

bootsFromErebor :: CardDef
bootsFromErebor =
  (attachment "03013" "Boots from Erebor" Neutral (StaticCost 0))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

keepingCount :: CardDef
keepingCount =
  (attachment "04005" "Keeping Count" Tactics (StaticCost 0))
    { cdLimitOnePerHost = True
    }

goodMeal :: CardDef
goodMeal = attachment "04010" "Good Meal" Neutral (StaticCost 0)

rivendellBlade :: CardDef
rivendellBlade =
  (attachment "04031" "Rivendell Blade" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

songOfEarendil :: CardDef
songOfEarendil =
  (attachment "04034" "Song of Eärendil" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    }

swordThatWasBroken :: CardDef
swordThatWasBroken =
  unique
    (attachment "04055" "Sword that was Broken" Leadership (StaticCost 3))
      { cdCardTraits = setFromList [Artifact]
      }

rivendellBow :: CardDef
rivendellBow =
  (attachment "04057" "Rivendell Bow" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdLimitOnePerHost = True
    }

resourceful :: CardDef
resourceful =
  (attachment "04062" "Resourceful" Neutral (StaticCost 4))
    { cdKeywords = setFromList [Secrecy 3]
    }

ringMail :: CardDef
ringMail =
  (attachment "04080" "Ring Mail" Tactics (StaticCost 2))
    { cdCardTraits = setFromList [Item, Armor]
    , cdKeywords = setFromList [Restricted]
    }

everMyHeartRises :: CardDef
everMyHeartRises =
  (attachment "04082" "Ever My Heart Rises" Spirit (StaticCost 0))
    { cdCardTraits = setFromList [Condition]
    }

loveOfTales :: CardDef
loveOfTales =
  (attachment "04085" "Love of Tales" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Condition]
    , cdLimitOnePerHost = True
    }

lightOfValinor :: CardDef
lightOfValinor =
  unique
    (attachment "04107" "Light of Valinor" Spirit (StaticCost 1))
      { cdCardTraits = setFromList [Condition]
      }

asfaloth :: CardDef
asfaloth =
  unique
    (attachment "04110" "Asfaloth" Lore (StaticCost 2))
      { cdCardTraits = setFromList [Mount]
      }

hardyLeadership :: CardDef
hardyLeadership = unique $ attachment "04130" "Hardy Leadership" Leadership (StaticCost 2)

miruvor :: CardDef
miruvor =
  (attachment "04133" "Miruvor" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    }

bloodOfNumenor :: CardDef
bloodOfNumenor =
  (attachment "05013" "Blood of Númenor" Spirit (StaticCost 0))
    { cdCardTraits = setFromList [Condition]
    }

gondorianFire :: CardDef
gondorianFire =
  (attachment "06085" "Gondorian Fire" Tactics (StaticCost 0))
    { cdCardTraits = setFromList [Gondor]
    }

mirrorOfGaladriel :: CardDef
mirrorOfGaladriel =
  (attachment "08118" "Mirror of Galadriel" Spirit (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    }

ringOfBarahir :: CardDef
ringOfBarahir =
  (attachment "06007" "Ring of Barahir" Spirit (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item, Ring]
    }

mightyProwess :: CardDef
mightyProwess =
  (attachment "06035" "Mighty Prowess" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Skill]
    , cdLimitOnePerHost = True
    }

lordOfMorthond :: CardDef
lordOfMorthond =
  (attachment "06058" "Lord of Morthond" Leadership (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Title]
    }

bookOfEldacar :: CardDef
bookOfEldacar =
  (attachment "06059" "Book of Eldacar" Tactics (StaticCost 4))
    { cdUnique = True
    , cdCardTraits = setFromList [Record]
    }

swordOfMorthond :: CardDef
swordOfMorthond =
  (attachment "06082" "Sword of Morthond" Leadership (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Item, Weapon]
    }

mapOfEarnil :: CardDef
mapOfEarnil =
  (attachment "06087" "Map of Earnil" Spirit (StaticCost 4))
    { cdUnique = True
    , cdCardTraits = setFromList [Record]
    }

rangerBow :: CardDef
rangerBow =
  (attachment "06088" "Ranger Bow" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Weapon]
    , cdKeywords = setFromList [Restricted]
    }

palantir :: CardDef
palantir =
  (attachment "06090" "Palantir" Neutral (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    }

tomeOfAtanatar :: CardDef
tomeOfAtanatar =
  (attachment "06109" "Tome of Atanatar" Leadership (StaticCost 4))
    { cdUnique = True
    , cdCardTraits = setFromList [Record]
    }

poisonedStakes :: CardDef
poisonedStakes =
  (attachment "06115" "Poisoned Stakes" Lore (StaticCost 2))
    { cdCardTraits = setFromList [Trap]
    }

scrollOfIsildur :: CardDef
scrollOfIsildur =
  (attachment "06142" "Scroll of Isildur" Lore (StaticCost 4))
    { cdUnique = True
    , cdCardTraits = setFromList [Record]
    }

keysOfOrthanc :: CardDef
keysOfOrthanc =
  (attachment "07010" "Keys of Orthanc" Neutral (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Item]
    }

silverLamp :: CardDef
silverLamp =
  (attachment "07009" "Silver Lamp" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Item]
    }

elvenMail :: CardDef
elvenMail =
  (attachment "08029" "Elven Mail" Tactics (StaticCost 2))
    { cdCardTraits = setFromList [Armor, Item]
    , cdKeywords = setFromList [Restricted]
    }

cloakOfLorien :: CardDef
cloakOfLorien =
  (attachment "08120" "Cloak of Lórien" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

leafBrooch :: CardDef
leafBrooch =
  (attachment "08034" "Leaf Brooch" Neutral (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

defenderOfTheWest :: CardDef
defenderOfTheWest =
  (attachment "08093" "Defender of the West" Neutral (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Title]
    }

theFallOfGilGalad :: CardDef
theFallOfGilGalad =
  (attachment "08007" "The Fall of Gil-Galad" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Song]
    , cdDeckLimit = 1
    }

wardenOfArnor :: CardDef
wardenOfArnor =
  (attachment "08031" "Warden of Arnor" Spirit (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Title]
    }

theDaysRising :: CardDef
theDaysRising =
  (attachment "08139" "The Day's Rising" Leadership (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Song]
    }

theLongDefeat :: CardDef
theLongDefeat =
  (attachment "10122" "The Long Defeat" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Condition]
    , cdLimitOnePerHost = True
    }

favorOfTheValar :: CardDef
favorOfTheValar =
  (attachment "10124" "Favor of the Valar" Neutral (StaticCost 3))
    { cdCardTraits = setFromList [Condition]
    , cdLimitOnePerHost = True
    }

swordOfNumenor :: CardDef
swordOfNumenor =
  (attachment "10142" "Sword of Númenor" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

weatherStainedCloak :: CardDef
weatherStainedCloak =
  (attachment "10148" "Weather-stained Cloak" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

swordThain :: CardDef
swordThain =
  (attachment "10149" "Sword-thain" Neutral (StaticCost 4))
    { cdUnique = True
    , cdCardTraits = setFromList [Title]
    }

rangerProvisions :: CardDef
rangerProvisions =
  (attachment "10055" "Ranger Provisions" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

steedOfImladris :: CardDef
steedOfImladris =
  (attachment "10059" "Steed of Imladris" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Mount]
    , cdKeywords = setFromList [Restricted]
    }

elvenSpear :: CardDef
elvenSpear =
  (attachment "10087" "Elven Spear" Tactics (StaticCost 0))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

silverHarp :: CardDef
silverHarp =
  (attachment "10090" "Silver Harp" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Item, Instrument]
    , cdKeywords = setFromList [Restricted]
    }

elfFriend :: CardDef
elfFriend =
  (attachment "10093" "Elf-friend" Neutral (StaticCost 1))
    { cdCardTraits = setFromList [Title]
    }

heirOfValandil :: CardDef
heirOfValandil =
  unique
    (attachment "09010" "Heir of Valandil" Leadership (StaticCost 2))
      { cdCardTraits = setFromList [Title]
      }

athelas :: CardDef
athelas =
  (attachment "09011" "Athelas" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    }

secretVigil :: CardDef
secretVigil =
  (attachment "09012" "Secret Vigil" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Condition]
    , cdLimitOnePerHost = True
    }

starBrooch :: CardDef
starBrooch =
  (attachment "09013" "Star Brooch" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

ravenWingedHelm :: CardDef
ravenWingedHelm =
  (attachment "10005" "Raven-winged Helm" Tactics (StaticCost 2))
    { cdCardTraits = setFromList [Item, Armor]
    , cdLimitOnePerHost = True
    }

hobbitPony :: CardDef
hobbitPony =
  (attachment "10007" "Hobbit Pony" Spirit (StaticCost 0))
    { cdCardTraits = setFromList [Mount]
    , cdKeywords = setFromList [Restricted]
    }

grapplingHook :: CardDef
grapplingHook =
  (attachment "11005" "Grappling Hook" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    }

marinersCompass :: CardDef
marinersCompass =
  (attachment "11008" "Mariner's Compass" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    }

explorersAlmanac :: CardDef
explorersAlmanac =
  (attachment "11011" "Explorer's Almanac" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Item]
    }

toTheSeaToTheSea :: CardDef
toTheSeaToTheSea =
  unique
    (attachment "11014" "To the Sea, to the Sea!" Spirit (StaticCost 1))
      { cdCardTraits = setFromList [Song]
      }

narya :: CardDef
narya =
  unique
    (attachment "11015" "Narya" Neutral (StaticCost 2))
      { cdCardTraits = setFromList [Ring, Artifact]
      }

rodOfTheSteward :: CardDef
rodOfTheSteward =
  (attachment "12002" "Rod of the Steward" Leadership (StaticCost 0))
    { cdUnique = True
    , cdCardTraits = setFromList [Item]
    }

inServiceOfTheSteward :: CardDef
inServiceOfTheSteward =
  (attachment "12003" "In Service of the Steward" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Title]
    }

raimentOfWar :: CardDef
raimentOfWar =
  (attachment "12034" "Raiment of War" Tactics (StaticCost 2))
    { cdCardTraits = setFromList [Item, Armor, Weapon]
    , cdKeywords = setFromList [Restricted]
    , cdRestrictedSlots = 2
    }

guardedCeaselessly :: CardDef
guardedCeaselessly =
  (attachment "12038" "Guarded Ceaselessly" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Condition]
    }

armoredDestrier :: CardDef
armoredDestrier =
  (attachment "12059" "Armored Destrier" Leadership (StaticCost 2))
    { cdCardTraits = setFromList [Mount]
    , cdKeywords = setFromList [Restricted]
    }

entanglingNets :: CardDef
entanglingNets =
  (attachment "12064" "Entangling Nets" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Trap]
    }

dunedainRemedy :: CardDef
dunedainRemedy =
  (attachment "12084" "Dúnedain Remedy" Leadership (StaticCost 0))
    { cdCardTraits = setFromList [Signal]
    }

hithlain :: CardDef
hithlain =
  (attachment "12088" "Hithlain" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

strider :: CardDef
strider =
  unique
    (attachment "12091" "Strider" Neutral (StaticCost 1))
      { cdCardTraits = setFromList [Title]
      }

runeMaster :: CardDef
runeMaster =
  unique
    (attachment "12110" "Rune-master" Leadership (StaticCost 1))
      { cdCardTraits = setFromList [Title]
      }

vigilantGuard :: CardDef
vigilantGuard =
  (attachment "12113" "Vigilant Guard" Tactics (StaticCost 3))
    { cdCardTraits = setFromList [Skill]
    , cdLimitOnePerHost = True
    }

windfola :: CardDef
windfola =
  unique
    (attachment "12116" "Windfola" Spirit (StaticCost 1))
      { cdCardTraits = setFromList [Mount]
      , cdKeywords = setFromList [Restricted]
      }

rangerSpear :: CardDef
rangerSpear =
  (attachment "12145" "Ranger Spear" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

princeOfDolAmroth :: CardDef
princeOfDolAmroth =
  unique
    (attachment "12146" "Prince of Dol Amroth" Neutral (StaticCost 3))
      { cdCardTraits = setFromList [Title]
      }

-- * The Dream-chaser Campaign Expansion: boons, each removed from the game as it leaves
-- play, their Upgraded sides once XP has upgraded them

dccBoon :: CardCode -> Name -> [Trait] -> CardDef
dccBoon code name traits = (attachment code name Neutral (StaticCost 0)) {cdCardTraits = setFromList (Boon : traits)}

upgradedSide :: CardDef -> CardDef
upgradedSide def = def {cdCardCode = CardCode (unCardCode (cdCardCode def) <> "U"), cdCardTraits = insertSet Upgraded (cdCardTraits def)}

-- | A Modification boon, attached to a ship-objective as the campaign card's setup says.
armorPlating :: CardDef
armorPlating = dccBoon "DCC172" "Armor Plating" [Modification, Armor]

armorPlatingUpgraded :: CardDef
armorPlatingUpgraded = upgradedSide armorPlating

ballista :: CardDef
ballista = dccBoon "DCC173" "Ballista" [Modification, Weapon]

ballistaUpgraded :: CardDef
ballistaUpgraded = upgradedSide ballista

enlargedQuarters :: CardDef
enlargedQuarters = dccBoon "DCC174" "Enlarged Quarters" [Modification]

enlargedQuartersUpgraded :: CardDef
enlargedQuartersUpgraded = upgradedSide enlargedQuarters

extraSails :: CardDef
extraSails = dccBoon "DCC175" "Extra Sails" [Modification]

extraSailsUpgraded :: CardDef
extraSailsUpgraded = upgradedSide extraSails

spyglass :: CardDef
spyglass = dccBoon "DCC180" "Spyglass" [Item]

spyglassUpgraded :: CardDef
spyglassUpgraded = upgradedSide spyglass

blackKey :: CardDef
blackKey = unique $ dccBoon "DCC183" "Black Key" [Item, Artifact]

blackKeyUpgraded :: CardDef
blackKeyUpgraded = upgradedSide blackKey

-- | Permanent: attached to the hero the Campaign Log records it on.
parryingCutlass :: CardDef
parryingCutlass = (dccBoon "DCC189" "Parrying Cutlass" [Item, Weapon]) {cdKeywords = setFromList [Restricted]}

parryingCutlassUpgraded :: CardDef
parryingCutlassUpgraded = upgradedSide parryingCutlass

throwingAxe :: CardDef
throwingAxe = (dccBoon "DCC190" "Throwing Axe" [Item, Weapon]) {cdKeywords = setFromList [Restricted]}

throwingAxeUpgraded :: CardDef
throwingAxeUpgraded = upgradedSide throwingAxe

cirdansBlessing :: CardCode -> Sphere -> CardDef
cirdansBlessing code sphere = (attachment code "Círdan's Blessing" sphere (StaticCost 1)) {cdCardTraits = setFromList [Condition, Boon]}

cirdansBlessingLeadership :: CardDef
cirdansBlessingLeadership = cirdansBlessing "DCC205" Leadership

cirdansBlessingTactics :: CardDef
cirdansBlessingTactics = cirdansBlessing "DCC206" Tactics

cirdansBlessingSpirit :: CardDef
cirdansBlessingSpirit = cirdansBlessing "DCC207" Spirit

cirdansBlessingLore :: CardDef
cirdansBlessingLore = cirdansBlessing "DCC208" Lore
-- The Angmar Awakened campaign's boons

iarionsPendant :: CardDef
iarionsPendant =
  (unique $ attachment "AAC157" "Iârion's Pendant" Neutral (StaticCost 1))
    { cdCardTraits = setFromList [Artifact, Item, Boon]
    , cdVictoryPoints = Just 1
    }

-- | A Permanent condition boon Mysterious Omens offers, attached to a hero.
omenBoon :: CardCode -> Name -> CardDef
omenBoon code name = (attachment code name Neutral (StaticCost 0)) {cdCardTraits = setFromList [Condition, Boon]}

cunning :: CardDef
cunning = omenBoon "AAC161" "Cunning"

hardy :: CardDef
hardy = omenBoon "AAC162" "Hardy"

resolute :: CardDef
resolute = omenBoon "AAC163" "Resolute"

ruthless :: CardDef
ruthless = omenBoon "AAC164" "Ruthless"

stalwart :: CardDef
stalwart = omenBoon "AAC165" "Stalwart"

amarthiulsCourage :: CardDef
amarthiulsCourage =
  (unique $ attachment "AAC167" "Amarthiúl's Courage" Neutral (StaticCost 0))
    { cdCardTraits = setFromList [Condition, Boon]
    }

restrictedBoon :: CardCode -> Name -> Int -> [Trait] -> CardDef
restrictedBoon code name cost traits =
  (unique $ attachment code name Neutral (StaticCost cost))
    { cdCardTraits = setFromList (Boon : traits)
    , cdKeywords = setFromList [Restricted]
    }

chieftainsBrooch :: CardDef
chieftainsBrooch = restrictedBoon "AAC177" "Chieftain's Brooch" 2 [Artifact, Item]

mailOfEarnur :: CardDef
mailOfEarnur = restrictedBoon "AAC180" "Mail of Eärnur" 1 [Artifact, Armor, Item]

silverthorn :: CardDef
silverthorn = restrictedBoon "AAC181" "Silverthorn" 0 [Artifact, Weapon, Item]

daechanarsBrand :: CardDef
daechanarsBrand = restrictedBoon "AAC185" "Daechanar's Brand" 0 [Artifact, Item, Weapon]

heirloomOfIarchon :: CardDef
heirloomOfIarchon = restrictedBoon "AAC186" "Heirloom of Iârchon" 0 [Artifact, Item]

ordersFromAngmar :: CardDef
ordersFromAngmar = restrictedBoon "AAC187" "Orders from Angmar" 0 [Item]

raimentOfTheSecondAge :: CardDef
raimentOfTheSecondAge = (restrictedBoon "AAC191" "Raiment of the Second Age" 3 [Armor, Weapon, Item]) {cdUnique = False}

dwarvenShield :: CardDef
dwarvenShield =
  (attachment "16010" "Dwarven Shield" Leadership (StaticCost 2))
    { cdCardTraits = setFromList [Item, Armor]
    , cdKeywords = setFromList [Restricted]
    }

mirkwoodLongKnife :: CardDef
mirkwoodLongKnife =
  (attachment "16011" "Mirkwood Long-knife" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

theRoadGoesEverOn :: CardDef
theRoadGoesEverOn =
  (attachment "16012" "The Road Goes Ever On" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Song]
    }

kahlielsHeaddress :: CardDef
kahlielsHeaddress =
  (attachment "17010" "Kahliel's Headdress" Neutral (StaticCost 3))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    , cdKeywords = setFromList [Restricted]
    }

steedOfTheNorth :: CardDef
steedOfTheNorth =
  (attachment "17030" "Steed of the North" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Mount]
    , cdKeywords = setFromList [Restricted]
    }

mightyWarrior :: CardDef
mightyWarrior =
  (attachment "17031" "Mighty Warrior" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Skill]
    , cdLimitOnePerHost = True
    }

haradrimSpear :: CardDef
haradrimSpear =
  (attachment "17056" "Haradrim Spear" Leadership (StaticCost 2))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

firesideSong :: CardDef
firesideSong =
  (attachment "17061" "Fireside Song" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Song]
    , cdLimitOnePerHost = True
    }

doughtyRanger :: CardDef
doughtyRanger =
  (attachment "17063" "Doughty Ranger" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Skill]
    , cdLimitOnePerHost = True
    }

songOfHope :: CardDef
songOfHope =
  (attachment "17082" "Song of Hope" Leadership (StaticCost 0))
    { cdCardTraits = setFromList [Song]
    , cdLimitOnePerHost = True
    }

fearlessScout :: CardDef
fearlessScout =
  (attachment "17086" "Fearless Scout" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Skill]
    , cdLimitOnePerHost = True
    }

dunedainPipe :: CardDef
dunedainPipe =
  (attachment "17088" "Dúnedain Pipe" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Item, Pipe]
    , cdLimitOnePerHost = True
    }

diligentNoble :: CardDef
diligentNoble =
  (attachment "17111" "Diligent Noble" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Skill]
    , cdLimitOnePerHost = True
    }

legacyBlade :: CardDef
legacyBlade =
  (attachment "17117" "Legacy Blade" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

followed :: CardDef
followed =
  (attachment "17139" "Followed" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Condition, Trap]
    }

sting :: CardDef
sting =
  unique
    (attachment "19117" "Sting" Tactics (StaticCost 0))
      { cdCardTraits = setFromList [Artifact, Item, Weapon]
      , cdKeywords = setFromList [GuardedBy [EnemyType], Restricted]
      }

roundShield :: CardDef
roundShield =
  (attachment "19118" "Round Shield" Tactics (StaticCost 0))
    { cdCardTraits = setFromList [Item, Armor]
    , cdKeywords = setFromList [Restricted]
    }

armorOfErebor :: CardDef
armorOfErebor =
  (attachment "19120" "Armor of Erebor" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Item, Armor]
    , cdKeywords = setFromList [Restricted]
    }

mithrilShirt :: CardDef
mithrilShirt =
  unique
    (attachment "19152" "Mithril Shirt" Spirit (StaticCost 1))
      { cdCardTraits = setFromList [Artifact, Item, Armor]
      , cdKeywords = setFromList [GuardedBy [LocationType]]
      }

radagastsStaff :: CardDef
radagastsStaff =
  unique
    (attachment "19154" "Radagast's Staff" Neutral (StaticCost 2))
      { cdCardTraits = setFromList [Artifact, Item, Staff]
      }

hauberkOfMail :: CardDef
hauberkOfMail =
  (attachment "18007" "Hauberk of Mail" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Item, Armor]
    , cdLimitOnePerHost = True
    }

kingOfDale :: CardDef
kingOfDale =
  unique
    (attachment "18008" "King of Dale" Spirit (StaticCost 2))
      { cdCardTraits = setFromList [Title]
      }

bowOfYew :: CardDef
bowOfYew =
  (attachment "18009" "Bow of Yew" Tactics (StaticCost 0))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

mapOfRhovanion :: CardDef
mapOfRhovanion =
  (attachment "18010" "Map of Rhovanion" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

necklaceOfGirion :: CardDef
necklaceOfGirion =
  unique
    (attachment "18011" "Necklace of Girion" Neutral (StaticCost 1))
      { cdCardTraits = setFromList [Artifact, Item]
      , cdKeywords = setFromList [GuardedBy [EnemyType, LocationType]]
      }

squiresHelm :: CardDef
squiresHelm =
  (attachment "19003" "Squire's Helm" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Item, Armor]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

woodmensClearing :: CardDef
woodmensClearing =
  (attachment "19006" "Woodmen's Clearing" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Condition]
    , cdLimitOnePerHost = True
    }

greyCloak :: CardDef
greyCloak =
  (attachment "19009" "Grey Cloak" Spirit (StaticCost 0))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

theArkenstone :: CardDef
theArkenstone =
  unique
    (attachment "19010" "The Arkenstone" Neutral (StaticCost 1))
      { cdCardTraits = setFromList [Artifact, Item]
      , cdKeywords = setFromList [GuardedBy [EnemyType, LocationType]]
      }

ancestralArmor :: CardDef
ancestralArmor =
  (attachment "19028" "Ancestral Armor" Leadership (StaticCost 4))
    { cdCardTraits = setFromList [Item, Armor]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

glamdring :: CardDef
glamdring =
  unique
    (attachment "19029" "Glamdring" Lore (StaticCost 1))
      { cdCardTraits = setFromList [Artifact, Item, Weapon]
      , cdKeywords = setFromList [GuardedBy [EnemyType, LocationType], Restricted]
      }

woodmensPath :: CardDef
woodmensPath =
  (attachment "19030" "Woodmen's Path" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Condition]
    }

theElvenking :: CardDef
theElvenking =
  unique
    (attachment "19057" "The Elvenking" Leadership (StaticCost 1))
      { cdCardTraits = setFromList [Title]
      }

orcrist :: CardDef
orcrist =
  unique
    (attachment "19058" "Orcrist" Leadership (StaticCost 1))
      { cdCardTraits = setFromList [Artifact, Item, Weapon]
      , cdKeywords = setFromList [GuardedBy [EnemyType, LocationType], Restricted]
      }

leatherBoots :: CardDef
leatherBoots =
  (attachment "19060" "Leather Boots" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Item]
    , cdLimitOnePerHost = True
    }

outmatched :: CardDef
outmatched =
  (attachment "19062" "Outmatched" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Condition, Trap]
    }

warriorSword :: CardDef
warriorSword =
  (attachment "19089" "Warrior Sword" Tactics (StaticCost 1))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

valiantDetermination :: CardDef
valiantDetermination =
  (attachment "19092" "Valiant Determination" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Condition]
    }

ringOfThror :: CardDef
ringOfThror =
  unique
    (attachment "19093" "Ring of Thrór" Neutral (StaticCost 1))
      { cdCardTraits = setFromList [Artifact, Ring]
      , cdKeywords = setFromList [GuardedBy [EnemyType, LocationType]]
      }

putOffPursuit :: CardDef
putOffPursuit =
  (attachment "21008" "Put Off Pursuit" Lore (StaticCost 1))
    { cdCardTraits = setFromList [Condition]
    , cdLimitOnePerHost = True
    }

durinsAxeTactics :: CardDef
durinsAxeTactics =
  unique
    (attachment "22006" "Durin's Axe" Tactics (StaticCost 1))
      { cdCardTraits = setFromList [Artifact, Item, Weapon]
      , cdKeywords = setFromList [GuardedBy [EnemyType], Restricted]
      }

silverCirclet :: CardDef
silverCirclet =
  (attachment "22008" "Silver Circlet" Spirit (StaticCost 2))
    { cdCardTraits = setFromList [Item]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

redBookOfWestmarch :: CardDef
redBookOfWestmarch =
  unique
    (attachment "22110" "Red Book of Westmarch" Leadership (StaticCost 3))
      { cdCardTraits = setFromList [Artifact, Item]
      , cdKeywords = setFromList [Restricted]
      }

songOfHealing :: CardDef
songOfHealing =
  (attachment "22112" "Song of Healing" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Song]
    , cdLimitOnePerHost = True
    }

sparePipe :: CardDef
sparePipe =
  (attachment "22115" "Spare Pipe" Spirit (StaticCost 1))
    { cdCardTraits = setFromList [Item, Pipe]
    }

shiningShield :: CardDef
shiningShield =
  (attachment "22138" "Shining Shield" Leadership (StaticCost 1))
    { cdCardTraits = setFromList [Armor, Item]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }

keenLongbow :: CardDef
keenLongbow =
  (attachment "22031" "Keen Longbow" Lore (StaticCost 2))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    , cdLimitOnePerHost = True
    }
tirelessThoroughbred :: CardDef
tirelessThoroughbred =
  (attachment "22143" "Tireless Thoroughbred" Tactics (StaticCost 2))
    { cdCardTraits = setFromList [Mount]
    , cdKeywords = setFromList [Restricted]
    }

thorongil :: CardDef
thorongil =
  unique
    (attachment "22146" "Thorongil" Neutral (StaticCost 3))
      { cdCardTraits = setFromList [Title]
      }

spareHoodAndCloak :: CardDef
spareHoodAndCloak =
  (attachment "131012" "Spare Hood and Cloak" Spirit (StaticCost 0))
    { cdCardTraits = setFromList [Item]
    }

greatYewBow :: CardDef
greatYewBow =
  (attachment "132014" "Great Yew Bow" Tactics (StaticCost 2))
    { cdCardTraits = setFromList [Item, Weapon]
    , cdKeywords = setFromList [Restricted]
    }

blackArrow :: CardDef
blackArrow =
  (attachment "132015" "Black Arrow" Tactics (StaticCost 0))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    , cdVictoryPoints = Just 1
    , cdDeckLimit = 1
    }

throrsKey :: CardDef
throrsKey =
  (attachment "132016" "Thrór's Key" Spirit (StaticCost 1))
    { cdUnique = True
    , cdCardTraits = setFromList [Artifact, Item]
    }

expertTreasureHunter :: CardDef
expertTreasureHunter =
  (attachment "132017" "Expert Treasure-hunter" Lore (StaticCost 0))
    { cdCardTraits = setFromList [Skill]
    , cdLimitOnePerHost = True
    }

-- * The Hobbit saga's treasures

{- | A treasure: a player card discovered in the scenario its set icon names, then added to
its discoverer's deck, 1 copy, in a later scenario whose setup lists that set (The Hobbit:
Over Hill and Under Hill rules insert, "Treasure"). No deck is built with one: the campaign
adds those discovered (see 'LordOfTheRings.Campaign.seatPoolCards'). Each reads "Attach to".
-}
treasure :: CardCode -> Name -> EncounterSet -> [Trait] -> CardDef
treasure code name set traits =
  (attachment code name Neutral (StaticCost 0))
    { cdUnique = True
    , cdCardTraits = setFromList traits
    , cdTreasure = True
    , cdSetIcon = Just set
    , cdDeckLimit = 1
    }

stingTreasure :: CardDef
stingTreasure =
  (treasure "131020" "Sting" WeMustAwayEreBreakOfDay [Artifact, Item, Weapon])
    { cdKeywords = setFromList [Restricted]
    }

orcristTreasure :: CardDef
orcristTreasure =
  (treasure "131021" "Orcrist" WeMustAwayEreBreakOfDay [Artifact, Item, Weapon])
    { cdKeywords = setFromList [Restricted]
    }

glamdringTreasure :: CardDef
glamdringTreasure =
  (treasure "131022" "Glamdring" WeMustAwayEreBreakOfDay [Artifact, Item, Weapon])
    { cdKeywords = setFromList [Restricted]
    }

-- | Discovered in Flies and Spiders, whose setup also lets each player shuffle a copy in.
bilbosMagicRing :: CardDef
bilbosMagicRing = treasure "132019" "Bilbo's Magic Ring" FliesAndSpiders [Artifact, Item, Ring]

theArkenstoneTreasure :: CardDef
theArkenstoneTreasure = treasure "132020" "The Arkenstone" TheLonelyMountain [Artifact, Erebor, Item]

mithrilShirtTreasure :: CardDef
mithrilShirtTreasure = treasure "132021" "Mithril Shirt" TheLonelyMountain [Artifact, Erebor, Item]

throrsBattleAxe :: CardDef
throrsBattleAxe =
  (treasure "132022" "Thrór's Battle Axe" TheLonelyMountain [Artifact, Erebor, Item])
    { cdKeywords = setFromList [Restricted]
    }

throrsHuntingBow :: CardDef
throrsHuntingBow =
  (treasure "132023" "Thrór's Hunting Bow" TheLonelyMountain [Artifact, Erebor, Item])
    { cdKeywords = setFromList [Restricted]
    }

throrsGoldenCup :: CardDef
throrsGoldenCup =
  (treasure "132024" "Thrór's Golden Cup" TheLonelyMountain [Artifact, Erebor, Item])
    { cdVictoryPoints = Just 3
    }
