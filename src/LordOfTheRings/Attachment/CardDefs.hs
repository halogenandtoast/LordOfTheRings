module LordOfTheRings.Attachment.CardDefs (module LordOfTheRings.Attachment.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere
import LordOfTheRings.Trait

allAttachmentCards :: Map CardCode CardDef
allAttachmentCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ stewardOfGondor
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
      ]

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
