module LordOfTheRings.Hero.CardDefs (module LordOfTheRings.Hero.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.Code
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere
import LordOfTheRings.Trait

allHeroCards :: Map CardCode CardDef
allHeroCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ aragorn
      , theodred
      , gloin
      , gimli
      , legolas
      , thalin
      , eowyn
      , eleanor
      , dunhere
      , denethor
      , glorfindel
      , beravor
      , princeImrahil
      , dainIronfoot
      , bifur
      , elrond
      , beregond
      , boromir
      , faramir
      , eomer
      , celeborn
      , haldirOfLorien
      , mablung
      , galadriel
      , nori
      , ori
      , hirgon
      , fastred
      , lothiriel
      , bilboBaggins
      , frodoBaggins
      , brandSonOfBain
      , boromirTactics
      , dwalin
      , elrohir
      , elladan
      , aragornLore
      , hama
      , glorfindelSpirit
      , arwenUndomiel
      , eowynTactics
      , hirluinTheFair
      , mirlonde
      , pippin
      , caldara
      , theoden
      , grima
      , idraen
      , erkenbrand
      , amarthiul
      , rossiel
      , dori
      , erestor
      , aragornTactics
      , halbarad
      , merrySpirit
      , cirdanTheShipwright
      , galdorOfTheHavensHero
      , denethorLeadership
      , lanwyn
      , elfhelmLeadership
      , argalad
      , naasiyah
      , princeImrahilTactics
      , gimliLeadership
      , legolasSpirit
      , kahliel
      , thurindir
      , folcoBoffin
      , quickbeam
      , bilboBagginsTactics
      , radagast
      , brandSonOfBainLeadership
      , bardSonOfBrand
      , grimbeornTheOld
      , haldan
      , thranduil
      , dainIronfootSpirit
      , smeagol
      ]

aragorn :: CardDef
aragorn =
  (hero "01001" "Aragorn" Leadership 12 2 3 2 5)
    { cdCardTraits = setFromList [Dunedain, Noble, Ranger]
    , cdKeywords = setFromList [Sentinel]
    }

theodred :: CardDef
theodred =
  (hero "01002" "Théodred" Leadership 8 1 2 1 4)
    { cdCardTraits = setFromList [Noble, Rohan, Warrior]
    }

gloin :: CardDef
gloin =
  (hero "01003" "Glóin" Leadership 9 2 2 1 4)
    { cdCardTraits = setFromList [Dwarf, Noble]
    }

gimli :: CardDef
gimli =
  (hero "01004" "Gimli" Tactics 11 2 2 2 5)
    { cdCardTraits = setFromList [Dwarf, Noble, Warrior]
    }

legolas :: CardDef
legolas =
  (hero "01005" "Legolas" Tactics 9 1 3 1 4)
    { cdCardTraits = setFromList [Noble, Silvan, Warrior]
    , cdKeywords = setFromList [Ranged]
    }

thalin :: CardDef
thalin =
  (hero "01006" "Thalin" Tactics 9 1 2 2 4)
    { cdCardTraits = setFromList [Dwarf, Warrior]
    }

eowyn :: CardDef
eowyn =
  (hero "01007" "Éowyn" Spirit 9 4 1 1 3)
    { cdCardTraits = setFromList [Noble, Rohan]
    }

eleanor :: CardDef
eleanor =
  (hero "01008" "Eleanor" Spirit 7 1 1 2 3)
    { cdCardTraits = setFromList [Gondor, Noble]
    }

dunhere :: CardDef
dunhere =
  (hero "01009" "Dúnhere" Spirit 8 1 2 1 4)
    { cdCardTraits = setFromList [Rohan, Warrior]
    }

denethor :: CardDef
denethor =
  (hero "01010" "Denethor" Lore 8 1 1 3 3)
    { cdCardTraits = setFromList [Gondor, Noble, Steward]
    }

glorfindel :: CardDef
glorfindel =
  (hero "01011" "Glorfindel" Lore 12 3 3 1 5)
    { cdCardTraits = setFromList [Noble, Noldor, Warrior]
    }

beravor :: CardDef
beravor =
  (hero "01012" "Beravor" Lore 10 2 2 2 4)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

princeImrahil :: CardDef
princeImrahil =
  (hero "02050" "Prince Imrahil" Leadership 11 2 3 2 4)
    { cdCardTraits = setFromList [Gondor, Noble]
    }

dainIronfoot :: CardDef
dainIronfoot =
  (hero "02116" "Dáin Ironfoot" Leadership 11 1 2 3 5)
    { cdCardTraits = setFromList [Dwarf, Noble]
    }

bifur :: CardDef
bifur =
  (hero "03002" "Bifur" Lore 7 2 1 2 3)
    { cdCardTraits = setFromList [Dwarf]
    }

elrond :: CardDef
elrond =
  (hero "04128" "Elrond" Lore 13 3 2 3 4)
    { cdCardTraits = setFromList [Noble, Noldor]
    }

beregond :: CardDef
beregond =
  (hero "05001" "Beregond" Tactics 10 0 1 4 4)
    { cdCardTraits = setFromList [Gondor, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

boromir :: CardDef
boromir =
  (hero "05002" "Boromir" Leadership 11 1 3 2 5)
    { cdCardTraits = setFromList [Gondor, Noble, Warrior]
    }

faramir :: CardDef
faramir =
  (hero "06081" "Faramir" Lore 11 2 2 2 5)
    { cdCardTraits = setFromList [Gondor, Noble, Ranger]
    , cdKeywords = setFromList [Ranged]
    }

eomer :: CardDef
eomer =
  (hero "07001" "Éomer" Tactics 10 1 3 2 4)
    { cdCardTraits = setFromList [Noble, Rohan, Warrior]
    }

celeborn :: CardDef
celeborn =
  (hero "08001" "Celeborn" Leadership 11 3 2 2 4)
    { cdCardTraits = setFromList [Noble, Silvan]
    }

haldirOfLorien :: CardDef
haldirOfLorien =
  (hero "08056" "Haldir of Lórien" Lore 9 2 3 1 3)
    { cdCardTraits = setFromList [Ranger, Scout, Silvan]
    , cdKeywords = setFromList [Ranged]
    }

mablung :: CardDef
mablung =
  (hero "08084" "Mablung" Tactics 10 2 2 2 4)
    { cdCardTraits = setFromList [Gondor, Ranger]
    }

galadriel :: CardDef
galadriel =
  (hero "08112" "Galadriel" Spirit 9 4 0 0 4)
    { cdCardTraits = setFromList [Noble, Noldor]
    }

nori :: CardDef
nori =
  (hero "131003" "Nori" Spirit 9 2 1 2 4)
    { cdCardTraits = setFromList [Dwarf]
    }

ori :: CardDef
ori =
  (hero "131004" "Ori" Lore 8 2 2 1 3)
    { cdCardTraits = setFromList [Dwarf]
    }

hirgon :: CardDef
hirgon =
  (hero "17055" "Hirgon" Tactics 9 2 2 1 4)
    { cdCardTraits = setFromList [Gondor, Scout]
    }

fastred :: CardDef
fastred =
  (hero "17081" "Fastred" Spirit 9 1 2 3 3)
    { cdCardTraits = setFromList [Rohan, Warrior]
    }

lothiriel :: CardDef
lothiriel =
  (hero "22027" "Lothíriel" Spirit 8 3 1 1 3)
    { cdCardTraits = setFromList [Gondor, Noble]
    }

bilboBaggins :: CardDef
bilboBaggins =
  (hero "02001" "Bilbo Baggins" Lore 9 1 1 2 2)
    { cdCardTraits = setFromList [Hobbit]
    }

frodoBaggins :: CardDef
frodoBaggins =
  (hero "02025" "Frodo Baggins" Spirit 7 2 1 2 2)
    { cdCardTraits = setFromList [Hobbit]
    }

brandSonOfBain :: CardDef
brandSonOfBain =
  (hero "02072" "Brand son of Bain" Tactics 10 2 3 2 3)
    { cdCardTraits = setFromList [Dale]
    , cdKeywords = setFromList [Ranged]
    }

boromirTactics :: CardDef
boromirTactics =
  (hero "02095" "Boromir" Tactics 11 1 3 2 5)
    { cdCardTraits = setFromList [Gondor, Noble, Warrior]
    }

dwalin :: CardDef
dwalin =
  (hero "03001" "Dwalin" Spirit 9 1 2 2 4)
    { cdCardTraits = setFromList [Dwarf]
    }

elrohir :: CardDef
elrohir =
  (hero "04001" "Elrohir" Leadership 10 2 2 1 4)
    { cdCardTraits = setFromList [Noldor, Noble, Ranger]
    }

elladan :: CardDef
elladan =
  (hero "04028" "Elladan" Tactics 10 2 1 2 4)
    { cdCardTraits = setFromList [Noldor, Noble, Ranger]
    }

aragornLore :: CardDef
aragornLore =
  (hero "04053" "Aragorn" Lore 12 2 3 2 5)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    , cdKeywords = setFromList [Sentinel]
    }

hama :: CardDef
hama =
  (hero "04076" "Háma" Tactics 9 1 3 1 4)
    { cdCardTraits = setFromList [Rohan, Warrior]
    }

glorfindelSpirit :: CardDef
glorfindelSpirit =
  (hero "04101" "Glorfindel" Spirit 5 3 3 1 5)
    { cdCardTraits = setFromList [Noldor, Noble, Warrior]
    }

arwenUndomiel :: CardDef
arwenUndomiel =
  (hero "10140" "Arwen Undómiel" Spirit 9 3 1 2 3)
    { cdCardTraits = setFromList [Noldor, Noble]
    }

eowynTactics :: CardDef
eowynTactics =
  (hero "145002" "Éowyn" Tactics 9 4 1 1 3)
    { cdCardTraits = setFromList [Rohan, Noble]
    }

hirluinTheFair :: CardDef
hirluinTheFair =
  (hero "06001" "Hirluin the Fair" Leadership 8 1 1 1 4)
    { cdCardTraits = setFromList [Outlands]
    , cdKeywords = setFromList [Ranged]
    }

mirlonde :: CardDef
mirlonde =
  (hero "06032" "Mirlonde" Lore 8 2 2 1 3)
    { cdCardTraits = setFromList [Silvan]
    }

pippin :: CardDef
pippin =
  (hero "06056" "Pippin" Spirit 6 2 1 1 2)
    { cdCardTraits = setFromList [Hobbit]
    }

caldara :: CardDef
caldara =
  (hero "06107" "Caldara" Spirit 8 2 1 2 3)
    { cdCardTraits = setFromList [Gondor]
    }

theoden :: CardDef
theoden =
  (hero "06134" "Théoden" Tactics 12 2 3 2 4)
    { cdCardTraits = setFromList [Noble, Rohan, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

grima :: CardDef
grima =
  (hero "07002" "Gríma" Lore 9 2 1 2 3)
    { cdCardTraits = setFromList [Isengard, Rohan]
    }

idraen :: CardDef
idraen =
  (hero "08025" "Idraen" Spirit 11 2 3 2 4)
    { cdCardTraits = setFromList [Dunedain, Ranger, Scout]
    }

erkenbrand :: CardDef
erkenbrand =
  (hero "08137" "Erkenbrand" Leadership 10 1 2 3 4)
    { cdCardTraits = setFromList [Rohan, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

amarthiul :: CardDef
amarthiul =
  (hero "10115" "Amarthiúl" Leadership 10 1 3 3 3)
    { cdCardTraits = setFromList [Dunedain, Ranger, Warrior]
    }

rossiel :: CardDef
rossiel =
  (hero "10028" "Rossiel" Lore 8 2 1 2 3)
    { cdCardTraits = setFromList [Silvan]
    }

dori :: CardDef
dori =
  (hero "10054" "Dori" Tactics 10 1 2 2 5)
    { cdCardTraits = setFromList [Dwarf]
    , cdKeywords = setFromList [Sentinel]
    }

erestor :: CardDef
erestor =
  (hero "10084" "Erestor" Lore 10 2 2 2 4)
    { cdCardTraits = setFromList [Noldor]
    }

aragornTactics :: CardDef
aragornTactics =
  (hero "09001" "Aragorn" Tactics 12 2 3 2 5)
    { cdCardTraits = setFromList [Dunedain, Ranger, Warrior]
    }

halbarad :: CardDef
halbarad =
  (hero "09002" "Halbarad" Leadership 10 2 2 2 4)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

merrySpirit :: CardDef
merrySpirit =
  (hero "10001" "Merry" Spirit 6 2 1 1 2)
    { cdCardTraits = setFromList [Hobbit]
    }

cirdanTheShipwright :: CardDef
cirdanTheShipwright =
  (hero "11001" "Círdan the Shipwright" Spirit 12 4 2 2 4)
    { cdCardTraits = setFromList [Noldor, Noble]
    }

galdorOfTheHavensHero :: CardDef
galdorOfTheHavensHero =
  (hero "11002" "Galdor of the Havens" Lore 9 2 2 1 4)
    { cdCardTraits = setFromList [Noldor]
    }

denethorLeadership :: CardDef
denethorLeadership =
  (hero "12001" "Denethor" Leadership 8 1 1 3 3)
    { cdCardTraits = setFromList [Gondor, Noble, Steward]
    }

lanwyn :: CardDef
lanwyn =
  (hero "12030" "Lanwyn" Spirit 9 2 3 1 3)
    { cdCardTraits = setFromList [Dale, Scout]
    , cdKeywords = setFromList [Ranged]
    }

elfhelmLeadership :: CardDef
elfhelmLeadership =
  (hero "12057" "Elfhelm" Leadership 10 2 2 2 4)
    { cdCardTraits = setFromList [Rohan, Scout, Warrior]
    }

argalad :: CardDef
argalad =
  (hero "12082" "Argalad" Lore 9 2 2 1 4)
    { cdCardTraits = setFromList [Silvan, Scout]
    , cdKeywords = setFromList [Ranged]
    }

naasiyah :: CardDef
naasiyah =
  (hero "12109" "Na'asiyah" Tactics 8 1 2 2 4)
    { cdCardTraits = setFromList [Corsair, Warrior]
    }

princeImrahilTactics :: CardDef
princeImrahilTactics =
  (hero "12138" "Prince Imrahil" Tactics 11 2 3 2 4)
    { cdCardTraits = setFromList [Gondor, Noble, Warrior]
    }

gimliLeadership :: CardDef
gimliLeadership =
  (hero "16001" "Gimli" Leadership 11 2 2 2 5)
    { cdCardTraits = setFromList [Dwarf, Noble, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

legolasSpirit :: CardDef
legolasSpirit =
  (hero "16002" "Legolas" Spirit 9 1 3 1 4)
    { cdCardTraits = setFromList [Silvan, Noble, Scout]
    , cdKeywords = setFromList [Ranged]
    }

kahliel :: CardDef
kahliel =
  (hero "17001" "Kahliel" Leadership 10 2 2 2 4)
    { cdCardTraits = setFromList [Harad, Noble]
    }

thurindir :: CardDef
thurindir =
  (hero "17028" "Thurindir" Lore 8 2 2 0 4)
    { cdCardTraits = setFromList [Dunedain, Ranger]
    }

folcoBoffin :: CardDef
folcoBoffin =
  (hero "17109" "Folco Boffin" Lore 7 2 2 0 2)
    { cdCardTraits = setFromList [Hobbit]
    }

-- | Unique as errata'd: the first printing left off the unique symbol.
quickbeam :: CardDef
quickbeam =
  (hero "17135" "Quickbeam" Tactics 12 2 4 1 5)
    { cdCardTraits = setFromList [Ent]
    }

bilboBagginsTactics :: CardDef
bilboBagginsTactics =
  (hero "19112" "Bilbo Baggins" Tactics 9 1 1 2 2)
    { cdCardTraits = setFromList [Hobbit, Burglar]
    }

radagast :: CardDef
radagast =
  (hero "19145" "Radagast" Lore 11 2 2 3 4)
    { cdCardTraits = setFromList [Istari]
    }

brandSonOfBainLeadership :: CardDef
brandSonOfBainLeadership =
  (hero "18001" "Brand son of Bain" Leadership 10 2 3 2 3)
    { cdCardTraits = setFromList [Dale, Noble]
    }

bardSonOfBrand :: CardDef
bardSonOfBrand =
  (hero "18002" "Bard son of Brand" Spirit 9 2 2 2 3)
    { cdCardTraits = setFromList [Dale, Noble]
    }

grimbeornTheOld :: CardDef
grimbeornTheOld =
  (hero "19001" "Grimbeorn the Old" Tactics 11 0 3 3 5)
    { cdCardTraits = setFromList [Beorning, Warrior]
    , cdKeywords = setFromList [Sentinel]
    }

haldan :: CardDef
haldan =
  (hero "19026" "Haldan" Lore 10 2 3 1 4)
    { cdCardTraits = setFromList [Woodman, Scout]
    }

thranduil :: CardDef
thranduil =
  (hero "19056" "Thranduil" Leadership 9 1 1 3 4)
    { cdCardTraits = setFromList [Silvan, Noble]
    }

dainIronfootSpirit :: CardDef
dainIronfootSpirit =
  (hero "19084" "Dáin Ironfoot" Spirit 11 1 2 3 5)
    { cdCardTraits = setFromList [Dwarf, Noble, Warrior]
    }

-- | His other side is the enemy Gollum ('LordOfTheRings.Enemy.CardDefs.gollumASitE').
smeagol :: CardDef
smeagol =
  (hero "21072" "Sméagol" Lore 3 2 2 2 3)
    { cdCardTraits = setFromList [Gollum]
    , cdOtherSide = Just "21073"
    }
