module LordOfTheRings.Objective.CardDefs (module LordOfTheRings.Objective.CardDefs) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.EncounterSet
import LordOfTheRings.Keyword
import LordOfTheRings.Prelude
import LordOfTheRings.Trait
import LordOfTheRings.Name (Name)

allObjectiveCards :: Map CardCode CardDef
allObjectiveCards =
  Map.fromList $
    map
      (\def -> (cdCardCode def, def))
      [ gandalfsMap
      , caveTorch
      , dungeonTorch
      , shadowKey
      , mendor
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

mendor :: CardDef
mendor =
  ((objective "RC002" "Mendor" PassageThroughMirkwoodCampaign 1)
      { cdCardType = ObjectiveAllyType, cdWillpower = Just 1, cdAttack = Just 1, cdDefense = Just 1, cdHitPoints = Just 3 })
    { cdUnique = True
    , cdCardTraits = setFromList [Scout, Silvan]
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
