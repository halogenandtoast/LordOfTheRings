module LordOfTheRings.Hobbit.EasyModeSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardDef (..), CardType (NightmareType, QuestType), allEncounterCards)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Name (Name (..))
import TestImport

-- | The copies of each card of the set easy mode removes, by title.
removedInEasyMode :: EncounterSet -> Map Text Int
removedInEasyMode set =
  Map.fromList
    [ (title, normal - easy)
    | def <- toList allEncounterCards
    , cdEncounterSet def == Just set
    , cdCardType def `notElem` [QuestType, NightmareType]
    , let Name title _ = cdName def
          normal = fromMaybe 1 (cdEncounterSetQuantity def)
          easy = fromMaybe normal (cdEasyModeQuantity def)
    , easy /= normal
    ]

-- | The cards the Easy Mode rules list to remove from each encounter set of the Hobbit
-- saga expansions (Easy Mode rules sheet, 2013, "Over Hill and Under Hill" and "On the
-- Doorstep").
spec :: Spec
spec = describe "Hobbit easy mode" do
  for_ easyModeLists \(set, removed) ->
    it ("removes the cards the Easy Mode rules list from " <> show set) (removedInEasyMode set `shouldBe` Map.fromList removed :: IO ())
 where
  easyModeLists =
    [ (WeMustAwayEreBreakOfDay, [("Troll Camp", 2), ("Lots or None at All", 2), ("Hungry Troll", 3), ("Roast 'Em or Boil 'Em?", 1)])
    , (OverTheMistyMountainsGrim, [("Stone-giant", 2), ("Galloping Boulders", 3)])
    , (DungeonsDeepAndCavernsDim, [("Great Gray Wolf", 1), ("Hiding in the Trees", 1), ("Come down little bird", 1)])
    , (WesternLands, [("No Campfire", 2), ("Wind-whipped Rain", 1), ("Dreary Hills", 1)])
    , (MistyMountainGoblins, [("Goblin Runners", 2), ("Grip, grab! Pinch, nab!", 3), ("The Goblins' Caves", 1)])
    , (TheGreatGoblin, [("Front Porch", 1), ("Chaos in the Cavern", 2)])
    , (Wilderland, [("Fighting Among Friends", 2), ("Weighed Down", 1)])
    , (FliesAndSpiders, [("Giant Web", 1), ("Old Tomnoddy", 2), ("Fat Spider", 1), ("Lazy Lob", 2), ("Crazy Cob", 1), ("Poisoned by Spiders", 2)])
    , (TheLonelyMountain, [("Great Hall", 3), ("Pretending to Sleep", 3), ("Dragon-Spell", 2)])
    , (TheBattleOfFiveArmies, [("Vanguard of Bolg", 1), ("Ravenous Warg", 2), ("Hatred Rekindled", 1), ("Goblins are Upon You!", 2), ("Northern Slopes", 1)])
    ]
