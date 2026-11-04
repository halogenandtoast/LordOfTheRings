{- | The Saga Expansions: The Hobbit's two boxes and The Lord of the Rings' six. Their
scenarios share rules no other scenario has: Bilbo Baggins of the Baggins sphere in The
Hobbit's, a Fellowship sphere hero in The Lord of the Rings', and there each player
revealing his own encounter card in the staging step (The Black Riders rules insert,
"New Staging Rules").
-}
module LordOfTheRings.Saga (module LordOfTheRings.Saga) where

import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Id (ScenarioId)
import LordOfTheRings.Prelude

data Saga = TheHobbit | TheLordOfTheRings
  deriving stock (Show, Eq)

sagaOf :: ScenarioId -> Maybe Saga
sagaOf sid
  | sid `elem` hobbitScenarios = Just TheHobbit
  | sid `elem` lordOfTheRingsScenarios = Just TheLordOfTheRings
  | otherwise = Nothing

-- | Over Hill and Under Hill's, then On the Doorstep's.
hobbitScenarios :: [ScenarioId]
hobbitScenarios =
  [ "we-must-away-ere-break-of-day"
  , "over-the-misty-mountains-grim"
  , "dungeons-deep-and-caverns-dim"
  , "flies-and-spiders"
  , "the-lonely-mountain"
  , "the-battle-of-five-armies"
  ]

-- | The Black Riders' to The Mountain of Fire's, in campaign order.
lordOfTheRingsScenarios :: [ScenarioId]
lordOfTheRingsScenarios =
  [ "a-shadow-of-the-past"
  , "a-knife-in-the-dark"
  , "flight-to-the-ford"
  , "the-ring-goes-south"
  , "journey-in-the-dark"
  , "breaking-of-the-fellowship"
  , "the-uruk-hai"
  , "helms-deep"
  , "the-road-to-isengard"
  , "the-passage-of-the-marshes"
  , "journey-to-the-cross-roads"
  , "shelobs-lair"
  , "the-passing-of-the-grey-company"
  , "the-siege-of-gondor"
  , "the-battle-of-the-pelennor-fields"
  , "the-tower-of-cirith-ungol"
  , "the-black-gate-opens"
  , "mount-doom"
  ]

-- | Whether each player reveals his own encounter card in the staging step, in turn order,
-- resolving it, and any card its surge reveals, before the next player reveals his: "you"
-- on such a card is the player who revealed it.
revealsIndividually :: ScenarioId -> Bool
revealsIndividually = (== Just TheLordOfTheRings) . sagaOf

{- | The hero the first player must take control of as the scenario is set up, beyond his
deck's heroes, one of these if more than one: the Baggins sphere Bilbo Baggins of the
scenario's box in The Hobbit's, in The Lord of the Rings' a Fellowship sphere hero, with
The One Ring attached when the scenario has one. Any Fellowship hero with the Ring-bearer
trait is a legal Ring-bearer (The Road Darkens rules insert, FAQ).
-}
data FellowshipSetup = FellowshipSetup
  { fellowshipHeroes :: [CardCode]
  , fellowshipRing :: Maybe CardCode
  }

fellowshipSetup :: ScenarioId -> Maybe FellowshipSetup
fellowshipSetup sid
  | sid `elem` take 3 hobbitScenarios = Just $ FellowshipSetup ["131001"] Nothing
  | sid `elem` drop 3 hobbitScenarios = Just $ FellowshipSetup ["132001"] Nothing
  | sid `elem` ["a-shadow-of-the-past", "a-knife-in-the-dark", "flight-to-the-ford"] = ringBearer "141085"
  | sid `elem` ["the-ring-goes-south", "journey-in-the-dark", "breaking-of-the-fellowship"] = ringBearer "142091"
  -- the Fellowship sphere Aragorn of each box (Treason of Saruman, Flame of the West and
  -- Mountain of Fire rules inserts)
  | sid `elem` ["the-uruk-hai", "helms-deep", "the-road-to-isengard"] = Just $ FellowshipSetup ["143001"] Nothing
  | sid `elem` ["the-passage-of-the-marshes", "journey-to-the-cross-roads", "shelobs-lair"] = ringBearer "144063"
  | sid `elem` ["the-passing-of-the-grey-company", "the-siege-of-gondor", "the-battle-of-the-pelennor-fields"] = Just $ FellowshipSetup ["145001"] Nothing
  | sid == "the-black-gate-opens" = Just $ FellowshipSetup ["146002"] Nothing
  | sid `elem` ["the-tower-of-cirith-ungol", "mount-doom"] = ringBearer "146065"
  | otherwise = Nothing
 where
  ringBearer ring = Just $ FellowshipSetup ringBearers (Just ring)

-- | The Fellowship sphere heroes with the Ring-bearer trait: the Frodo Baggins of The Black
-- Riders, The Road Darkens, The Land of Shadow and The Mountain of Fire.
ringBearers :: [CardCode]
ringBearers = ["141001", "142001", "144001", "146001"]

{- | The titles no player may use, as a hero or in his deck, in the scenario: another
version of its Baggins or Fellowship hero, Sméagol anywhere in the sagas (MEC77 rules),
Saruman and Gríma in The Treason of Saruman, and, in campaign mode, Frodo Baggins in The
Treason of Saruman and The Flame of the West (their rules inserts). The Mountain of Fire
bars Aragorn from The Tower of Cirith Ungol and Mount Doom, and Frodo Baggins from The
Black Gate Opens.
-}
sagaBarredTitles :: ScenarioId -> Bool -> [Text]
sagaBarredTitles sid inCampaign = case sagaOf sid of
  Nothing -> []
  Just TheHobbit -> ["Bilbo Baggins"]
  Just TheLordOfTheRings ->
    "Sméagol"
      : case () of
        _
          | sid `elem` ["the-uruk-hai", "helms-deep", "the-road-to-isengard"] ->
              ["Aragorn", "Saruman", "Gríma"] <> ["Frodo Baggins" | inCampaign]
          | sid `elem` ["the-passing-of-the-grey-company", "the-siege-of-gondor", "the-battle-of-the-pelennor-fields"] ->
              ["Aragorn"] <> ["Frodo Baggins" | inCampaign]
          | sid == "the-black-gate-opens" -> ["Aragorn", "Frodo Baggins"]
          | sid `elem` ["the-tower-of-cirith-ungol", "mount-doom"] -> ["Frodo Baggins", "Aragorn"]
          | otherwise -> ["Frodo Baggins"]

{- | The treasure sets a Hobbit scenario's setup lists: a player may add to his deck the
treasures of these sets he has discovered with the same heroes, 1 copy of each (The
Hobbit: Over Hill and Under Hill rules insert, "Treasure").
-}
treasureSetsFor :: ScenarioId -> [EncounterSet]
treasureSetsFor sid = take (length (takeWhile (/= sid) hobbitScenarios)) hobbitTreasureSets
 where
  -- the set icon of the treasures discovered in each scenario, in order
  hobbitTreasureSets =
    [ WeMustAwayEreBreakOfDay
    , OverTheMistyMountainsGrim
    , DungeonsDeepAndCavernsDim
    , FliesAndSpiders
    , TheLonelyMountain
    , TheBattleOfFiveArmies
    ]

{- | The burden sets whose burdens the scenario's campaign card removes from the encounter
deck and each player's deck: those of the Campaign Pool are left out as it is set up.
-}
removedBurdenSets :: ScenarioId -> [EncounterSet]
removedBurdenSets = \case
  "helms-deep" -> [AShadowOfThePast, FlightToTheFord, TheRingGoesSouth]
  "the-road-to-isengard" -> [AShadowOfThePast, FlightToTheFord, TheRingGoesSouth]
  "the-passage-of-the-marshes" -> [HelmsDeep, TheRoadToIsengard]
  "journey-to-the-cross-roads" -> [HelmsDeep, TheRoadToIsengard]
  "the-tower-of-cirith-ungol" -> [HelmsDeep, TheRoadToIsengard, ThePassingOfTheGreyCompany]
  "the-black-gate-opens" -> [AShadowOfThePast, FlightToTheFord, TheRingGoesSouth, ThePassageOfTheMarshes, ShelobsLair]
  "mount-doom" -> [HelmsDeep, TheRoadToIsengard, ThePassingOfTheGreyCompany]
  _ -> []
