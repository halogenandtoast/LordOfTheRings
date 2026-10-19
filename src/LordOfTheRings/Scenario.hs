{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Scenario (module LordOfTheRings.Scenario, module LordOfTheRings.Scenario.Type) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.EncounterSet (EncounterSet)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.Cards (allEnemies)
import LordOfTheRings.Location.Cards (allLocations)
import LordOfTheRings.GameT
import LordOfTheRings.Message
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Quest
import LordOfTheRings.Quest.Cards qualified as Quest
import LordOfTheRings.Question
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Scenario.Type
import LordOfTheRings.Treachery.Cards (allTreacheries)

newScenario :: (IsScenario a) => (ScenarioAttrs -> a) -> Scenario
newScenario f = Scenario $ f $ ScenarioAttrs [] Nothing [] Normal ResourcePhase

instance RunMessage Scenario where
  runMessage msg (Scenario a) = Scenario <$> runMessage msg a

newtype PassageThroughMirkwood = PassageThroughMirkwood ScenarioAttrs
  deriving newtype (Show, ToJSON)

allEncounterCardDefs :: Map CardCode SomeEncounterCardDef
allEncounterCardDefs =
  Map.unions
    [ Map.map MkSomeEncounterCardDef allEnemies
    , Map.map MkSomeEncounterCardDef allTreacheries
    , Map.map MkSomeEncounterCardDef allLocations
    ]

gatherEncounterSet :: (MonadRandom m) => Difficulty -> EncounterSet -> m [SomeEncounterCard]
gatherEncounterSet difficulty encounterSet = do
  concatMapM
    (\e -> replicateM (pickCount e.encounterSetCount) (genSomeEncounterCard e))
    $ filter (\e -> e.encounterSet == encounterSet) (Map.elems allEncounterCardDefs)
 where
  pickCount :: (Int, Int, Int) -> Int
  pickCount (easy, normal, nightmare) = case difficulty of
    Easy -> easy
    Normal -> normal
    Nightmare -> nightmare

instance IsScenario PassageThroughMirkwood
instance RunMessage PassageThroughMirkwood where
  runMessage msg (PassageThroughMirkwood a) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet a.difficulty)
          [EncounterSet.DolGuldurOrcs, EncounterSet.PassageThroughMirkwood, EncounterSet.SpidersOfMirkwood]
      pure $ PassageThroughMirkwood $ a {encounterDeck}
    DoStep 6 Setup -> do
      fliesAndSpiders <- genEncounterCard Quest.fliesAndSpiders
      questDeck <-
        (fliesAndSpiders :)
          <$> traverse
            genEncounterCard
            [ Quest.aForkInTheRoad
            , Quest.aChosenPathDontLeaveThePath
            , Quest.aChosenPathBeornsPath
            ]

      quest <- Just <$> newQuest fliesAndSpiders

      pure $ PassageThroughMirkwood $ a {questDeck, quest}
    _ -> PassageThroughMirkwood <$> runMessage msg a

cardMatches :: CardMatcher -> SomeEncounterCard -> Bool
cardMatches = \case
  CardWithCode code -> \case
    SomeEncounterCard (MkEncounterCard _ c) -> code == c.code

runScenarioMessage :: Message -> ScenarioAttrs -> GameT ScenarioAttrs
runScenarioMessage msg attrs = case msg of
  BeginPhase p -> pure $ attrs {phase = p}
  ShuffleDeck EncounterDeck -> do
    encounterDeck <- shuffleM attrs.encounterDeck
    pure $ attrs {encounterDeck}
  Search pid EncounterDeck cardMatcher ToStaging Shuffle -> do
    let matches = filter (cardMatches cardMatcher) attrs.encounterDeck
    case matches of
      [] -> pure attrs
      (x : xs) ->
        if all ((== x.code) . (.code)) xs
          then do
            encounterDeck <- shuffleM $ filter ((/= x.id) . (.id)) attrs.encounterDeck
            push $ AddToStaging x
            pure $ attrs {encounterDeck}
          else do
            push $
              Ask pid $
                ChooseOne $
                  targetChoice x [AddToStaging x]
                    : map (\y -> targetChoice y [AddToStaging y, ShuffleDeck EncounterDeck]) xs
            pure attrs
  _ -> pure attrs

instance RunMessage ScenarioAttrs where
  runMessage msg attrs = do
    quest <- traverse (runMessage msg) attrs.quest
    runScenarioMessage msg $ attrs {quest}
