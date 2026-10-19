{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Scenario (module LordOfTheRings.Scenario, module LordOfTheRings.Scenario.Type) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.EncounterSet (EncounterSet)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.GameT
import LordOfTheRings.Message
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quest
import LordOfTheRings.Question
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Scenario.Type

newScenario :: (IsScenario a) => (ScenarioAttrs -> a) -> Scenario
newScenario f = Scenario $ f $ ScenarioAttrs [] [] ResourcePhase

instance RunMessage Scenario where
  runMessage msg (Scenario a) = Scenario <$> runMessage msg a

newtype PassageThroughMirkwood = PassageThroughMirkwood ScenarioAttrs
  deriving newtype (Show, ToJSON)

gatherEncounterSet :: (MonadRandom m) => EncounterSet -> m [EncounterCard]
gatherEncounterSet encounterSet =
  concatMapM
    (\def -> replicateM (fromMaybe 1 $ cdEncounterSetQuantity def) (genEncounterCard def))
    $ filter ((== Just encounterSet) . cdEncounterSet) (toList allEncounterCards)

instance IsScenario PassageThroughMirkwood
instance RunMessage PassageThroughMirkwood where
  runMessage msg (PassageThroughMirkwood a) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          gatherEncounterSet
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

      push $ PutQuestIntoPlay fliesAndSpiders
      pure $ PassageThroughMirkwood $ a {questDeck}
    _ -> PassageThroughMirkwood <$> runMessage msg a

cardMatches :: CardMatcher -> EncounterCard -> Bool
cardMatches = \case
  CardWithCode code -> (== code) . toCardCode

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
        if all ((== toCardCode x) . toCardCode) xs
          then do
            encounterDeck <- shuffleM $ filter ((/= x.id) . (.id)) attrs.encounterDeck
            push $ AddToStaging x
            pure $ attrs {encounterDeck}
          else do
            push $
              chooseOne pid $
                targetLabel x [AddToStaging x]
                    : map (\y -> targetLabel y [AddToStaging y, ShuffleDeck EncounterDeck]) xs
            pure attrs
  _ -> pure attrs

instance RunMessage ScenarioAttrs where
  runMessage = runScenarioMessage
