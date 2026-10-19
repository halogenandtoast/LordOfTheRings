module LordOfTheRings.Scenario.Runner (module LordOfTheRings.Scenario.Runner, module X) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.EncounterSet (EncounterSet)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Message
import LordOfTheRings.Outcome
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Target
import LordOfTheRings.Scenario.Types as X

gatherEncounterSet :: (MonadRandom m) => EncounterSet -> m [EncounterCard]
gatherEncounterSet encounterSet =
  concatMapM
    (\def -> replicateM (fromMaybe 1 $ cdEncounterSetQuantity def) (genEncounterCard def))
    $ filter ((== Just encounterSet) . cdEncounterSet) (toList allEncounterCards)

cardMatches :: CardMatcher -> EncounterCard -> Bool
cardMatches = \case
  CardWithCode code -> (== code) . toCardCode

instance RunMessage ScenarioAttrs where
  runMessage msg attrs = case msg of
    ShuffleDeck EncounterDeck -> do
      encounterDeck <- shuffleM attrs.encounterDeck
      pure $ attrs & encounterDeckL .~ encounterDeck
    Search pid EncounterDeck cardMatcher ToStaging Shuffle -> do
      let matches = filter (cardMatches cardMatcher) attrs.encounterDeck
      case matches of
        [] -> pure attrs
        (x : xs) ->
          if all ((== toCardCode x) . toCardCode) xs
            then do
              encounterDeck <- shuffleM $ filter ((/= x.id) . (.id)) attrs.encounterDeck
              push $ AddToStaging x
              pure $ attrs & encounterDeckL .~ encounterDeck
            else do
              push
                $ chooseOne pid
                  [targetLabel y [RemoveFromEncounterDeck y, AddToStaging y, ShuffleDeck EncounterDeck] | y <- x : xs]
              pure attrs
    RemoveFromEncounterDeck card -> pure $ attrs & encounterDeckL %~ filter (/= card)
    AdvanceQuest -> case attrs.questDeck of
      [] -> do
        push $ GameOver Won
        pure attrs
      (next : rest) -> do
        push $ AdvanceToQuest next
        pure $ attrs & questDeckL .~ rest
    AdvanceToQuest card -> do
      pushAll [PutQuestIntoPlay card, Flip (toTarget card)]
      pure attrs
    _ -> pure attrs
