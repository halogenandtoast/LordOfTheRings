module LordOfTheRings.Scenario.Runner (module LordOfTheRings.Scenario.Runner, module X) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.EncounterSet (EncounterSet)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Message
import LordOfTheRings.Outcome
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Source (Source, sourceTarget)
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
  CardWithTrait trait -> member trait . cdCardTraits . toCardDef

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
    RevealEncounterCard -> case attrs.encounterDeck of
      (card : rest) -> do
        push $ RevealCard card
        pure $ attrs & encounterDeckL .~ rest
      []
        | null attrs.encounterDiscard -> pure attrs
        | otherwise -> do
            deck <- shuffleM attrs.encounterDiscard
            push RevealEncounterCard
            pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
    DealShadowCard eid -> case attrs.encounterDeck of
      (card : rest) -> do
        push $ AttachShadowCard eid card
        pure $ attrs & encounterDeckL .~ rest
      [] -> pure attrs
    LookAtTopOfEncounterDeck pid source canMove -> do
      for_ (headMay attrs.encounterDeck) \top ->
        push
          $ Ask pid
          $ QuestionLabel "The top card of the encounter deck" (sourceTarget source) []
          $ SearchCards [EncounterCard top]
          $ ChooseOne
          $ if canMove
            then [Label "Leave it on top" [], Label "Move it to the bottom" [MoveTopOfEncounterDeckToBottom]]
            else [Label "Continue" []]
      pure attrs
    MoveTopOfEncounterDeckToBottom -> case attrs.encounterDeck of
      top : rest -> pure $ attrs & encounterDeckL .~ rest <> [top]
      [] -> pure attrs
    AddToEncounterDiscard cards -> pure $ attrs & encounterDiscardL %~ (cards <>)
    RemoveFromEncounterDeck card -> pure $ attrs & encounterDeckL %~ filter (/= card)
    RemoveFromEncounterDiscard card -> pure $ attrs & encounterDiscardL %~ filter (/= card)
    SearchEncounterDeckAndDiscard pid source matcher -> do
      let fromDeck = filter (cardMatches matcher) attrs.encounterDeck
          fromDiscard = filter (cardMatches matcher) attrs.encounterDiscard
          take' remove y = targetLabel y [remove y, AddToStaging y, ShuffleDeck EncounterDeck]
          choices =
            map (take' RemoveFromEncounterDeck) fromDeck
              <> map (take' RemoveFromEncounterDiscard) fromDiscard
      push
        $ Ask pid
        $ QuestionLabel
          (if null choices then "Nothing found: the search turned up no match" else "Choose a card to add to the staging area")
          (Just source)
          []
        $ SearchCards (map EncounterCard $ attrs.encounterDeck <> attrs.encounterDiscard)
        $ ChooseOne
        $ if null choices then [Label "Continue" [ShuffleDeck EncounterDeck]] else choices
      pure attrs
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
