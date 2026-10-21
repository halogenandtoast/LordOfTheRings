module LordOfTheRings.Scenario.Runner (module LordOfTheRings.Scenario.Runner, module X) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.EncounterSet (EncounterSet)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (placeResources)
import LordOfTheRings.Outcome
import LordOfTheRings.I18n
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Source (Source, sourceTarget)
import LordOfTheRings.Target
import LordOfTheRings.Scenario.Types as X

-- | The copies of each card in the set, fewer of some in easy mode.
gatherEncounterSet :: (MonadRandom m) => ScenarioAttrs -> EncounterSet -> m [EncounterCard]
gatherEncounterSet attrs encounterSet =
  concatMapM
    (\def -> replicateM (copies def) (genEncounterCard def))
    -- quest stages share the set but make up the quest deck, never the encounter deck
    $ filter (\def -> cdEncounterSet def == Just encounterSet && cdCardType def /= QuestType) (toList allEncounterCards)
 where
  copies def =
    fromMaybe 1 $ (guard attrs.easyMode *> cdEasyModeQuantity def) <|> cdEncounterSetQuantity def

instance RunMessage ScenarioAttrs where
  runMessage msg attrs = case msg of
    DoStep 4 Setup | attrs.easyMode -> do
      heroes <- select AnyHero
      for_ heroes \hero -> placeResources attrs hero 1
      pure attrs
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
          $ QuestionLabel (ikey "label.topOfEncounterDeck") (sourceTarget source) []
          $ SearchCards [EncounterCard top]
          $ ChooseOne
          $ if canMove
            then [Label (ikey "label.leaveOnTop") [], Label (ikey "label.moveToBottom") [MoveTopOfEncounterDeckToBottom]]
            else [Label (ikey "label.continue") []]
      pure attrs
    RevealOneOfTopEncounterCards pid source n -> do
      let (top, rest) = splitAt n attrs.encounterDeck
      unless (null top)
        $ push
        $ Ask pid
        $ QuestionLabel (ikey "label.revealOne") (sourceTarget source) []
        $ SearchCards (map EncounterCard top)
        $ ChooseOne [targetLabel c [RevealCard c, AddToEncounterDiscard (filter (/= c) top)] | c <- top]
      pure $ attrs & encounterDeckL .~ rest
    ResetToStage _ rest -> pure $ attrs & questDeckL .~ rest
    MoveTopOfEncounterDeckToBottom -> case attrs.encounterDeck of
      top : rest -> pure $ attrs & encounterDeckL .~ rest <> [top]
      [] -> pure attrs
    GuardObjective oid -> case attrs.encounterDeck of
      card : rest -> do
        push
          $ if cdCardType (toCardDef card) `elem` [EnemyType, LocationType]
            then Run [AddToStaging card, SetObjectiveGuard oid card.id]
            else RevealCard card
        pure $ attrs & encounterDeckL .~ rest
      [] -> pure attrs
    AddSetAsideToStaging matcher -> do
      let (matching, rest) = partition (cardMatches matcher) attrs.scenarioSetAside
      traverse_ (push . AddToStaging) matching
      pure $ attrs & setAsideL .~ rest
    ShuffleEncounterDiscardIntoDeck matcher -> do
      let (matching, rest) = partition (cardMatches matcher) attrs.encounterDiscard
      deck <- shuffleM (matching <> attrs.encounterDeck)
      pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ rest
    SetAside card -> pure $ attrs & setAsideL %~ (card :) & encounterDeckL %~ filter (/= card)
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
          (if null choices then (ikey "label.searchFoundNothing") else (ikey "label.searchChoose"))
          (Just source)
          []
        $ SearchCards (map EncounterCard $ attrs.encounterDeck <> attrs.encounterDiscard)
        $ ChooseOne
        $ if null choices then [Label (ikey "label.continue") [ShuffleDeck EncounterDeck]] else choices
      pure attrs
    DebugSearchEncounterDeck pid -> do
      push
        $ Ask pid
        $ QuestionLabel (ikey "label.revealCard") Nothing []
        $ SearchCards (map EncounterCard attrs.encounterDeck)
        $ ChooseOne
        $ Label (ikey "label.done") []
        : [targetLabel y [RemoveFromEncounterDeck y, RevealCard y] | y <- attrs.encounterDeck]
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
