{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Scenario.Runner (module LordOfTheRings.Scenario.Runner, module X) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card hiding (hero)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.EncounterSet (EncounterSet)
import LordOfTheRings.I18n
import LordOfTheRings.Keyword (Keyword (Guarded))
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (placeResources)
import LordOfTheRings.Game.Base (gamePhase, gamePlayers)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (ReturnBeforeEncounterDeckResets))
import LordOfTheRings.Outcome
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Scenario.Sailing (discardEncounterCard, runSailing)
import LordOfTheRings.Scenario.Types as X
import LordOfTheRings.Source (sourceTarget)
import LordOfTheRings.Target
import LordOfTheRings.Window (WindowType (AdvancedToStage), mkAfter)

-- | The copies of each card in the set, fewer of some in easy mode.
gatherEncounterSet :: MonadRandom m => ScenarioAttrs -> EncounterSet -> m [EncounterCard]
gatherEncounterSet attrs encounterSet =
  concatMapM
    (\def -> replicateM (copies def) (genEncounterCard def))
    -- quest stages share the set but make up the quest deck, never the encounter deck
    $ filter
      (\def -> cdEncounterSet def == Just encounterSet && cdCardType def /= QuestType)
      (toList allEncounterCards)
 where
  copies def =
    fromMaybe 1 $ (guard attrs.easyMode *> cdEasyModeQuantity def) <|> cdEncounterSetQuantity def

instance RunMessage ScenarioAttrs where
  runMessage msg attrs = resetEmptyEncounterDeck =<< case msg of
    -- the scenario gathers its encounter deck at step 1; shuffle it before any setup reveals
    DoStep 2 Setup -> do
      encounterDeck <- shuffleM attrs.encounterDeck
      pure $ attrs & encounterDeckL .~ encounterDeck
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
                $ chooseOne
                  pid
                  [targetLabel y [RemoveFromEncounterDeck y, AddToStaging y, ShuffleDeck EncounterDeck] | y <- x : xs]
              pure attrs
    RevealEncounterCard -> case attrs.encounterDeck of
      (card : rest) -> do
        push $ RevealCard card
        pure $ attrs & encounterDeckL .~ rest
      [] -> pure attrs
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
            then
              [ Label (ikey "label.leaveOnTop") []
              , Label (ikey "label.moveToBottom") [MoveTopOfEncounterDeckToBottom]
              ]
            else [Label (ikey "label.continue") []]
      pure attrs
    RevealOneOfTopEncounterCards pid source n toBottom -> do
      let (top, rest) = splitAt n attrs.encounterDeck
          others c = filter (/= c) top
      unless (null top)
        $ push
        $ Ask pid
        $ QuestionLabel (ikey $ if toBottom then "label.revealOneRestToBottom" else "label.revealOne") (sourceTarget source) []
        $ SearchCards (map EncounterCard top)
        $ ChooseOne
          [ targetLabel c [RevealCard c, if toBottom then PutOnBottomOfEncounterDeck (others c) else AddToEncounterDiscard (others c)]
          | c <- top
          ]
      pure $ attrs & encounterDeckL .~ rest
    PutOnBottomOfEncounterDeck cards -> pure $ attrs & encounterDeckL %~ (<> cards)
    -- the deck reset as it empties partway, the rest are discarded from it (ruling, Collateral
    -- Damage)
    DiscardTopOfEncounterDeck source target n -> do
      let discardFrom k a acc = do
            let (top, rest) = splitAt k a.encounterDeck
                a' = a & encounterDeckL .~ rest & encounterDiscardL %~ (top <>)
                left = k - length top
            a'' <- resetEmptyEncounterDeck a'
            if left > 0 && notNull a''.encounterDeck
              then discardFrom left a'' (acc <> top)
              else pure (a'', acc <> top)
      (attrs', discarded) <- discardFrom n attrs []
      push $ DiscardedTopOfEncounterDeck source target discarded
      pure attrs'
    ResetToStage _ rest -> pure $ attrs & questDeckL .~ rest
    MoveTopOfEncounterDeckToBottom -> case attrs.encounterDeck of
      top : rest -> pure $ attrs & encounterDeckL .~ rest <> [top]
      [] -> pure attrs
    GuardObjective oid -> case attrs.encounterDeck of
      card : rest -> do
        let def = toCardDef card
        push
          $ if
            -- it guards before it resolves, so before its surge; a treachery only until its
            -- effects resolve or are canceled
            | cdCardType def `elem` [EnemyType, LocationType, TreacheryType] ->
                Run [SetObjectiveGuard oid card.id, RevealCard card]
            -- the next card guards this objective, then the revealed one is guarded in turn
            | Guarded `member` cdKeywords def -> Run [GuardObjective oid, RevealCard card]
            | otherwise -> RevealCard card
        pure $ attrs & encounterDeckL .~ rest
      []
        | null attrs.encounterDiscard -> pure attrs
        | otherwise -> do
            deck <- shuffleM attrs.encounterDiscard
            push $ GuardObjective oid
            pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
    -- the next encounter card guards the location, attached to it as it is revealed so its
    -- own effects see it guarding (Troll-fells); any other card is only revealed
    GuardLocation lid -> case attrs.encounterDeck of
      card : rest -> do
        push
          $ if cdCardType (toCardDef card) `elem` [EnemyType, LocationType, TreacheryType, EncounterSideQuestType]
            then Run [PlaceLocation lid (AttachedTo $ CardIdTarget card.id), RevealCard card]
            else RevealCard card
        pure $ attrs & encounterDeckL .~ rest
      []
        | null attrs.encounterDiscard -> pure attrs
        | otherwise -> do
            deck <- shuffleM attrs.encounterDiscard
            push $ GuardLocation lid
            pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ []
    AddSetAsideToStaging matcher -> do
      let (matching, rest) = partition (cardMatches matcher) attrs.scenarioSetAside
      traverse_ (push . AddToStaging) matching
      pure $ attrs & setAsideL .~ rest
    ShuffleEncounterDiscardIntoDeck matcher -> do
      let (matching, rest) = partition (cardMatches matcher) attrs.encounterDiscard
      deck <- shuffleM (matching <> attrs.encounterDeck)
      pure $ attrs & encounterDeckL .~ deck & encounterDiscardL .~ rest
    SetAside card -> pure $ attrs & setAsideL %~ (card :) & encounterDeckL %~ filter (/= card)
    -- a set-aside card revealed, as Cornered Animals reveals Orc Ambush
    RevealCard card -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside card put into play, as The Seething Lake makes Doors of Durin active
    PutIntoPlayAsActiveLocation card -> pure $ attrs & setAsideL %~ filter (/= card)
    ShuffleSetAsideIntoEncounterDeck cards -> do
      let shuffled = filter (`elem` cards) attrs.scenarioSetAside
      deck <- shuffleM (shuffled <> attrs.encounterDeck)
      pure $ attrs & encounterDeckL .~ deck & setAsideL %~ filter (`notElem` shuffled)
    ShuffleVictoryDisplayIntoEncounterDeck matcher -> do
      let (matching, rest) = partition (\case EncounterCard c -> cardMatches matcher c; _ -> False) attrs.victoryDisplay
      deck <- shuffleM ([c | EncounterCard c <- matching] <> attrs.encounterDeck)
      pure $ attrs & encounterDeckL .~ deck & victoryDisplayL .~ rest
    PutOnTopOfEncounterDeck card -> pure $ attrs & encounterDeckL %~ (card :)
    -- a player card with the Encounter keyword goes to its owner's discard pile instead
    AddToEncounterDiscard cards -> do
      owned <- for cards \c -> (c,) <$> setAsideOwner c.id
      pushAll [PutInDiscard (MkPlayerCard c.id (toCardCode c) (Just owner)) | (c, Just owner) <- owned]
      -- a card of the Corsair deck goes to its own discard pile, while there is one
      pure $ foldr discardEncounterCard attrs [c | (c, Nothing) <- owned]
    -- a set-aside card with the Encounter keyword, as Ranger Summons shuffles one in
    ShuffleIntoEncounterDeck (CardIdTarget cid) -> do
      owned <- getsGame (concatMap (.setAsideCards) . toList . gamePlayers)
      deck <- shuffleM $ [MkEncounterCard cid (toCardCode c) | c <- owned, c.id == cid] <> attrs.encounterDeck
      pure $ attrs & encounterDeckL .~ deck
    RemoveFromEncounterDeck card -> pure $ attrs & encounterDeckL %~ filter (/= card)
    RemoveFromEncounterDiscard card -> pure $ attrs & encounterDiscardL %~ filter (/= card)
    RemoveFromVictoryDisplay card -> pure $ attrs & victoryDisplayL %~ filter ((/= card.id) . (.id))
    SearchEncounterDeckAndDiscard pid source matcher -> search pid source matcher AddToStaging
    SearchEncounterDeckAndDiscardToEngage pid source matcher -> search pid source matcher (PutEnemyIntoPlayEngaged pid)
    SearchEncounterDeckAndDiscardToReveal pid source matcher -> search pid source matcher RevealCard
    -- a set-aside enemy put into play engaged, as No Way Out puts Chief Turch
    PutEnemyIntoPlayEngaged _ card -> pure $ attrs & setAsideL %~ filter (/= card)
    -- a set-aside Ship objective put into play, as The Beacon gives a Dol Amroth Warship
    PutObjectiveAllyIntoPlayUnder _ card -> pure $ attrs & setAsideL %~ filter (/= card)
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
    ShuffleIntoQuestDeck card -> do
      deck <- shuffleM (card : filter (/= card) attrs.questDeck)
      pure $ attrs & questDeckL .~ deck & victoryDisplayL %~ filter ((/= card.id) . (.id))
    RemoveFromQuestDeck card -> pure $ attrs & questDeckL %~ filter (/= card)
    PutOnBottomOfQuestDeck card ->
      pure $ attrs & questDeckL %~ ((<> [card]) . filter (/= card)) & victoryDisplayL %~ filter ((/= card.id) . (.id))
    -- a stage put into play straight from the quest deck, as Hasty Council chooses one, leaves it
    PutQuestIntoPlay card -> pure $ attrs & questDeckL %~ filter (/= card)
    AdvanceToQuest card -> do
      pushAll [PutQuestIntoPlay card, Flip (toTarget card), CheckWindows [mkAfter AdvancedToStage]]
      pure attrs
    TakeHiddenCard pid -> do
      (deck, discard) <-
        if null attrs.encounterDeck
          then (,[]) <$> shuffleM attrs.encounterDiscard
          else pure (attrs.encounterDeck, attrs.encounterDiscard)
      pure $ case deck of
        card : rest -> attrs & encounterDeckL .~ rest & encounterDiscardL .~ discard & hiddenCardsL %~ Map.insertWith (flip (<>)) pid [card]
        [] -> attrs
    TurnHiddenCardFaceup pid card msgs | card `elem` hiddenOf pid -> do
      pushAll msgs
      pure $ attrs & hiddenCardsL . ix pid %~ filter (/= card)
    DiscardHiddenCard pid card | card `elem` hiddenOf pid ->
      pure $ attrs & hiddenCardsL . ix pid %~ filter (/= card) & encounterDiscardL %~ (card :)
    -- an eliminated player's hidden cards are discarded
    EliminatePlayer pid -> pure $ attrs & hiddenCardsL %~ Map.delete pid & encounterDiscardL %~ (hiddenOf pid <>)
    _ -> runSailing msg attrs
   where
    -- "If the encounter deck is ever empty during the quest phase, the encounter discard
    -- pile is shuffled and reset back into the encounter deck."
    resetEmptyEncounterDeck a = do
      phase <- getsGame gamePhase
      if phase == QuestPhase && null a.encounterDeck && notNull a.encounterDiscard
        then do
          -- the topmost matching card goes to the staging area first, as Ghostly Ruins reads
          matchers <- (\ms -> [m | ReturnBeforeEncounterDeckResets m <- ms]) <$> getModifiers GameTarget
          let returned = take 1 [c | c <- a.encounterDiscard, any (`cardMatches` c) matchers]
          traverse_ (push . AddToStaging) returned
          deck <- shuffleM $ filter (`notElem` returned) a.encounterDiscard
          pure $ a & encounterDeckL .~ deck & encounterDiscardL .~ []
        else pure a
    hiddenOf pid = Map.findWithDefault [] pid attrs.hiddenCards
    -- the player searches the encounter deck and discard pile, the card found going where
    -- the message puts it, then the deck is shuffled
    search pid source matcher put = do
      let fromDeck = filter (cardMatches matcher) attrs.encounterDeck
          fromDiscard = filter (cardMatches matcher) attrs.encounterDiscard
          take' remove y = targetLabel y [remove y, put y, ShuffleDeck EncounterDeck]
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
