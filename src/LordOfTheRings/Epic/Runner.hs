module LordOfTheRings.Epic.Runner (runEpicMessage, finalizeEpicOutcome, usesEpicRoundSynchronization, epicRoundPhaseOf, isEpicQuestAdvanceAuthorized) where

import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability.Types (isForcedAbility)
import LordOfTheRings.Card
import LordOfTheRings.Classes.RunMessage (runMessage)
import LordOfTheRings.Classes.Entity (overAttrs, toAttrs, toId, insertEntity)
import LordOfTheRings.Classes.HasQueue (push, pushEnd, runQueueT)
import LordOfTheRings.Epic.Lorien (transferLorienSupport, receiveLorienSupport)
import LordOfTheRings.Epic.Board
import LordOfTheRings.Epic.DolGuldur (DolPhase (DolPreparingStages, DolRevealingStages), DolTransition (..), DolAck (..))
import LordOfTheRings.Epic.Types
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhaseOf, UntilEndOfRound))
import LordOfTheRings.Enemy (EnemyAttrs (..), createEnemy)
import LordOfTheRings.Entities (enemiesL)
import LordOfTheRings.Game.Base
import LordOfTheRings.Id (QuestId)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.I18n (cardVar, textVar)
import LordOfTheRings.Matcher.Types (CardMatcher (CardWithType, AnyCard), cardMatches)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Quest (isSideQuest)
import LordOfTheRings.Quest.Types (questUnderneath)
import LordOfTheRings.Modifier (ModifierType (FewerStagingReveals, AtEndOfRound))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Prelude
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Helpers.Modifiers (resourcesBarredFrom)
import LordOfTheRings.Helpers.Healing (healableCharacters)
import LordOfTheRings.Matcher (getLead, getPlayer, select, HeroMatcher (HeroControlledBy), AllyMatcher (AllyControlledBy), PlayerMatcher (PlayerWithId))
import LordOfTheRings.Scenario.Runner (epicMessageId)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Question (Question (PlayerWindow, QuestionLabel, ChooseOne))
import LordOfTheRings.I18n (ikey)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioSetAside))
import LordOfTheRings.Source (Source (CardCodeSource, GameSource))
import LordOfTheRings.Target
import LordOfTheRings.Window (mkWhen, mkAfter, WindowType (RoundBegins, AdvancedToStage))
import LordOfTheRings.Trait (Trait (Basic, Upgraded))

usesEpicRoundSynchronization :: Game -> Bool
usesEpicRoundSynchronization g = maybe False (\replica -> isJust (annuminasCityStrength replica.epicShared) || isJust (dolGuldurCityStrength replica.epicShared)) $ scenarioEpic $ toAttrs g.scenario

epicRoundPhaseOf :: Game -> Maybe EpicRoundPhase
epicRoundPhaseOf g = epicRoundPhase <$> (scenarioEpic (toAttrs g.scenario) >>= epicRoundState)

setEpicRoundPhase :: EpicRoundPhase -> Game -> Game
setEpicRoundPhase phase g = g & scenarioL %~ overAttrs (\a -> a
  { scenarioEpic = fmap (\replica -> replica {epicRoundState = Just $ EpicRoundState g.round phase}) a.scenarioEpic })

isEpicQuestAdvanceAuthorized :: Game -> QuestId -> Bool
isEpicQuestAdvanceAuthorized g qid = epicRoundPhaseOf g == Just EpicRoundPreparingAdvance
  && fmap fst (gameEpicQuestAdvance g) == Just qid

presentEpicTarget :: Game -> Target -> Bool
presentEpicTarget g (PlayerTarget pid) = maybe False (not . (.eliminated)) $ Map.lookup pid g.players
presentEpicTarget g target = target == ScenarioTarget || any ((== target) . epicTarget) board.epicBoardTargets
 where
  own = maybe (GroupOrdinal 0) epicGroup $ scenarioEpic $ toAttrs g.scenario
  board = epicBoardFor own "" g

runEpicMessage :: Message -> Game -> GameT Game
runEpicMessage msg g = case msg of
  EndRound | usesEpicRoundSynchronization g -> do
    push AwaitEpicRound
    pure $ case epicRoundPhaseOf g of
      Nothing -> setEpicRoundPhase EpicRoundWaiting g
      Just EpicRoundIdle -> setEpicRoundPhase EpicRoundWaiting g
      _ -> g
  ResumeEpicQuestAdvance phase
    | phase `elem` [EpicRoundResolving, EpicRoundEnded]
    , epicRoundPhaseOf g == Just EpicRoundRevealed ->
        -- The coordinator appends held-claim acknowledgements in the same
        -- delivery. A new wait here would park ahead of those acknowledgements.
        pure $ setEpicRoundPhase phase g
  RejectDolStageClaim nonce -> case scenarioEpic (toAttrs g.scenario) of
    Just replica | nonce `Set.member` replica.epicPendingRequests -> do
      push $ ScenarioSpecific "dolGuldur.chooseStage" Null
      pure $ g & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just replica
        {epicPendingRequests = Set.delete nonce replica.epicPendingRequests}})
    _ -> pure g
  AwaitEpicRound -> do
    when (epicRoundPhaseOf g `elem` map Just [EpicRoundWaiting, EpicRoundReady, EpicRoundEnded, EpicRoundExpired, EpicRoundBeginning, EpicRoundBeginReady, EpicRoundPrepared, EpicRoundRevealed, EpicRoundBegan]) do
      lead <- getLead
      push $ Ask lead $ PlayerWindow msg $ QuestionLabel (ikey "label.epic.waitingForRound") Nothing [] $ ChooseOne []
    pure g
  FreezeEpicRoundEnd abilities | epicRoundPhaseOf g == Just EpicRoundWaiting -> do
    -- FAQ 1.37: the end-of-round passive window is active before Forced
    -- abilities. All teams freeze here before the coordinator releases any.
    let marker = Effect GameSource GameTarget [AtEndOfRound] UntilEndOfRound
    pure $ setEpicRoundPhase EpicRoundReady g
      & frozenRoundEndAbilitiesL ?~ filter isForcedAbility abilities
      & effectsL %~ (marker :)
  BeginEpicRoundEnd | epicRoundPhaseOf g == Just EpicRoundReady -> do
    push EndRound
    pure $ setEpicRoundPhase EpicRoundResolving g
  BeginRound | usesEpicRoundSynchronization g -> case epicRoundPhaseOf g of
    Just EpicRoundBeginning -> do
      -- Raw BeginRound has reached entities once. Finish their queued resets
      -- before parking; the global coordinator freezes triggers afterwards.
      pushEnd AwaitEpicRound
      let advanced = g & roundL +~ 1
            & separationL %~ fmap (\s -> s {separationAreas = [a {areaStatus = AreaWaiting} | a <- separationAreas s]})
      pure $ setEpicRoundPhase EpicRoundBeginning advanced & frozenRoundEndAbilitiesL .~ Nothing
    _ -> do
      push AwaitEpicRound
      pure $ if epicRoundPhaseOf g == Just EpicRoundResolving
        then setEpicRoundPhase EpicRoundEnded g
        else g
  FinishEpicRoundEffects | epicRoundPhaseOf g == Just EpicRoundEnded ->
    pure $ setEpicRoundPhase EpicRoundExpired g & effectsL %~ filter ((/= UntilEndOfRound) . effectDuration)
  -- Compatibility for already-queued releases from before the split protocol.
  -- New event coordinators use BeginEpicRoundBeginning instead.
  BeginEpicNextRound | epicRoundPhaseOf g == Just EpicRoundExpired -> do
    push BeginRound
    pure $ setEpicRoundPhase EpicRoundIdle g & frozenRoundEndAbilitiesL .~ Nothing
  BeginEpicRoundBeginning | epicRoundPhaseOf g == Just EpicRoundExpired -> do
    push BeginRound
    pure $ setEpicRoundPhase EpicRoundBeginning g
  FreezeEpicRoundBeginning abilities | epicRoundPhaseOf g == Just EpicRoundBeginning ->
    pure $ setEpicRoundPhase EpicRoundBeginReady g
      & frozenRoundBeginsAbilitiesL ?~ filter isForcedAbility abilities
      & resolvingQuestsL .~ Map.filter (not . isSideQuest) g.entities.quests
  PrepareEpicQuestAdvance mode card
    | epicRoundPhaseOf g `elem` map Just [EpicRoundBeginReady, EpicRoundResolving, EpicRoundEnded]
    , [old] <- filter (not . isSideQuest) $ toList g.entities.quests -> do
        let oldId = toId old
        runQueueT $ case mode of
          EpicDefeatQuest -> push $ CompleteQuest oldId
          EpicLeaveQuest -> do
            push $ LeaveQuestStage oldId
            -- Dol Guldur rotates stage-three quests without defeating them;
            -- return the same physical card to its available quest deck.
            when (cdStage (toCardDef old) == Just 3) $ case toCard old of
              EncounterCard prior -> push $ ShuffleIntoQuestDeck prior
              _ -> pure ()
            push $ AdvanceToQuest card
        pure $ setEpicRoundPhase EpicRoundPreparingAdvance g
          & epicQuestAdvanceL ?~ (oldId, card)
          & resolvingQuestsL %~ Map.insert oldId old
  MarkEpicQuestPrepared
    | epicRoundPhaseOf g == Just EpicRoundPreparingAdvance
    , Just (_, card) <- (gameEpicQuestAdvance g)
    , any ((== card.id) . (.cardId) . toAttrs) g.entities.quests -> do
        runQueueT do
          for_ (scenarioEpic $ toAttrs g.scenario) $ \replica ->
            for_ replica.epicShared.dolGuldurCoordination $ \state ->
              when (state.epicDolTransition.dolTransitionPhase == DolPreparingStages) $
                push $ QueueEpicCommand (GroupOrdinal 0) (toCardCode card)
                  (EpicDolStepDone state.epicDolTransition.dolTransitionEpoch $ DolTeamPrepared replica.epicGroup) ScenarioTarget
          push AwaitEpicRound
        pure $ setEpicRoundPhase EpicRoundPrepared g
  RevealEpicQuestAdvance
    | epicRoundPhaseOf g == Just EpicRoundPrepared
    , Just (_, card) <- (gameEpicQuestAdvance g) -> do
        runQueueT do
          push $ Flip $ CardIdTarget card.id
          push $ CheckWindows [mkAfter AdvancedToStage]
          push MarkEpicQuestRevealed
        pure $ setEpicRoundPhase EpicRoundRevealing g
  MarkEpicQuestRevealed
    | epicRoundPhaseOf g == Just EpicRoundRevealing
    , Just (_, card) <- gameEpicQuestAdvance g -> do
        runQueueT do
          for_ (scenarioEpic $ toAttrs g.scenario) $ \replica ->
            for_ replica.epicShared.dolGuldurCoordination $ \state ->
              when (state.epicDolTransition.dolTransitionPhase == DolRevealingStages) $
                push $ QueueEpicCommand (GroupOrdinal 0) (toCardCode card)
                  (EpicDolStepDone state.epicDolTransition.dolTransitionEpoch $ DolTeamRevealed replica.epicGroup) ScenarioTarget
          push AwaitEpicRound
        pure $ setEpicRoundPhase EpicRoundRevealed g & epicQuestAdvanceL .~ Nothing
  ReleaseEpicRoundBeginning
    | epicRoundPhaseOf g `elem` map Just [EpicRoundBeginReady, EpicRoundRevealed] -> do
        runQueueT do
          push $ CheckWindows [mkWhen RoundBegins]
          push FinishEpicRoundBeginning
        pure $ setEpicRoundPhase EpicRoundBeginResolving g
  FinishEpicRoundBeginning | epicRoundPhaseOf g == Just EpicRoundBeginResolving -> do
    push AwaitEpicRound
    pure $ setEpicRoundPhase EpicRoundBegan g & frozenRoundBeginsAbilitiesL .~ Nothing & resolvingQuestsL .~ mempty
  BeginEpicRoundResources | epicRoundPhaseOf g == Just EpicRoundBegan -> do
    runQueueT do
      push $ BeginPhase minBound
      push $ EndPhase minBound
    pure $ setEpicRoundPhase EpicRoundIdle g
  RequestAnnuminasHost code destination -> do
    let replica = scenarioEpic $ toAttrs g.scenario
        own = maybe (GroupOrdinal 0) epicGroup replica
        host = take 1 $ filter ((== toCardCode Enemies.hostOfAngmar) . toCardCode) $ scenarioSetAside $ toAttrs g.scenario
    if destination == own
      then g <$ runQueueT (traverse_ addToStaging host)
      else case replica of
        Nothing -> pure g
        Just r -> do
          nonce <- epicMessageId r
          runQueueT do
            push $ QueueEpicCommand destination code (EpicSendSetAsideHost own nonce) ScenarioTarget
            push $ AwaitEpicReply nonce
          pure $ g & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ r {epicPendingRequests = Set.insert nonce r.epicPendingRequests}})
  RequestEpicEffect destination code effect target -> do
    let replica = scenarioEpic $ toAttrs g.scenario
        own = maybe (GroupOrdinal 0) epicGroup replica
    let coordinatorEffect = case effect of
          EpicSubmitDolClaim {} -> True
          EpicDolStepDone {} -> True
          _ -> False
    if destination == own && not coordinatorEffect
      then g <$ when (presentEpicTarget g target) (push $ ResolveEpicEffect code effect target)
      else case replica of
        Nothing -> pure g
        Just r -> do
          nonce <- epicMessageId r
          runQueueT do
            push $ QueueEpicCommand destination code (EpicResolveWithReply own nonce effect) target
            push $ AwaitEpicReply nonce
          pure $ g & scenarioL %~ overAttrs (\attrs -> attrs {scenarioEpic = Just r {epicPendingRequests = Set.insert nonce r.epicPendingRequests}})
  AwaitEpicReply nonce -> do
    let pending = maybe False (Set.member nonce . epicPendingRequests) $ scenarioEpic $ toAttrs g.scenario
    when pending do
      lead <- getLead
      push $ Ask lead $ PlayerWindow msg $ QuestionLabel (ikey "label.epic.waitingForTeam") Nothing [] $ ChooseOne []
    pure g
  ResolveEpicEffect _ (EpicReceiveSetAsideHost nonce card) _ -> case scenarioEpic (toAttrs g.scenario) of
    Just replica | nonce `Set.member` replica.epicPendingRequests -> do
      runQueueT $ traverse_ addToStaging card
      let updated = replica {epicPendingRequests = Set.delete nonce replica.epicPendingRequests}
      pure $ g & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just updated})
    _ -> pure g
  MoveAnnuminasEnemy eid destination arrival | Just foe <- Map.lookup eid g.entities.enemies -> do
    let replica = scenarioEpic $ toAttrs g.scenario
        own = maybe (GroupOrdinal 0) epicGroup replica
    if destination == own
      then do
        runQueueT do
          placeEnemy eid StagingArea
          when (arrival == EpicAttackFirstPlayer) do
            lead <- getLead
            immediateAttack lead eid
        pure g
      else case replica of
        Nothing -> pure g
        Just _ -> do
          let a = toAttrs foe
              carried effect = effect.effectTarget `elem` [EnemyTarget eid, CardIdTarget a.cardId]
              transfer = EpicEnemyTransfer eid (MkEncounterCard a.cardId $ toCardCode foe)
                a.enemyTokens a.enemyShadowCards a.enemyShadowRevealed a.enemyShadowFaceUp
                a.enemyFacedown a.enemyOwner (filter carried $ gameEffects g)
          push $ RequestEpicEffect destination (toCardCode foe) (EpicReceiveEnemy transfer arrival) ScenarioTarget
          -- Moving between stages is neither leaving nor entering play. The
          -- request waits for the arrival attack before the source continues.
          pure $ g & entitiesL . enemiesL %~ Map.delete eid & effectsL %~ filter (not . carried)
  ResolveEpicEffect code (EpicResolveWithReply origin nonce effect) target -> do
    -- Queue the acknowledgement after the complete native effect chain. It
    -- stays behind any choices in the recipient's persisted local queue.
    runQueueT do
      when (presentEpicTarget g target) $ push $ ResolveEpicEffect code effect target
      push $ QueueEpicCommand origin code (EpicAcknowledge nonce) ScenarioTarget
    pure g
  ResolveEpicEffect _ (EpicAcknowledge nonce) _ ->
    pure $ g & scenarioL %~ overAttrs (\attrs -> attrs
      {scenarioEpic = fmap (\replica -> replica {epicPendingRequests = Set.delete nonce replica.epicPendingRequests}) attrs.scenarioEpic})
  ResolveEpicEffect _ (EpicReceiveEnemy transfer arrival) _ -> do
    when (Map.member transfer.epicEnemyId g.entities.enemies) $
      throwIO $ userError "Epic enemy identity already exists at the destination"
    let foe = overAttrs (\a -> a
          { enemyPlacement = StagingArea
          , enemyTokens = transfer.epicEnemyTokens
          , enemyShadowCards = transfer.epicEnemyShadows
          , enemyShadowRevealed = transfer.epicEnemyRevealedShadows
          , enemyShadowFaceUp = transfer.epicEnemyFaceupShadows
          , enemyFacedown = transfer.epicEnemyFacedown
          , enemyOwner = transfer.epicEnemyOwner
          }) $ createEnemy transfer.epicEnemyCard transfer.epicEnemyId
    when (arrival == EpicAttackFirstPlayer) $ runQueueT do
      lead <- getLead
      immediateAttack lead transfer.epicEnemyId
    pure $ g & entitiesL . enemiesL %~ insertEntity foe & effectsL <>~ transfer.epicEnemyEffects
  ResolveEpicEffect _ (EpicTransferLorienSupport destination recipient controller) target -> transferLorienSupport destination recipient controller target g
  ResolveEpicEffect _ (EpicReceiveLorienSupport packet recipient) _ -> receiveLorienSupport packet recipient g
  SyncEpic shared -> do
    runQueueT $ traverse_ gameOver $ epicOutcome shared
    pure g
  ContributeShared key _ | key `elem` [AnnuminasCityStrength, AnnuminasVictory, AnnuminasDefeat, DolGuldurCityStrength, DolGuldurVictory, DolGuldurDefeat] -> do
    runQueueT $ traverse_ gameOver $ scenarioEpic (toAttrs g.scenario) >>= epicOutcome . epicShared
    pure g
  ChooseEpicTarget pid code query effect -> do
    let replica = scenarioEpic $ toAttrs g.scenario
        own = maybe (GroupOrdinal 0) epicGroup replica
        boards =
          epicBoardFor own "" g : [b | r <- maybeToList replica, b <- r.epicBoards, b.epicBoardGroup /= own]
        eligibleBoards = [b | b <- boards, query /= EpicOtherNonUniqueNoVictoryEnemy || b.epicBoardGroup /= own]
        candidates =
          [ (board, target) | board <- eligibleBoards, target <- board.epicBoardTargets, matchesEpicTarget query target
          ]
        resolve board target
          | board.epicBoardGroup == own = push $ ResolveEpicEffect code effect target
          | otherwise = push $ RequestEpicEffect board.epicBoardGroup code effect target
    runQueueT $ chooseOneRebuildingM pid msg do
      questionLabeled "epic.chooseTarget"
      if query `elem` [EpicWarshipGroup, EpicHealingGroup, EpicDrawingGroup]
        then for_ (filter (matchesEpicGroup query) boards) \board ->
          if board.epicBoardGroup == own
            then labeled "epic.thisGroup" $ resolve board ScenarioTarget
            else labeledWith "epic.group" [textVar "group" board.epicBoardName] $ resolve board ScenarioTarget
        else do
          showing [EncounterCard target.epicTargetCard | (_, target) <- candidates]
          for_ candidates \(board, target) ->
            if board.epicBoardGroup == own
              then labeledWith "epic.localTarget" (cardVar "card" target.epicTargetCard) $ resolve board target.epicTarget
              else labeledWith "epic.target" (cardVar "card" target.epicTargetCard <> [textVar "group" board.epicBoardName]) $ resolve board target.epicTarget
    pure g
  ResolveEpicEffect code effect target -> do
    let source = CardCodeSource code
    runQueueT $ case effect of
      -- Coordinator effects are intercepted under the event lock before
      -- delivery, including requests originating at coordinator group zero.
      EpicSubmitDolClaim {} -> pure ()
      EpicDolStepDone {} -> pure ()
      EpicSendSetAsideHost destination nonce -> do
        let host = headMay $ filter ((== toCardCode Enemies.hostOfAngmar) . toCardCode) $ scenarioSetAside $ toAttrs g.scenario
        for_ host $ \card -> push $ RemoveSetAsideFromGame [card]
        push $ QueueEpicCommand destination code (EpicReceiveSetAsideHost nonce host) ScenarioTarget
      EpicDiscardEnemy -> discard source target
      EpicDestroyEnemy -> destroy source target
      EpicDolDrawResource -> eachPlayer $ \pid -> do
        drawCards source pid 1
        heroes <- filterM (fmap not . resourcesBarredFrom source) =<< select (HeroControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "annuminas.resourceHero"
          targets heroes $ \hid -> placeResources source hid 1
      EpicDolDiscardToOne -> eachPlayer $ \pid -> do
        handSize <- length . (.hand) <$> getPlayer pid
        chooseAndDiscardFromHand pid source $ max 0 $ handSize - 1
      EpicDolDiscardAlly -> eachPlayer $ \pid -> do
        allies <- select $ AllyControlledBy $ PlayerWithId pid
        chooseOrRunOneM pid do
          questionLabeled "dolGuldur.discardAlly"
          targets allies $ discard source
      EpicDolSearchCard -> case target of
        PlayerTarget pid -> searchDeck pid source maxBound AnyCard False
        _ -> pure ()
      EpicDolSearchAlly -> case target of
        PlayerTarget pid -> searchDeckIntoPlay pid source 10 (CardWithType AllyType) 1 []
        _ -> pure ()
      EpicDolEngageEnemies -> eachPlayer $ \pid -> push $ ResolveEpicEffect code EpicDolEngageEnemy $ PlayerTarget pid
      EpicDolEngageEnemy -> case target of
        PlayerTarget pid -> do
          let attrs = toAttrs g.scenario
              deck = attrs.encounterDeck
              discarded = attrs.encounterDiscard
              enemyCard = cardMatches $ CardWithType EnemyType
          case deck of
            [] -> when (any enemyCard discarded) do
              shuffleEncounterDiscardIntoDeck AnyCard
              push msg
            card : remaining -> do
              discardTopOfEncounterDeckFor source target 1
              when (null remaining) $ shuffleEncounterDiscardIntoDeck AnyCard
              if enemyCard card
                then do
                  removeFromEncounterDeck card
                  removeFromEncounterDiscard card
                  putEnemyIntoPlayEngaged pid card
                else when (any enemyCard $ remaining <> discarded) $ push msg
        _ -> pure ()
      EpicDrawCards -> eachPlayer $ \pid -> drawCards source pid 1
      EpicDamage n -> dealDamage source target n
      EpicProgress n -> placeProgress source target n
      EpicDiscardUntilEnemy -> do
        let deck = (toAttrs g.scenario).encounterDeck
            enemyCard = cardMatches (CardWithType EnemyType)
            discardCount = maybe (length deck) (+ 1) $ findIndex enemyCard deck
        discardTopOfEncounterDeckFor source ScenarioTarget discardCount
        for_ (find enemyCard deck) addToStagingFromDiscard
      EpicHealControlledCharacter -> eachPlayer $ \pid -> do
        characters <- healableCharacters source (HeroControlledBy $ PlayerWithId pid) (AllyControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "annuminas.healCharacter"
          targets characters $ \character -> healDamage source character 1
      EpicGrantHeroResources -> eachPlayer $ \pid -> do
        heroes <- filterM (fmap not . resourcesBarredFrom source) =<< select (HeroControlledBy $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "annuminas.resourceHero"
          targets heroes $ \hid -> placeResources source hid 1
      EpicGrantDunedain -> do
        lead <- getLead
        for_ (take 1 $ filter ((== toCardCode Objectives.dunedainOfAnnuminas) . toCardCode) $ scenarioSetAside $ toAttrs g.scenario) $ putObjectiveAllyIntoPlayUnder lead
      EpicDelayStaging ->
        push $ CreateEffect $ Effect source GameTarget [FewerStagingReveals 1] (UntilEndOfPhaseOf QuestPhase)
      EpicCaptureHero ->
        for_ (toList g.entities.quests) \stage ->
          when (not (isSideQuest stage) && cdStage (toCardDef stage) == Just 3) $
            eachPlayer $ \pid -> do
              heroes <- select (HeroControlledBy $ PlayerWithId pid)
              chooseOrRunOneM pid do
                questionLabeled "annuminas.captureHero"
                targets heroes $ \hid -> push $ CaptureHeroUnderQuest (toId stage) hid
      EpicDiscardCaptive -> do
        for_ (toList g.entities.quests) \stage ->
          when (not (isSideQuest stage) && cdStage (toCardDef stage) == Just 3) do
            captives <- shuffleM [c | PlayerCard c <- questUnderneath (toAttrs stage), cdCardType (toCardDef c) `elem` [HeroType, ObjectiveHeroType]]
            for_ (take 1 captives) \card -> do
              takeFromUnderneath stage card.id
              putInDiscard card
        push CheckEliminations
      EpicRescueCaptives ->
        for_ (toList g.entities.quests) \stage ->
          when (not (isSideQuest stage) && cdStage (toCardDef stage) == Just 3) do
            for_ [c | PlayerCard c <- questUnderneath (toAttrs stage), cdCardType (toCardDef c) `elem` [HeroType, ObjectiveHeroType]] \card ->
              for_ card.owner \pid ->
                when (maybe False (not . (.eliminated)) $ Map.lookup pid g.players) do
                  takeFromUnderneath stage card.id
                  push $ PutHeroIntoPlay pid card
      EpicUpgradeShip -> case target of
        AllyTarget aid | Just shipAlly <- lookup aid $ mapToList g.entities.allies -> do
          healDamage source target 5
          -- An encounter effect readies, even if player-card readying is barred.
          encounterReadies target
          for_ (cdOtherSide $ toCardDef shipAlly) \other ->
            when
              (Basic `member` cdCardTraits (toCardDef shipAlly) && Upgraded `member` cdCardTraits (toCardDef other))
              $ flipOver target
        _ -> pure ()
      EpicShuffleWarship ->
        for_
          ( take 1
              $ filter
                ((== toCardCode Objectives.gondorianWarship) . toCardCode)
                (scenarioSetAside $ toAttrs g.scenario)
          )
          \card ->
            shuffleSetAsideIntoEncounterDeck [card]
    pure g
  ApplyEpicCommand command -> case scenarioEpic (toAttrs g.scenario) of
    Just replica
      | replica.epicGroup == command.epicCommandTo
      , command.epicCommandId `Set.notMember` replica.epicAppliedCommands -> do
          let hasReply = case command.epicCommandEffect of EpicResolveWithReply {} -> True; _ -> False
              present = hasReply || presentEpicTarget g command.epicCommandTarget
          when present $ push $ ResolveEpicEffect command.epicCommandCard command.epicCommandEffect command.epicCommandTarget
          let updated = replica {epicAppliedCommands = Set.insert command.epicCommandId replica.epicAppliedCommands}
          pure $ g & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just updated})
    _ -> pure g
  _ -> pure g

-- Record the shared terminal event before clearing the local queue. A queued
-- contribution after GameOver would never execute.
finalizeEpicOutcome :: Outcome -> Game -> GameT Game
finalizeEpicOutcome requested g = do
  updated <- case scenarioEpic $ toAttrs g.scenario of
    Just replica | isJust (annuminasCityStrength replica.epicShared) || isJust (dolGuldurCityStrength replica.epicShared)
      , isNothing (epicOutcome replica.epicShared) -> do
      let key
            | isJust (dolGuldurCityStrength replica.epicShared) = if requested == Won then DolGuldurVictory else DolGuldurDefeat
            | otherwise = if requested == Won then AnnuminasVictory else AnnuminasDefeat
      attrs <- runQueueT $ runMessage (ContributeShared key 1) (toAttrs g.scenario)
      pure $ g & scenarioL %~ overAttrs (const attrs)
    _ -> pure g
  let authoritative = scenarioEpic (toAttrs updated.scenario) >>= epicOutcome . epicShared
  pure $ updated & outcomeL ?~ fromMaybe requested authoritative
