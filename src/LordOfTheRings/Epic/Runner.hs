module LordOfTheRings.Epic.Runner (runEpicMessage) where

import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity (overAttrs, toAttrs, insertEntity)
import LordOfTheRings.Classes.HasQueue (push, runQueueT)
import LordOfTheRings.Epic.Board
import LordOfTheRings.Epic.Types
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhaseOf))
import LordOfTheRings.Enemy (EnemyAttrs (..), createEnemy)
import LordOfTheRings.Entities (enemiesL)
import LordOfTheRings.Game.Base
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.I18n (cardVar, textVar)
import LordOfTheRings.Matcher.Types (CardMatcher (CardWithType), cardMatches)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Quest (isSideQuest)
import LordOfTheRings.Quest.Types (questUnderneath)
import LordOfTheRings.Modifier (ModifierType (FewerStagingReveals))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Prelude
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Helpers.Modifiers (resourcesBarredFrom)
import LordOfTheRings.Helpers.Healing (healableCharacters)
import LordOfTheRings.Matcher (getLead, select, HeroMatcher (HeroControlledBy), AllyMatcher (AllyControlledBy), PlayerMatcher (PlayerWithId))
import LordOfTheRings.Scenario.Runner (epicMessageId)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Question (Question (PlayerWindow, QuestionLabel, ChooseOne))
import LordOfTheRings.I18n (ikey)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioSetAside))
import LordOfTheRings.Source (Source (CardCodeSource))
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Basic, Upgraded))

runEpicMessage :: Message -> Game -> GameT Game
runEpicMessage msg g = case msg of
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
          push $ QueueEpicCommand destination (toCardCode foe) (EpicReceiveEnemy transfer arrival) ScenarioTarget
          -- Moving between stages is neither leaving nor entering play.
          pure $ g & entitiesL . enemiesL %~ Map.delete eid & effectsL %~ filter (not . carried)
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
  SyncEpic shared -> do
    when (annuminasCityDefeated shared) $ runQueueT $ gameOver Lost
    pure g
  ContributeShared AnnuminasCityStrength _ -> do
    when (maybe False (annuminasCityDefeated . epicShared) $ scenarioEpic $ toAttrs g.scenario) $
      runQueueT $ gameOver Lost
    pure g
  ChooseEpicTarget pid code query effect -> do
    let replica = scenarioEpic $ toAttrs g.scenario
        own = maybe (GroupOrdinal 0) epicGroup replica
        boards =
          epicBoardFor own "" g : [b | r <- maybeToList replica, b <- r.epicBoards, b.epicBoardGroup /= own]
        candidates =
          [ (board, target) | board <- boards, target <- board.epicBoardTargets, matchesEpicTarget query target
          ]
        resolve board target
          | board.epicBoardGroup == own = push $ ResolveEpicEffect code effect target
          | otherwise = push $ QueueEpicCommand board.epicBoardGroup code effect target
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
      EpicSendSetAsideHost destination nonce -> do
        let host = headMay $ filter ((== toCardCode Enemies.hostOfAngmar) . toCardCode) $ scenarioSetAside $ toAttrs g.scenario
        for_ host $ \card -> push $ RemoveSetAsideFromGame [card]
        push $ QueueEpicCommand destination code (EpicReceiveSetAsideHost nonce host) ScenarioTarget
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
          let board = epicBoardFor replica.epicGroup "" g
              present = command.epicCommandTarget == ScenarioTarget
                || any ((== command.epicCommandTarget) . epicTarget) board.epicBoardTargets
          when present $ push $ ResolveEpicEffect command.epicCommandCard command.epicCommandEffect command.epicCommandTarget
          let updated = replica {epicAppliedCommands = Set.insert command.epicCommandId replica.epicAppliedCommands}
          pure $ g & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just updated})
    _ -> pure g
  _ -> pure g
