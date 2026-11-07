module LordOfTheRings.Epic.Runner (runEpicMessage) where

import Data.Set qualified as Set
import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity (overAttrs, toAttrs)
import LordOfTheRings.Classes.HasQueue (push, runQueueT)
import LordOfTheRings.Epic.Board
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Base
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.I18n (cardVar, textVar)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic, scenarioSetAside))
import LordOfTheRings.Source (Source (CardCodeSource))
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Basic, Upgraded))

runEpicMessage :: Message -> Game -> GameT Game
runEpicMessage msg g = case msg of
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
      if query == EpicWarshipGroup
        then for_ (filter epicBoardHasWarship boards) \board ->
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
      EpicDamage n -> dealDamage source target n
      EpicProgress n -> placeProgress source target n
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
