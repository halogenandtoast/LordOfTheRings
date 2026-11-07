module LordOfTheRings.Epic.Coordinator (EpicRoundStep (..), nextEpicRoundStep, epicRoundBlocked, annuminasStageTwoReady) where

import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Game.Base (Game (..))
import LordOfTheRings.Message (Message (AwaitEpicRound))
import LordOfTheRings.Question (questionRebuild)
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Prelude

-- These steps are committed with all group games under the event row lock.
-- Each step waits for the prior step's questions and cross-team deliveries.
data EpicRoundStep
  = FreezeEpicTeams
  | ResolveEpicTeam GroupOrdinal
  | ExpireEpicTeams
  | BeginEpicTeams
  | FreezeEpicBeginningTeams
  | ReleaseEpicBeginningTeams
  | RevealEpicTeams
  | ReleaseEpicResources
  deriving stock (Show, Eq)

nextEpicRoundStep
  :: [(GroupOrdinal, Maybe EpicRoundState, [CardCode], Bool)]
  -> Maybe EpicRoundStep
nextEpicRoundStep teams = do
  guard $ length teams == 3 && length (nub [ordinal | (ordinal, _, _, _) <- teams]) == 3
  guard $ not $ any (\(_, _, _, pending) -> pending) teams
  states <- traverse (\(_, state, _, _) -> state) teams
  guard $ length (nub $ map epicRoundNumber states) == 1
  let phases = map epicRoundPhase states
      allPhase phase = all (== phase) phases
      ordered = sortOn (\(ordinal, _, codes, _) -> (minimumMay codes, ordinal)) teams
  if | allPhase EpicRoundWaiting -> Just FreezeEpicTeams
     | allPhase EpicRoundEnded -> Just ExpireEpicTeams
     | allPhase EpicRoundExpired -> Just BeginEpicTeams
     | allPhase EpicRoundBeginning -> Just FreezeEpicBeginningTeams
     | allPhase EpicRoundBeginReady -> Just ReleaseEpicBeginningTeams
     | allPhase EpicRoundPrepared -> Just RevealEpicTeams
     | allPhase EpicRoundRevealed -> Just ReleaseEpicBeginningTeams
     | allPhase EpicRoundBegan -> Just ReleaseEpicResources
     | all (`elem` [EpicRoundReady, EpicRoundEnded]) phases -> do
         (ordinal, _, _, _) <- find (\(_, state, _, _) -> fmap epicRoundPhase state == Just EpicRoundReady) ordered
         pure $ ResolveEpicTeam ordinal
     | otherwise -> Nothing

-- A remote consequence can ask a Ready team to defend an attack. Only the
-- round-barrier question is inert; every other question and reply must settle.
epicRoundBlocked :: Game -> Bool
epicRoundBlocked g =
  any ((/= Just AwaitEpicRound) . questionRebuild) (toList g.question)
    || maybe False (\replica -> not (null replica.epicPendingRequests && null replica.epicPendingCommands && null replica.epicPendingDeltas)) (scenarioEpic $ toAttrs g.scenario)

-- The Epic setup reverse requires every stage two to be complete at the same
-- beginning of the round. These are fresh boards, including shared Host VPs.
annuminasStageTwoReady :: [EpicBoard] -> Bool
annuminasStageTwoReady boards =
  length boards == 3
    && length (nub $ map epicBoardGroup boards) == 3
    && all ready boards
 where
  ready board = case board.epicBoardStage of
    Just stage -> case stage.epicStageQuests of
      [q] -> q.epicQuestNumber == 2 && q.epicQuestFlipped
        && maybe False (q.epicQuestProgress >=) q.epicQuestRequirement
      _ -> False
    Nothing -> False
