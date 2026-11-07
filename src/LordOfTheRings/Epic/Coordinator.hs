module LordOfTheRings.Epic.Coordinator (EpicRoundStep (..), nextEpicRoundStep, epicRoundBlocked) where

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
