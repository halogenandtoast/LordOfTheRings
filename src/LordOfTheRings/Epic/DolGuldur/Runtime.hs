module LordOfTheRings.Epic.DolGuldur.Runtime
  ( dolOrdinaryBlocked, dolClaimDrainable, dolRoundStateForDrain ) where

import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Epic.DolGuldur qualified as Dol
import LordOfTheRings.Epic.Types
import LordOfTheRings.Game.Base (Game (..))
import LordOfTheRings.Message (Message (AwaitEpicRound, AwaitEpicReply))
import LordOfTheRings.Question (questionRebuild)
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Prelude

-- Only coordinator-held claim replies are inert. Attacks, searches, ordinary
-- cross-team requests, and uncommitted outboxes must finish before continuing.
dolOrdinaryBlocked :: SharedEventState -> GroupOrdinal -> Game -> Bool
dolOrdinaryBlocked shared own g =
  any (not . inert . questionRebuild) (toList g.question)
    || maybe False (\r -> any (`notElem` held) (toList r.epicPendingRequests)
      || not (null r.epicPendingCommands && null r.epicPendingDeltas)) replica
 where
  replica = scenarioEpic $ toAttrs g.scenario
  held = [nonce | state <- maybeToList shared.dolGuldurCoordination, (team, nonce) <- state.epicDolClaimReplies, team == own]
  inert (Just AwaitEpicRound) = True
  inert (Just (AwaitEpicReply nonce)) = nonce `elem` held
  inert _ = False

-- During the initial transition, a team can reach stage two and reserve its
-- final quest before later teams have resolved their frozen stage-one effects.
-- This permits releasing those later effects, never expiring the round.
dolClaimDrainable :: SharedEventState -> GroupOrdinal -> Game -> Bool
dolClaimDrainable shared own g = case shared.dolGuldurCoordination of
  Nothing -> False
  Just state -> Dol.dolClaimAllowsRoundDrain onlyClaim own state.epicDolTransition
 where
  onlyClaim = not (dolOrdinaryBlocked shared own g)
    && any (\q -> case questionRebuild q of Just (AwaitEpicReply _) -> True; _ -> False) (toList g.question)

dolRoundStateForDrain :: SharedEventState -> GroupOrdinal -> Game -> Maybe EpicRoundState
dolRoundStateForDrain shared own g = do
  replica <- scenarioEpic $ toAttrs g.scenario
  roundState <- replica.epicRoundState
  pure $ if roundState.epicRoundPhase == EpicRoundResolving && dolClaimDrainable shared own g
    then roundState {epicRoundPhase = EpicRoundEnded}
    else roundState
