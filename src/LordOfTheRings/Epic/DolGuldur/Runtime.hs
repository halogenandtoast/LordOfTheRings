module LordOfTheRings.Epic.DolGuldur.Runtime
  ( dolOrdinaryBlocked ) where

import LordOfTheRings.Classes.Entity (toAttrs)
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
