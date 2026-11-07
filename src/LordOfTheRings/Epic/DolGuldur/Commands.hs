-- | Pure event-lock handling for the durable Dol Guldur command outbox.
-- The caller saves canonical state and any response delivery atomically.
module LordOfTheRings.Epic.DolGuldur.Commands
  ( DolCommandResponse (..)
  , isDolCoordinatorCommand
  , startDolCoordination
  , handleDolCoordinatorCommand
  ) where

import Data.Set qualified as Set
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Epic.DolGuldur
import LordOfTheRings.Epic.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Target (Target (ScenarioTarget))

data DolCommandResponse
  = RejectDolClaim GroupOrdinal UUID
  | AcknowledgeDolClaim GroupOrdinal UUID
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

isDolCoordinatorCommand :: EpicCommand -> Bool
isDolCoordinatorCommand command = case command.epicCommandEffect of
  EpicDolStepDone {} -> True
  EpicResolveWithReply _ _ (EpicSubmitDolClaim {}) -> True
  EpicResolveWithReply _ _ (EpicDolStepDone {}) -> True
  _ -> False

startDolCoordination
  :: DolEpoch -> GroupOrdinal -> [(GroupOrdinal, Maybe CardCode)]
  -> SharedEventState -> Either Text SharedEventState
startDolCoordination epoch holder previous shared = do
  transition <- mapError $ beginDolTransition epoch holder previous
  pure shared
    { dolGuldurCoordination = Just $ EpicDolState transition [] mempty
    , revision = shared.revision + 1
    }

handleDolCoordinatorCommand
  :: Int -> GroupOrdinal -> [(GroupOrdinal, Maybe CardCode)] -> EpicCommand
  -> SharedEventState -> Either Text (SharedEventState, Maybe DolCommandResponse)
handleDolCoordinatorCommand currentRound holder previous command shared = do
  unless (isDolCoordinatorCommand command) $ Left "Not a Dol Guldur coordinator command"
  unless (command.epicCommandTo == holder && command.epicCommandTarget == ScenarioTarget) $
    Left "Dol Guldur coordinator command has the wrong destination"
  case command.epicCommandEffect of
    EpicResolveWithReply origin nonce effect -> do
      unless (origin == command.epicCommandFrom) $ Left "Dol Guldur reply origin differs from command sender"
      processEffect effect $ Just (origin, nonce)
    effect -> processEffect effect Nothing
 where
  processEffect effect reply
    | maybe False (Set.member command.epicCommandId . epicDolProcessedCommands) shared.dolGuldurCoordination =
        pure (shared, Nothing)
    | otherwise = case effect of
        EpicSubmitDolClaim request -> case reply of
          Nothing -> Left "Dol Guldur claims require a reply nonce"
          Just (origin, nonce)
            | request.dolClaimEpoch.dolEpochRound /= currentRound -> finish shared $ Just $ AcknowledgeDolClaim origin nonce
            | otherwise -> do
                initialized <- case shared.dolGuldurCoordination of
                  Just _ -> pure shared
                  Nothing
                    | request.dolClaimEpoch.dolEpochKind == DolInitialStages ->
                        startDolCoordination request.dolClaimEpoch holder previous shared
                    | otherwise -> Left "Dol Guldur rotation has not been started"
                state <- requireState initialized
                mapError $ validateDolTransition state.epicDolTransition
                if request.dolClaimEpoch /= state.epicDolTransition.dolTransitionEpoch
                  then finish initialized $ Just $ AcknowledgeDolClaim origin nonce
                  else case acceptDolClaim origin request state.epicDolTransition of
                    Left _ -> finish initialized $ Just $ RejectDolClaim origin nonce
                    Right accepted ->
                      let released = accepted.dolTransitionPhase `elem` [DolDrainingQueues, DolTransitionDone]
                          replies = if released then state.epicDolClaimReplies else nub $ state.epicDolClaimReplies <> [(origin, nonce)]
                          updated = initialized {dolGuldurCoordination = Just state {epicDolTransition = accepted, epicDolClaimReplies = replies}}
                       in finish updated $ if released then Just $ AcknowledgeDolClaim origin nonce else Nothing
        EpicDolStepDone epoch acknowledgement -> do
          case shared.dolGuldurCoordination of
            Just state
              | epoch.dolEpochRound == currentRound
              , epoch == state.epicDolTransition.dolTransitionEpoch -> do
                  acknowledged <- mapError $ ackDolStep epoch acknowledgement state.epicDolTransition
                  finish (shared {dolGuldurCoordination = Just state {epicDolTransition = acknowledged}}) $ uncurry AcknowledgeDolClaim <$> reply
            _ -> finish shared $ uncurry AcknowledgeDolClaim <$> reply
        _ -> Left "Invalid Dol Guldur coordinator effect"

  -- A coordinator-less stale request is acknowledged without installing an
  -- unrelated transition. Repeated ACKs are harmless at its nonce-guarded source.
  finish updated response =
    let recorded = updated {dolGuldurCoordination = fmap (\state -> state
          {epicDolProcessedCommands = Set.insert command.epicCommandId state.epicDolProcessedCommands}) updated.dolGuldurCoordination}
        changed = recorded.dolGuldurCoordination /= shared.dolGuldurCoordination
     in pure (recorded {revision = shared.revision + if changed then 1 else 0}, response)

  requireState state = maybe (Left "Missing Dol Guldur coordination") Right state.dolGuldurCoordination

mapError :: Show a => Either a b -> Either Text b
mapError = either (Left . pack . show) Right
