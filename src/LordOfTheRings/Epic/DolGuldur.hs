-- | Pure decisions for Attack on Dol Guldur's three-team transitions. The
-- event-row transaction records the returned state and its durable deliveries
-- together. No game advances a physical stage until every claim is accepted,
-- and no stage is revealed until all three replacements are prepared.
module LordOfTheRings.Epic.DolGuldur
  ( DolEpochKind (..), DolEpoch (..), DolClaim (..), DolPhase (..)
  , DolTransition (..), DolEligibility (..), DolStep (..), DolAck (..), DolError (..)
  , dolFinalStages, beginDolTransition, validateDolTransition, acceptDolClaim
  , availableDolStages, nextDolStep, ackDolStep, dolClaimAllowsRoundDrain
  , dolAllStagesRevealed, dolTransitionFinished
  ) where

import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Prelude
import Data.Either (isRight)

data DolEpochKind = DolInitialStages | DolRotateStages
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DolEpoch = DolEpoch
  { dolEpochRound :: Int
  , dolEpochKind :: DolEpochKind
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DolClaim = DolClaim
  { dolClaimEpoch :: DolEpoch
  , dolClaimStage :: CardCode
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DolPhase
  = DolWaitingForForces
  | DolResolvingCity
  | DolWaitingForUnusedStage
  | DolResolvingUnusedStage
  | DolCollectingClaims
  | DolPreparingStages
  | DolRevealingStages
  | DolDrainingQueues
  | DolTransitionDone
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- Polymorphic team identities keep this module independent of Epic.Types,
-- allowing that module to persist DolTransition GroupOrdinal without a cycle.
-- Association lists also avoid a JSON object-key requirement for team types.
data DolTransition team = DolTransition
  { dolTransitionEpoch :: DolEpoch
  , dolCityHolder :: team
  , dolPreviousStages :: [(team, Maybe CardCode)]
  , dolTransitionPhase :: DolPhase
  , dolAcceptedClaims :: [(team, CardCode)]
  , dolPreparedTeams :: [team]
  , dolRevealedTeams :: [team]
  , dolRevealingTeam :: Maybe team
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data DolEligibility = DolEligibility
  { dolEventActive :: Bool
  , dolConsequencesSettled :: Bool
  -- ^ Ordinary questions and deliveries are settled. Only coordinator-held
  -- claim nonces and their inert barrier questions may be excluded here.
  , dolOldStageForcesComplete :: Bool
  , dolOldEndRoundQueuesSettled :: Bool
  }
  deriving stock (Show, Eq)

data DolStep team
  = ResolveDolCity team
  | ResolveUnusedDolStage team CardCode
  | PrepareDolTeams [(team, CardCode)]
  | RevealDolTeam team CardCode
  | ReleaseDolTeams
  -- ^ Resume the appropriate old round phase before acknowledging held claim
  -- requests. Initial stage-one queues must finish; rotation queues already did.
  | FinishDolTransition
  -- ^ Reset shared Siege for the next round before the generic round coordinator
  -- expires every team's round effects and releases their next BeginRound.
  deriving stock (Show, Eq)

data DolAck team
  = DolCityResolved
  | DolUnusedStageResolved
  | DolTeamPrepared team
  | DolTeamRevealed team
  deriving stock (Show, Eq)

data DolError
  = DolInvalidEpoch
  | DolInvalidTeams
  | DolInvalidCityHolder
  | DolInvalidPreviousStages
  | DolInvalidClaims
  | DolInvalidPreparation
  | DolInvalidRevelation
  | DolInvalidPhase
  | DolStaleEpoch
  | DolUnknownTeam
  | DolUnknownStage CardCode
  | DolCurrentStage CardCode
  | DolStageAlreadyClaimed CardCode
  | DolClaimAlreadyAccepted CardCode
  | DolClaimsNotOpen
  | DolUnexpectedAcknowledgement
  deriving stock (Show, Eq)

-- The four physical stage-three cards, whose B/D/F/H sides determine order.
dolFinalStages :: [CardCode]
dolFinalStages =
  [ "attack-on-dol-guldur-003", "attack-on-dol-guldur-004"
  , "attack-on-dol-guldur-005", "attack-on-dol-guldur-006"
  ]

beginDolTransition
  :: Eq team
  => DolEpoch -> team -> [(team, Maybe CardCode)]
  -> Either DolError (DolTransition team)
beginDolTransition epoch holder previous = do
  let transition = DolTransition epoch holder previous
        (if dolEpochKind epoch == DolInitialStages then DolCollectingClaims else DolWaitingForForces)
        [] [] [] Nothing
  validateDolTransition transition
  pure transition

validateDolTransition :: Eq team => DolTransition team -> Either DolError ()
validateDolTransition transition = do
  let teams = map fst transition.dolPreviousStages
      claims = transition.dolAcceptedClaims
      claimedTeams = map fst claims
      stages = map snd claims
      prepared = transition.dolPreparedTeams
      revealed = transition.dolRevealedTeams
      unique values = length (nub values) == length values
      previousFinals = [stage | (_, Just stage) <- transition.dolPreviousStages, stage `elem` dolFinalStages]
  unless (transition.dolTransitionEpoch.dolEpochRound >= 0) $ Left DolInvalidEpoch
  unless (length teams == 3 && unique teams) $ Left DolInvalidTeams
  unless (transition.dolCityHolder `elem` teams) $ Left DolInvalidCityHolder
  when (transition.dolTransitionEpoch.dolEpochKind == DolRotateStages) $
    unless (length previousFinals == 3 && unique previousFinals) $ Left DolInvalidPreviousStages
  unless (unique claimedTeams && unique stages
    && all (`elem` teams) claimedTeams
    && all (`elem` dolFinalStages) stages
    && all (\(team, stage) -> lookup team transition.dolPreviousStages /= Just (Just stage)) claims) $
    Left DolInvalidClaims
  unless (unique prepared && all (`elem` claimedTeams) prepared) $ Left DolInvalidPreparation
  unless (unique revealed && all (`elem` prepared) revealed
    && maybe True (\team -> team `elem` prepared && team `notElem` revealed) transition.dolRevealingTeam) $
    Left DolInvalidRevelation
  let noPreparation = null prepared && null revealed && isNothing transition.dolRevealingTeam
      allClaimed = length claims == 3
      allPrepared = length prepared == 3
      allRevealed = length revealed == 3 && isNothing transition.dolRevealingTeam
      beforeClaims = transition.dolTransitionPhase `elem`
        [DolWaitingForForces, DolResolvingCity, DolWaitingForUnusedStage, DolResolvingUnusedStage]
      phaseValid
        | beforeClaims = null claims && noPreparation && transition.dolTransitionEpoch.dolEpochKind == DolRotateStages
        | transition.dolTransitionPhase == DolCollectingClaims = noPreparation
        | transition.dolTransitionPhase == DolPreparingStages = allClaimed && null revealed && isNothing transition.dolRevealingTeam
        | transition.dolTransitionPhase == DolRevealingStages = allClaimed && allPrepared
        | otherwise = allClaimed && allPrepared && allRevealed
  unless phaseValid $ Left DolInvalidPhase

-- | Called under the event lock. A replay of the accepted choice is harmless,
-- even after preparation started; a conflicting or stale request changes nothing.
-- Old stages belonging to OTHER teams do not reserve new assignments: swaps
-- between teams are legal. Only claims accepted in this epoch reserve a stage.
acceptDolClaim
  :: Eq team => team -> DolClaim -> DolTransition team
  -> Either DolError (DolTransition team)
acceptDolClaim team request transition = do
  validateDolTransition transition
  unless (request.dolClaimEpoch == transition.dolTransitionEpoch) $ Left DolStaleEpoch
  unless (team `elem` map fst transition.dolPreviousStages) $ Left DolUnknownTeam
  case lookup team transition.dolAcceptedClaims of
    Just accepted
      | accepted == request.dolClaimStage -> pure transition
      | otherwise -> Left $ DolClaimAlreadyAccepted accepted
    Nothing -> do
      unless (transition.dolTransitionPhase == DolCollectingClaims) $ Left DolClaimsNotOpen
      unless (request.dolClaimStage `elem` dolFinalStages) $ Left $ DolUnknownStage request.dolClaimStage
      when (lookup team transition.dolPreviousStages == Just (Just request.dolClaimStage)) $
        Left $ DolCurrentStage request.dolClaimStage
      when (request.dolClaimStage `elem` map snd transition.dolAcceptedClaims) $
        Left $ DolStageAlreadyClaimed request.dolClaimStage
      pure transition {dolAcceptedClaims = transition.dolAcceptedClaims <> [(team, request.dolClaimStage)]}

availableDolStages :: Eq team => team -> DolTransition team -> Either DolError [CardCode]
availableDolStages team transition = do
  validateDolTransition transition
  unless (team `elem` map fst transition.dolPreviousStages) $ Left DolUnknownTeam
  pure
    [ stage
    | transition.dolTransitionPhase == DolCollectingClaims
    , isNothing $ lookup team transition.dolAcceptedClaims
    , stage <- dolFinalStages
    , lookup team transition.dolPreviousStages /= Just (Just stage)
    , stage `notElem` map snd transition.dolAcceptedClaims
    ]

-- | The returned state already records the in-flight action. Persist it and
-- its deliveries atomically. Re-running the planner then waits for the native
-- acknowledgement instead of issuing the same City/unused/reveal twice.
nextDolStep
  :: Eq team => DolEligibility -> DolTransition team
  -> Either DolError (Maybe (DolStep team, DolTransition team))
nextDolStep eligibility transition = do
  validateDolTransition transition
  if not (eligibility.dolEventActive && eligibility.dolConsequencesSettled)
    then pure Nothing
    else case transition.dolTransitionPhase of
      DolWaitingForForces | eligibility.dolOldStageForcesComplete ->
        issue (ResolveDolCity transition.dolCityHolder) $ transition {dolTransitionPhase = DolResolvingCity}
      DolWaitingForUnusedStage -> case filter (`notElem` mapMaybe snd transition.dolPreviousStages) dolFinalStages of
        [unused] -> issue (ResolveUnusedDolStage transition.dolCityHolder unused) $ transition {dolTransitionPhase = DolResolvingUnusedStage}
        _ -> Left DolInvalidPreviousStages
      DolCollectingClaims | length transition.dolAcceptedClaims == 3 ->
        issue (PrepareDolTeams $ sortOn snd transition.dolAcceptedClaims) $ transition {dolTransitionPhase = DolPreparingStages}
      DolPreparingStages | length transition.dolPreparedTeams == 3 -> revealNext
      DolRevealingStages | isNothing transition.dolRevealingTeam ->
        if length transition.dolRevealedTeams == 3
          then issue ReleaseDolTeams $ transition {dolTransitionPhase = DolDrainingQueues}
          else revealNext
      DolDrainingQueues | eligibility.dolOldEndRoundQueuesSettled ->
        issue FinishDolTransition $ transition {dolTransitionPhase = DolTransitionDone}
      _ -> pure Nothing
 where
  issue step updated = pure $ Just (step, updated)
  revealNext = case find (\(team, _) -> team `notElem` transition.dolRevealedTeams) $ sortOn snd transition.dolAcceptedClaims of
    Nothing -> Left DolInvalidRevelation
    Just (team, stage) ->
      issue (RevealDolTeam team stage) $ transition {dolTransitionPhase = DolRevealingStages, dolRevealingTeam = Just team}

ackDolStep
  :: Eq team => DolEpoch -> DolAck team -> DolTransition team
  -> Either DolError (DolTransition team)
ackDolStep epoch acknowledgement transition = do
  validateDolTransition transition
  unless (epoch == transition.dolTransitionEpoch) $ Left DolStaleEpoch
  case acknowledgement of
    DolCityResolved
      | epoch.dolEpochKind /= DolRotateStages -> Left DolUnexpectedAcknowledgement
      | transition.dolTransitionPhase == DolResolvingCity -> pure transition {dolTransitionPhase = DolWaitingForUnusedStage}
      | transition.dolTransitionPhase `elem` [DolWaitingForUnusedStage, DolResolvingUnusedStage, DolCollectingClaims, DolPreparingStages, DolRevealingStages, DolDrainingQueues, DolTransitionDone] -> pure transition
    DolUnusedStageResolved
      | epoch.dolEpochKind /= DolRotateStages -> Left DolUnexpectedAcknowledgement
      | transition.dolTransitionPhase == DolResolvingUnusedStage -> pure transition {dolTransitionPhase = DolCollectingClaims}
      | transition.dolTransitionPhase `elem` [DolCollectingClaims, DolPreparingStages, DolRevealingStages, DolDrainingQueues, DolTransitionDone] -> pure transition
    DolTeamPrepared team
      | team `elem` transition.dolPreparedTeams -> pure transition
      | transition.dolTransitionPhase == DolPreparingStages
      , isJust (lookup team transition.dolAcceptedClaims) -> pure transition {dolPreparedTeams = transition.dolPreparedTeams <> [team]}
    DolTeamRevealed team
      | team `elem` transition.dolRevealedTeams -> pure transition
      | transition.dolTransitionPhase == DolRevealingStages
      , transition.dolRevealingTeam == Just team -> pure transition
          {dolRevealedTeams = transition.dolRevealedTeams <> [team], dolRevealingTeam = Nothing}
    _ -> Left DolUnexpectedAcknowledgement

-- | Only an accepted INITIAL-stage claim at the explicit inert claim barrier
-- lets a later team's old frozen stage-one end-of-round effects run. This is
-- never permission to expire effects or begin another round.
dolClaimAllowsRoundDrain :: Eq team => Bool -> team -> DolTransition team -> Bool
dolClaimAllowsRoundDrain onlyClaimBarrier team transition =
  isRight (validateDolTransition transition)
    && onlyClaimBarrier
    && transition.dolTransitionEpoch.dolEpochKind == DolInitialStages
    && transition.dolTransitionPhase == DolCollectingClaims
    && isJust (lookup team transition.dolAcceptedClaims)

dolAllStagesRevealed :: DolTransition team -> Bool
dolAllStagesRevealed transition = length transition.dolRevealedTeams == 3 && isNothing transition.dolRevealingTeam

dolTransitionFinished :: DolTransition team -> Bool
dolTransitionFinished = (== DolTransitionDone) . dolTransitionPhase
