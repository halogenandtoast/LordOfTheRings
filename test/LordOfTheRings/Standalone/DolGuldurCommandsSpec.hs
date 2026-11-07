module LordOfTheRings.Standalone.DolGuldurCommandsSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Epic.DolGuldur
import LordOfTheRings.Epic.DolGuldur.Commands
import LordOfTheRings.Epic.Types
import LordOfTheRings.Prelude
import LordOfTheRings.Target (Target (ScenarioTarget))
import Test.Hspec

expectRight :: Show a => Either a b -> b
expectRight = either (error . show) id

identity :: Int -> UUID
identity value = UUID.fromWords 0 0 0 $ fromIntegral value

holder :: GroupOrdinal
holder = GroupOrdinal 0

teams :: [(GroupOrdinal, Maybe CardCode)]
teams = [(GroupOrdinal team, Nothing) | team <- [0, 1, 2]]

epoch :: DolEpoch
epoch = DolEpoch 3 DolInitialStages

emptyEvent :: SharedEventState
emptyEvent = fromMaybe (error "invalid event") $ newDolGuldurEvent 3

claimCommand :: Int -> GroupOrdinal -> DolEpoch -> CardCode -> EpicCommand
claimCommand serial origin requestedEpoch stage = EpicCommand (identity serial) origin holder stage
  (EpicResolveWithReply origin (identity $ serial + 1000) $ EpicSubmitDolClaim $ DolClaim requestedEpoch stage) ScenarioTarget

applyCommand :: EpicCommand -> SharedEventState -> (SharedEventState, Maybe DolCommandResponse)
applyCommand command = expectRight . handleDolCoordinatorCommand 3 holder teams command

coordination :: SharedEventState -> EpicDolState
coordination = fromMaybe (error "missing coordination") . (.dolGuldurCoordination)

transition :: SharedEventState -> DolTransition GroupOrdinal
transition = (.epicDolTransition) . coordination

spec :: Spec
spec = describe "Dol Guldur event-lock command handling" do
  it "accepts three concurrent claims in every arrival order, including coordinator group zero" do
    for_ (permutations [0, 1, 2]) $ \arrival -> do
      let commands = zipWith (\team stage -> claimCommand (team + 1) (GroupOrdinal team) epoch stage) [0, 1, 2] $ take 3 dolFinalStages
          selected team = fromMaybe (error "missing command") $ find ((== GroupOrdinal team) . (.epicCommandFrom)) commands
          final = foldl' (\shared team -> let (updated, response) = applyCommand (selected team) shared in if isNothing response then updated else error "accepted claim was not held") emptyEvent arrival
      sortOn fst (transition final).dolAcceptedClaims `shouldBe` zip (map GroupOrdinal [0, 1, 2]) (take 3 dolFinalStages)
      length (coordination final).epicDolClaimReplies `shouldBe` 3
      final.revision `shouldBe` 3
      final.counters `shouldBe` emptyEvent.counters
      final.appliedDeltas `shouldBe` emptyEvent.appliedDeltas

  it "rejects a conflicting reservation normally and lets the losing team select another stage" do
    let chosen = headEx dolFinalStages
        (accepted, _) = applyCommand (claimCommand 1 holder epoch chosen) emptyEvent
        (rejected, response) = applyCommand (claimCommand 2 (GroupOrdinal 1) epoch chosen) accepted
    response `shouldBe` Just (RejectDolClaim (GroupOrdinal 1) $ identity 1002)
    (transition rejected).dolAcceptedClaims `shouldBe` [(holder, chosen)]
    availableDolStages (GroupOrdinal 1) (transition rejected) `shouldBe` Right (drop 1 dolFinalStages)
    let (retried, retryResponse) = applyCommand (claimCommand 3 (GroupOrdinal 1) epoch $ headEx $ drop 1 dolFinalStages) rejected
    retryResponse `shouldBe` Nothing
    length (transition retried).dolAcceptedClaims `shouldBe` 2

  it "remembers exact deliveries and holds distinct retry nonces for an already accepted choice" do
    let command = claimCommand 1 holder epoch $ headEx dolFinalStages
        (accepted, _) = applyCommand command emptyEvent
    applyCommand command accepted `shouldBe` (accepted, Nothing)
    let (retried, response) = applyCommand (claimCommand 2 holder epoch $ headEx dolFinalStages) accepted
    response `shouldBe` Nothing
    (transition retried).dolAcceptedClaims `shouldBe` (transition accepted).dolAcceptedClaims
    (coordination retried).epicDolClaimReplies `shouldBe` [(holder, identity 1001), (holder, identity 1002)]
    applyCommand (claimCommand 2 holder epoch $ headEx dolFinalStages) retried `shouldBe` (retried, Nothing)

  it "acknowledges stale rounds or kinds without rebuilding choices or altering reservations" do
    let stale = claimCommand 1 holder (DolEpoch 2 DolInitialStages) $ headEx dolFinalStages
        (unchanged, response) = applyCommand stale emptyEvent
    unchanged `shouldBe` emptyEvent
    response `shouldBe` Just (AcknowledgeDolClaim holder $ identity 1001)
    let (accepted, _) = applyCommand (claimCommand 2 holder epoch $ headEx dolFinalStages) emptyEvent
        (staleKind, kindResponse) = applyCommand (claimCommand 3 (GroupOrdinal 1) (DolEpoch 3 DolRotateStages) $ lastEx dolFinalStages) accepted
    (transition staleKind).dolAcceptedClaims `shouldBe` (transition accepted).dolAcceptedClaims
    kindResponse `shouldBe` Just (AcknowledgeDolClaim (GroupOrdinal 1) $ identity 1003)
    applyCommand (claimCommand 3 (GroupOrdinal 1) (DolEpoch 3 DolRotateStages) $ lastEx dolFinalStages) staleKind `shouldBe` (staleKind, Nothing)

  it "requires an explicitly started rotation and validates command destinations and reply origins" do
    let rotationClaim = claimCommand 1 holder (DolEpoch 3 DolRotateStages) $ lastEx dolFinalStages
        normal = claimCommand 1 holder epoch $ headEx dolFinalStages
    handleDolCoordinatorCommand 3 holder teams rotationClaim emptyEvent `shouldBe` Left "Dol Guldur rotation has not been started"
    handleDolCoordinatorCommand 3 holder teams (normal {epicCommandTo = GroupOrdinal 1}) emptyEvent `shouldBe` Left "Dol Guldur coordinator command has the wrong destination"
    handleDolCoordinatorCommand 3 holder teams (normal {epicCommandFrom = GroupOrdinal 1}) emptyEvent `shouldBe` Left "Dol Guldur reply origin differs from command sender"

  it "persists reservations, callbacks and deduplication across event reloads" do
    let command = claimCommand 1 holder epoch $ headEx dolFinalStages
        (accepted, _) = applyCommand command emptyEvent
        restored = expectRight $ eitherDecode $ encode accepted
    restored `shouldBe` accepted
    applyCommand command restored `shouldBe` (accepted, Nothing)
    Set.member (identity 1) (coordination restored).epicDolProcessedCommands `shouldBe` True

  it "handles direct and wrapped native step completions once, acknowledging only wrapped requests" do
    let rotationEpoch = DolEpoch 3 DolRotateStages
        oldStages = zip (map GroupOrdinal [0, 1, 2]) $ map Just $ take 3 dolFinalStages
        started = expectRight $ startDolCoordination rotationEpoch holder oldStages emptyEvent
        (_, inFlight) = fromMaybe (error "missing City intent") $ expectRight $ nextDolStep (DolEligibility True True True True) $ transition started
        resolving = started {dolGuldurCoordination = Just $ (coordination started) {epicDolTransition = inFlight}}
        direct = EpicCommand (identity 1) holder holder (headEx dolFinalStages) (EpicDolStepDone rotationEpoch DolCityResolved) ScenarioTarget
        (completed, response) = applyCommand direct resolving
    isDolCoordinatorCommand direct `shouldBe` True
    response `shouldBe` Nothing
    (transition completed).dolTransitionPhase `shouldBe` DolWaitingForUnusedStage
    applyCommand direct completed `shouldBe` (completed, Nothing)
    let wrapped = direct {epicCommandId = identity 2, epicCommandEffect = EpicResolveWithReply holder (identity 1002) $ EpicDolStepDone rotationEpoch DolCityResolved}
        (repeated, reply) = applyCommand wrapped completed
    isDolCoordinatorCommand wrapped `shouldBe` True
    (transition repeated).dolTransitionPhase `shouldBe` DolWaitingForUnusedStage
    reply `shouldBe` Just (AcknowledgeDolClaim holder $ identity 1002)
    repeated.revision `shouldBe` completed.revision + 1
    applyCommand wrapped repeated `shouldBe` (repeated, Nothing)

  for_ [DolDrainingQueues, DolTransitionDone] $ \releasedPhase ->
    it ("immediately acknowledges an accepted retry after barriers were released: " <> show releasedPhase) do
      let assignments = zip (map GroupOrdinal [0, 1, 2]) $ take 3 dolFinalStages
          claimed = foldl' (\shared (team, stage) -> fst $ applyCommand (claimCommand (unGroupOrdinal team + 1) team epoch stage) shared) emptyEvent assignments
          released = (transition claimed)
            { dolTransitionPhase = releasedPhase
            , dolPreparedTeams = map fst assignments
            , dolRevealedTeams = map fst assignments
            }
          canonical = claimed {dolGuldurCoordination = Just $ (coordination claimed) {epicDolTransition = released, epicDolClaimReplies = []}}
          (retried, response) = applyCommand (claimCommand 9 holder epoch $ headEx dolFinalStages) canonical
      response `shouldBe` Just (AcknowledgeDolClaim holder $ identity 1009)
      (coordination retried).epicDolClaimReplies `shouldBe` []
      (transition retried).dolAcceptedClaims `shouldBe` assignments
  it "preserves shared counters and already-applied contribution identities when starting coordination" do
    let prior = emptyEvent {counters = Map.insert "custom" 17 emptyEvent.counters, appliedDeltas = Set.singleton $ identity 99, revision = 8}
        started = expectRight $ startDolCoordination epoch holder teams prior
    started.counters `shouldBe` prior.counters
    started.appliedDeltas `shouldBe` prior.appliedDeltas
    started.revision `shouldBe` 9
    startDolCoordination epoch holder (take 2 teams) prior `shouldBe` Left "DolInvalidTeams"
