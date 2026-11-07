module LordOfTheRings.Standalone.DolGuldurCoordinatorSpec (spec) where

import LordOfTheRings.Prelude
import Data.Either (isRight)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Epic.DolGuldur
import Test.Hspec

initialEpoch, rotationEpoch :: DolEpoch
initialEpoch = DolEpoch 3 DolInitialStages
rotationEpoch = DolEpoch 4 DolRotateStages

ready :: DolEligibility
ready = DolEligibility True True True True

initial :: DolTransition Int
initial = expectRight $ beginDolTransition initialEpoch 0 [(0, Nothing), (1, Nothing), (2, Nothing)]

rotation :: DolTransition Int
rotation = expectRight $ beginDolTransition rotationEpoch 0 $ zip [0, 1, 2] $ map Just $ take 3 dolFinalStages

expectRight :: Show a => Either a b -> b
expectRight = either (error . show) id

claim :: Int -> CardCode -> DolTransition Int -> DolTransition Int
claim team code state = expectRight $ acceptDolClaim team (DolClaim state.dolTransitionEpoch code) state

step :: DolTransition Int -> (DolStep Int, DolTransition Int)
step state = fromMaybe (error "expected coordinator action") $ expectRight $ nextDolStep ready state

ack :: DolAck Int -> DolTransition Int -> DolTransition Int
ack message state = expectRight $ ackDolStep state.dolTransitionEpoch message state

collectingRotation :: DolTransition Int
collectingRotation = ack DolUnusedStageResolved $ snd $ step $ ack DolCityResolved $ snd $ step rotation

spec :: Spec
spec = describe "Dol Guldur atomic stage coordinator" do
  it "requires three distinct teams and exactly three distinct old final stages for rotation" do
    beginDolTransition rotationEpoch (0 :: Int) [(0, Nothing), (1, Nothing), (2, Nothing)] `shouldBe` Left DolInvalidPreviousStages
    beginDolTransition initialEpoch (0 :: Int) [(0, Nothing), (0, Nothing), (2, Nothing)] `shouldBe` Left DolInvalidTeams
    beginDolTransition initialEpoch (9 :: Int) [(0, Nothing), (1, Nothing), (2, Nothing)] `shouldBe` Left DolInvalidCityHolder
    beginDolTransition rotationEpoch (0 :: Int) [(0, Just $ headEx dolFinalStages), (1, Just $ headEx dolFinalStages), (2, Just $ lastEx dolFinalStages)] `shouldBe` Left DolInvalidPreviousStages

  it "settles all old forced effects before City and keeps City separate from the unused forced effect" do
    nextDolStep (ready {dolOldStageForcesComplete = False}) rotation `shouldBe` Right Nothing
    let (cityAction, cityInFlight) = step rotation
    cityAction `shouldBe` ResolveDolCity 0
    nextDolStep ready cityInFlight `shouldBe` Right Nothing
    let cityDone = ack DolCityResolved cityInFlight
    ack DolCityResolved cityDone `shouldBe` cityDone
    nextDolStep (ready {dolEventActive = False}) cityDone `shouldBe` Right Nothing
    nextDolStep (ready {dolConsequencesSettled = False}) cityDone `shouldBe` Right Nothing
    let (unusedAction, unusedInFlight) = step cityDone
    unusedAction `shouldBe` ResolveUnusedDolStage 0 (lastEx dolFinalStages)
    nextDolStep ready unusedInFlight `shouldBe` Right Nothing
    acceptDolClaim 0 (DolClaim rotationEpoch $ lastEx dolFinalStages) unusedInFlight `shouldBe` Left DolClaimsNotOpen
    (ack DolUnusedStageResolved unusedInFlight).dolTransitionPhase `shouldBe` DolCollectingClaims

  it "atomically rejects conflicts, stale epochs and changing an accepted claim while allowing exact retries" do
    let chosen = headEx dolFinalStages
        accepted = claim 0 chosen initial
    acceptDolClaim 1 (DolClaim initialEpoch chosen) accepted `shouldBe` Left (DolStageAlreadyClaimed chosen)
    acceptDolClaim 0 (DolClaim initialEpoch chosen) accepted `shouldBe` Right accepted
    acceptDolClaim 0 (DolClaim initialEpoch $ lastEx dolFinalStages) accepted `shouldBe` Left (DolClaimAlreadyAccepted chosen)
    acceptDolClaim 1 (DolClaim rotationEpoch chosen) accepted `shouldBe` Left DolStaleEpoch
    acceptDolClaim 8 (DolClaim initialEpoch chosen) accepted `shouldBe` Left DolUnknownTeam
    acceptDolClaim 1 (DolClaim initialEpoch "bad-stage") accepted `shouldBe` Left (DolUnknownStage "bad-stage")
    acceptDolClaim 0 (DolClaim rotationEpoch chosen) collectingRotation `shouldBe` Left (DolCurrentStage chosen)
    availableDolStages 1 accepted `shouldBe` Right (drop 1 dolFinalStages)

  it "permits cyclic swaps of other teams' old stages" do
    let stageB = headEx dolFinalStages
        stageD = headEx $ drop 1 dolFinalStages
        stageF = headEx $ drop 2 dolFinalStages
        swapped = claim 2 stageB $ claim 1 stageF $ claim 0 stageD collectingRotation
    validateDolTransition swapped `shouldBe` Right ()
    fst (step swapped) `shouldBe` PrepareDolTeams [(2, stageB), (0, stageD), (1, stageF)]

  it "prepares every physical stage before revealing any stage and drains old queues only after release" do
    let stageB = headEx dolFinalStages
        stageD = headEx $ drop 1 dolFinalStages
        stageF = headEx $ drop 2 dolFinalStages
        claimed = claim 0 stageF $ claim 1 stageB $ claim 2 stageD initial
        (prepareAction, preparing) = step claimed
    prepareAction `shouldBe` PrepareDolTeams [(1, stageB), (2, stageD), (0, stageF)]
    nextDolStep ready preparing `shouldBe` Right Nothing
    let twoPrepared = ack (DolTeamPrepared 2) $ ack (DolTeamPrepared 0) preparing
    nextDolStep ready twoPrepared `shouldBe` Right Nothing
    let allPrepared = ack (DolTeamPrepared 1) twoPrepared
        (revealB, revealingB) = step allPrepared
    revealB `shouldBe` RevealDolTeam 1 stageB
    nextDolStep ready revealingB `shouldBe` Right Nothing
    let reloadedReveal = expectRight $ eitherDecode @(DolTransition Int) $ encode revealingB
    reloadedReveal `shouldBe` revealingB
    nextDolStep ready reloadedReveal `shouldBe` Right Nothing
    ackDolStep rotationEpoch (DolTeamRevealed 1) reloadedReveal `shouldBe` Left DolStaleEpoch
    ackDolStep initialEpoch (DolTeamRevealed 2) revealingB `shouldBe` Left DolUnexpectedAcknowledgement
    let afterB = ack (DolTeamRevealed 1) revealingB
    ack (DolTeamRevealed 1) afterB `shouldBe` afterB
    let (revealD, revealingD) = step afterB
    revealD `shouldBe` RevealDolTeam 2 stageD
    let (revealF, revealingF) = step $ ack (DolTeamRevealed 2) revealingD
    revealF `shouldBe` RevealDolTeam 0 stageF
    let allRevealed = ack (DolTeamRevealed 0) revealingF
        (releaseAction, draining) = step allRevealed
    dolAllStagesRevealed allRevealed `shouldBe` True
    releaseAction `shouldBe` ReleaseDolTeams
    nextDolStep (ready {dolOldEndRoundQueuesSettled = False}) draining `shouldBe` Right Nothing
    let (finishAction, finished) = step draining
    finishAction `shouldBe` FinishDolTransition
    dolTransitionFinished finished `shouldBe` True
    nextDolStep ready finished `shouldBe` Right Nothing
    acceptDolClaim 0 (DolClaim initialEpoch stageF) finished `shouldBe` Right finished

  it "allows only an accepted inert initial claim to unblock later old-stage forced effects" do
    let accepted = claim 0 (headEx dolFinalStages) initial
    dolClaimAllowsRoundDrain True 0 accepted `shouldBe` True
    dolClaimAllowsRoundDrain False 0 accepted `shouldBe` False
    dolClaimAllowsRoundDrain True 1 accepted `shouldBe` False
    dolClaimAllowsRoundDrain True 0 (claim 0 (lastEx dolFinalStages) collectingRotation) `shouldBe` False

  it "persists an in-flight native action without replaying it after reload" do
    let (_, cityInFlight) = step rotation
        reloaded = expectRight $ eitherDecode @(DolTransition Int) $ encode cityInFlight
    reloaded `shouldBe` cityInFlight
    nextDolStep ready reloaded `shouldBe` Right Nothing
    nextDolStep ready (ack DolCityResolved reloaded) `shouldSatisfy` isRight

  it "accepts every legal three-team reassignment in every concurrent claim arrival order" do
    let assignments = [[a, b, c] | a <- dolFinalStages, b <- dolFinalStages, c <- dolFinalStages, length (nub [a,b,c]) == 3]
        orders = permutations [0, 1, 2]
    for_ assignments \oldStages -> do
      let base = expectRight $ beginDolTransition rotationEpoch 0 $ zip [0, 1, 2] $ map Just oldStages
          opened = ack DolUnusedStageResolved $ snd $ step $ ack DolCityResolved $ snd $ step base
          legal = filter (and . zipWith (/=) oldStages) assignments
      for_ legal \newStages -> for_ orders \arrival -> do
        let accepted = foldl' (\state team -> claim team (fromMaybe (error "unknown test team") $ lookup team $ zip [0, 1, 2] newStages) state) opened arrival
        validateDolTransition accepted `shouldBe` Right ()
        fst (step accepted) `shouldBe` PrepareDolTeams (sortOn snd $ zip [0, 1, 2] newStages)

  it "rejects corrupt persisted reservations and premature physical preparation" do
    validateDolTransition (initial {dolAcceptedClaims = [(0, headEx dolFinalStages), (1, headEx dolFinalStages)]}) `shouldBe` Left DolInvalidClaims
    nextDolStep ready (initial {dolPreparedTeams = [0]}) `shouldBe` Left DolInvalidPreparation
    ackDolStep initialEpoch DolCityResolved initial `shouldBe` Left DolUnexpectedAcknowledgement
