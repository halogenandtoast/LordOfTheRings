module LordOfTheRings.Standalone.DolGuldurRuntimeSpec (spec) where

import Data.UUID qualified as UUID
import Data.Set qualified as Set
import Data.Map.Strict qualified as Map
import LordOfTheRings.Epic.DolGuldur
import LordOfTheRings.Epic.DolGuldur.Runtime
import LordOfTheRings.Epic.Types
import LordOfTheRings.Scenario.Types (scenarioEpic)
import TestImport

claimGame :: GameT (SharedEventState, Game)
claimGame = do
  let initial = fromJustNote "invalid event" $ newDolGuldurEvent 3
      epoch = DolEpoch 0 DolInitialStages
      transition = either (error . show) id $ beginDolTransition epoch (GroupOrdinal 0)
        [(GroupOrdinal 0, Nothing), (GroupOrdinal 1, Nothing), (GroupOrdinal 2, Nothing)]
      accepted = either (error . show) id $ acceptDolClaim (GroupOrdinal 0) (DolClaim epoch "attack-on-dol-guldur-003") transition
      shared = initial {dolGuldurCoordination = Just $ EpicDolState accepted [(GroupOrdinal 0, UUID.nil)] mempty}
      replica = EpicReplica (GroupOrdinal 0) shared [] [] [] (Set.singleton UUID.nil) mempty
        (Just $ EpicRoundState 0 EpicRoundResolving)
  current <- getGame
  lead <- getLead
  let held = PlayerWindow (AwaitEpicReply UUID.nil) $ QuestionLabel "claim" Nothing [] $ ChooseOne []
  pure (shared, current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just replica}) & questionL .~ Map.singleton lead held)

spec :: Spec
spec = describe "Dol Guldur runtime claim barriers" do
  it "lets later frozen effects run while an accepted initial claimant waits" . gameTest $ \_ -> do
    (shared, current) <- claimGame
    dolOrdinaryBlocked shared (GroupOrdinal 0) current `shouldBe` False
    dolClaimDrainable shared (GroupOrdinal 0) current `shouldBe` True
    fmap epicRoundPhase (dolRoundStateForDrain shared (GroupOrdinal 0) current) `shouldBe` Just EpicRoundEnded

  it "never treats an attack or a search as an inert claim barrier" . gameTest $ \pid -> do
    (shared, current) <- claimGame
    let answering = current & questionL .~ Map.singleton pid (ChooseOne [Label "defend" []])
    dolOrdinaryBlocked shared (GroupOrdinal 0) answering `shouldBe` True
    dolClaimDrainable shared (GroupOrdinal 0) answering `shouldBe` False

  it "holds uncommitted consequences even when the claim nonce itself is known" . gameTest $ \_ -> do
    (shared, current) <- claimGame
    let contributed = current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = fmap (\r -> r
          {epicPendingDeltas = [SharedDelta UUID.nil (GroupOrdinal 0) DolGuldurCityStrength 1]}) a.scenarioEpic})
    dolOrdinaryBlocked shared (GroupOrdinal 0) contributed `shouldBe` True
    dolClaimDrainable shared (GroupOrdinal 0) contributed `shouldBe` False

  it "does not exempt an unrelated reply or another team's claim" . gameTest $ \_ -> do
    (shared, current) <- claimGame
    dolOrdinaryBlocked shared (GroupOrdinal 1) current `shouldBe` True
    dolClaimDrainable shared (GroupOrdinal 1) current `shouldBe` False
