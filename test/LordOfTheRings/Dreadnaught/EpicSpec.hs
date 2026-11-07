module LordOfTheRings.Dreadnaught.EpicSpec (spec) where

import Control.Monad.Random.Strict (evalRand)
import Data.UUID qualified as UUID
import LordOfTheRings.Epic.Types
import LordOfTheRings.Scenario.Runner (epicMessageId)
import System.Random (mkStdGen)
import TestImport

pureIt :: String -> IO () -> Spec
pureIt = it

stateFor :: Int -> SharedEventState
stateFor n = fromJustNote "invalid event size" $ newSharedEvent n

delta :: Word32 -> Int -> SharedKey -> Int -> SharedDelta
delta ident ordinal key n = SharedDelta (UUID.fromWords 0 0 0 ident) (GroupOrdinal ordinal) key n

spec :: Spec
spec = describe "Dreadnaught Epic shared state" do
  pureIt "counts contributions from different groups against the frozen global player count" do
    let (one, _) = applySharedDelta (delta 1 0 SearchingTheHighSeasProgress 20) (stateFor 4)
        (two, _) = applySharedDelta (delta 2 1 SearchingTheHighSeasProgress 12) one
    stageOneComplete one `shouldBe` False
    stageOneComplete two `shouldBe` True
    two.totalPlayers `shouldBe` 4

  pureIt "does not apply the same delivered delta twice" do
    let contribution = delta 1 0 DreadnaughtDamage 5
        (one, _) = applySharedDelta contribution (stateFor 4)
        (two, effective) = applySharedDelta contribution one
    two `shouldBe` one
    effective `shouldBe` Nothing

  pureIt "journals only damage actually added when simultaneous groups finish the Dreadnaught" do
    let (one, _) = applySharedDelta (delta 1 0 DreadnaughtDamage 58) (stateFor 4)
        (two, effective) = applySharedDelta (delta 2 1 DreadnaughtDamage 5) one
    sharedCounter DreadnaughtDamage two `shouldBe` 60
    fmap (.deltaAmount) effective `shouldBe` Just 2
    dreadnaughtDestroyed two `shouldBe` True

  pureIt "bounds reversals and rejects an event with no players" do
    let (state, effective) = applySharedDelta (delta 1 0 DreadnaughtDamage (-5)) (stateFor 4)
    sharedCounter DreadnaughtDamage state `shouldBe` 0
    fmap (.deltaAmount) effective `shouldBe` Just 0
    newSharedEvent 0 `shouldBe` Nothing

  pureIt "keeps the organizer's completed-search announcement while groups advance independently" do
    let (complete, _) = applySharedDelta (delta 1 0 SearchingTheHighSeasProgress 32) (stateFor 4)
        (removed, _) = applySharedDelta (delta 2 0 SearchingTheHighSeasProgress (-2)) complete
    sharedCounter SearchingTheHighSeasProgress removed `shouldBe` 30
    stageOneComplete removed `shouldBe` True

  pureIt "uses a fresh contribution ID after undo restores the RNG" do
    let shared = stateFor 4
        replica = EpicReplica (GroupOrdinal 0) shared [] [] [] mempty mempty Nothing
        identifier r = evalRand (epicMessageId r) (mkStdGen 42)
        initialId = identifier replica
        (accepted, _) = applySharedDelta (SharedDelta initialId (GroupOrdinal 0) SearchingTheHighSeasProgress 4) shared
        (reversed, _) = applySharedDelta (delta 99 0 SearchingTheHighSeasProgress (-4)) accepted
        replay = identifier replica {epicShared = reversed}
        (replayed, _) = applySharedDelta (SharedDelta replay (GroupOrdinal 0) SearchingTheHighSeasProgress 2) reversed
    replay `shouldNotBe` initialId
    sharedCounter SearchingTheHighSeasProgress replayed `shouldBe` 2
