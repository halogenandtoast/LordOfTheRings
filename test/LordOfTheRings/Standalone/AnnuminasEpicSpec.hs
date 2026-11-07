module LordOfTheRings.Standalone.AnnuminasEpicSpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Epic.Types
import LordOfTheRings.Outcome (Outcome (Won, Lost))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioEpic))
import TestImport

stateFor :: Int -> SharedEventState
stateFor players = fromJustNote "invalid Annúminas event size" $ newAnnuminasEvent players

delta :: Word32 -> Int -> Int -> SharedDelta
delta ident ordinal amount = SharedDelta (UUID.fromWords 0 0 0 ident) (GroupOrdinal ordinal) AnnuminasCityStrength amount

spec :: Spec
spec = describe "Annúminas Epic city strength" do
  it "starts at fifty for every legal event size and rejects impossible totals" do
    for_ [3 .. 12] $ \players -> do
      annuminasCityStrength (stateFor players) `shouldBe` Just 50
      annuminasCityDefeated (stateFor players) `shouldBe` False
    for_ [-1, 0, 1, 2, 13] $ \players -> newAnnuminasEvent players `shouldBe` Nothing
    pure () :: IO ()

  it "combines damage and repairs from all three teams and permits strength above fifty" do
    let (damaged, _) = applySharedDelta (delta 1 0 (-9)) (stateFor 6)
        (repaired, _) = applySharedDelta (delta 2 1 10) damaged
        (again, _) = applySharedDelta (delta 3 2 10) repaired
    annuminasCityStrength damaged `shouldBe` Just 41
    annuminasCityStrength again `shouldBe` Just 61
    pure () :: IO ()

  it "ignores duplicate damage and repair deliveries, including after save and reload" do
    let damage = delta 1 0 (-7)
        repair = delta 2 1 10
        (damaged, _) = applySharedDelta damage (stateFor 3)
        (repaired, _) = applySharedDelta repair damaged
        restored = either error id $ eitherDecode $ encode repaired
        (duplicateDamage, damageResult) = applySharedDelta damage restored
        (duplicateRepair, repairResult) = applySharedDelta repair duplicateDamage
    duplicateRepair `shouldBe` repaired
    damageResult `shouldBe` Nothing
    repairResult `shouldBe` Nothing
    pure () :: IO ()

  it "journals only damage actually suffered when simultaneous teams destroy the city" do
    let (damaged, _) = applySharedDelta (delta 1 0 (-48)) (stateFor 12)
        (destroyed, effective) = applySharedDelta (delta 2 1 (-9)) damaged
    annuminasCityStrength destroyed `shouldBe` Just 0
    fmap (.deltaAmount) effective `shouldBe` Just (-2)
    annuminasCityDefeated destroyed `shouldBe` True
    pure () :: IO ()

  it "keeps defeat when a repair arrives after the city falls" do
    let (destroyed, _) = applySharedDelta (delta 1 0 (-50)) (stateFor 3)
        (repaired, effective) = applySharedDelta (delta 2 1 10) destroyed
        (damaged, _) = applySharedDelta (delta 3 2 (-9)) repaired
    annuminasCityStrength damaged `shouldBe` Just 0
    fmap (.deltaAmount) effective `shouldBe` Just 0
    annuminasCityDefeated repaired `shouldBe` True
    pure () :: IO ()

  it "does not mistake an existing Dreadnaught event for a fallen city" do
    let existing = fromJustNote "invalid event" $ newSharedEvent 3
    annuminasCityStrength existing `shouldBe` Nothing
    annuminasCityDefeated existing `shouldBe` False
    pure () :: IO ()

  it "ends a team without the city card when another team destroys the shared city" . gameTest $ \_ -> do
    let shared = stateFor 3
        (destroyed, _) = applySharedDelta (delta 1 0 (-50)) shared
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 2) shared [] [] [] mempty mempty Nothing})
    run $ SyncEpic destroyed
    getsGame (.outcome) `shouldReturn` Just Lost

  it "ends the originating team immediately when its own contribution destroys the city" . gameTest $ \_ -> do
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 0) (stateFor 3) [] [] [] mempty mempty Nothing})
    run $ ContributeShared AnnuminasCityStrength (-50)
    getsGame (.outcome) `shouldReturn` Just Lost

  for_ [(AnnuminasVictory, Won), (AnnuminasDefeat, Lost)] $ \(key, outcome) ->
    it ("latches a shared terminal outcome and ignores later city changes: " <> show key) do
      let initial = stateFor 3
          terminalDelta = SharedDelta (UUID.fromWords 0 0 0 101) (GroupOrdinal 0) key 1
          (terminal, _) = applySharedDelta terminalDelta initial
          (lateDamage, damage) = applySharedDelta (delta 102 1 (-100)) terminal
          otherKey = if key == AnnuminasVictory then AnnuminasDefeat else AnnuminasVictory
          (lateEnding, ending) = applySharedDelta (SharedDelta (UUID.fromWords 0 0 0 103) (GroupOrdinal 2) otherKey 1) lateDamage
      annuminasOutcome terminal `shouldBe` Just outcome
      annuminasCityStrength lateDamage `shouldBe` Just 50
      fmap (.deltaAmount) damage `shouldBe` Just 0
      fmap (.deltaAmount) ending `shouldBe` Just 0
      annuminasOutcome lateEnding `shouldBe` Just outcome
      pure () :: IO ()

  it "cannot turn a destroyed city into a victory" do
    let (destroyed, _) = applySharedDelta (delta 104 0 (-50)) (stateFor 3)
        (lateWin, applied) = applySharedDelta (SharedDelta (UUID.fromWords 0 0 0 105) (GroupOrdinal 1) AnnuminasVictory 1) destroyed
    annuminasOutcome lateWin `shouldBe` Just Lost
    fmap (.deltaAmount) applied `shouldBe` Just 0
    pure () :: IO ()
