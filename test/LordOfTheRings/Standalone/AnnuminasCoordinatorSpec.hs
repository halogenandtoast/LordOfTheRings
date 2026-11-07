module LordOfTheRings.Standalone.AnnuminasCoordinatorSpec (spec) where

import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Epic.Coordinator
import LordOfTheRings.Epic.Types
import LordOfTheRings.Prelude
import Test.Hspec

type Team = (GroupOrdinal, Maybe EpicRoundState, [CardCode], Bool)

team :: Int -> EpicRoundPhase -> CardCode -> Team
team ordinal phase code = (GroupOrdinal ordinal, Just $ EpicRoundState 4 phase, [code], False)

spec :: Spec
spec = describe "Annúminas durable Epic round coordination" do
  it "waits until all three teams reach the same round boundary" do
    nextEpicRoundStep [team 0 EpicRoundWaiting "B", team 1 EpicRoundIdle "D", team 2 EpicRoundWaiting "F"] `shouldBe` Nothing
    nextEpicRoundStep [team 0 EpicRoundWaiting "B", team 1 EpicRoundWaiting "D", (GroupOrdinal 2, Just $ EpicRoundState 3 EpicRoundWaiting, ["F"], False)] `shouldBe` Nothing

  it "freezes all teams before any team's end-of-round effects resolve" do
    nextEpicRoundStep [team 2 EpicRoundWaiting "F", team 0 EpicRoundWaiting "B", team 1 EpicRoundWaiting "D"] `shouldBe` Just FreezeEpicTeams
    nextEpicRoundStep [team 0 EpicRoundReady "B", team 1 EpicRoundWaiting "D", team 2 EpicRoundWaiting "F"] `shouldBe` Nothing

  it "releases B, D, and F in order even when groups are loaded out of order" do
    nextEpicRoundStep [team 2 EpicRoundReady "F", team 1 EpicRoundReady "D", team 0 EpicRoundReady "B"] `shouldBe` Just (ResolveEpicTeam $ GroupOrdinal 0)
    nextEpicRoundStep [team 2 EpicRoundReady "F", team 0 EpicRoundEnded "B", team 1 EpicRoundReady "D"] `shouldBe` Just (ResolveEpicTeam $ GroupOrdinal 1)
    nextEpicRoundStep [team 1 EpicRoundEnded "D", team 2 EpicRoundReady "F", team 0 EpicRoundEnded "B"] `shouldBe` Just (ResolveEpicTeam $ GroupOrdinal 2)

  it "waits through a resolving team's questions and remote effects" do
    nextEpicRoundStep [team 0 EpicRoundEnded "B", team 1 EpicRoundResolving "D", team 2 EpicRoundReady "F"] `shouldBe` Nothing

  it "expires every team's round effects after the last team ends" do
    nextEpicRoundStep [team 0 EpicRoundEnded "B", team 1 EpicRoundEnded "D", team 2 EpicRoundEnded "F"] `shouldBe` Just ExpireEpicTeams
    nextEpicRoundStep [team 0 EpicRoundExpired "B", team 1 EpicRoundEnded "D", team 2 EpicRoundEnded "F"] `shouldBe` Nothing
    nextEpicRoundStep [team 0 EpicRoundExpired "B", team 1 EpicRoundExpired "D", team 2 EpicRoundExpired "F"] `shouldBe` Just BeginEpicTeams

  it "uses current quest order when stages are assigned to different groups" do
    nextEpicRoundStep [team 0 EpicRoundReady "H", team 1 EpicRoundReady "F", team 2 EpicRoundReady "B"] `shouldBe` Just (ResolveEpicTeam $ GroupOrdinal 2)

  it "rejects a partial event and duplicate group ordinals" do
    nextEpicRoundStep [team 0 EpicRoundWaiting "B", team 1 EpicRoundWaiting "D"] `shouldBe` Nothing
    nextEpicRoundStep [team 0 EpicRoundWaiting "B", team 0 EpicRoundWaiting "D", team 2 EpicRoundWaiting "F"] `shouldBe` Nothing

  it "keeps existing games without coordinator state independent" do
    nextEpicRoundStep [(GroupOrdinal 0, Nothing, ["B"], False), (GroupOrdinal 1, Nothing, ["D"], False), (GroupOrdinal 2, Nothing, ["F"], False)] `shouldBe` Nothing

  it "holds later teams while a released team's effect asks another team a question" do
    nextEpicRoundStep [team 0 EpicRoundWaiting "B", team 1 EpicRoundWaiting "D", (GroupOrdinal 2, Just $ EpicRoundState 4 EpicRoundWaiting, ["F"], True)] `shouldBe` Nothing
    nextEpicRoundStep [team 0 EpicRoundEnded "B", team 1 EpicRoundReady "D", (GroupOrdinal 2, Just $ EpicRoundState 4 EpicRoundReady, ["F"], True)] `shouldBe` Nothing
    nextEpicRoundStep [team 0 EpicRoundEnded "B", team 1 EpicRoundEnded "D", (GroupOrdinal 2, Just $ EpicRoundState 4 EpicRoundEnded, ["F"], True)] `shouldBe` Nothing

  describe "global beginning of the round" do
    it "waits for every round number before freezing beginning triggers" do
      nextEpicRoundStep [team 0 EpicRoundBeginning "B", team 1 EpicRoundExpired "D", team 2 EpicRoundExpired "F"] `shouldBe` Nothing
      nextEpicRoundStep [team 0 EpicRoundBeginning "B", team 1 EpicRoundBeginning "D", team 2 EpicRoundBeginning "F"] `shouldBe` Just FreezeEpicBeginningTeams

    it "reveals no final stage until every old stage has been replaced" do
      nextEpicRoundStep [team 0 EpicRoundPrepared "B", team 1 EpicRoundPreparingAdvance "D", team 2 EpicRoundPrepared "F"] `shouldBe` Nothing
      nextEpicRoundStep [team 0 EpicRoundPrepared "B", team 1 EpicRoundPrepared "D", team 2 EpicRoundPrepared "F"] `shouldBe` Just RevealEpicTeams
      nextEpicRoundStep [team 0 EpicRoundRevealed "B", team 1 EpicRoundRevealing "D", team 2 EpicRoundRevealed "F"] `shouldBe` Nothing

    it "holds the resource phase until all beginning effects finish" do
      nextEpicRoundStep [team 0 EpicRoundBegan "B", team 1 EpicRoundRevealed "D", team 2 EpicRoundBegan "F"] `shouldBe` Nothing
      nextEpicRoundStep [team 0 EpicRoundBegan "B", team 1 EpicRoundBegan "D", team 2 EpicRoundBegan "F"] `shouldBe` Just ReleaseEpicResources

    let board ordinal amount needed = EpicBoard (GroupOrdinal ordinal) "Team" [] False $ Just $
          EpicStage 0 [EpicQuest "stage-two" 2 amount needed True] [] False 0 False []
    it "requires all three current stage-two progress requirements" do
      annuminasStageTwoReady [board 0 50 (Just 50), board 1 49 (Just 50), board 2 50 (Just 50)] `shouldBe` False
      annuminasStageTwoReady [board 0 50 (Just 50), board 1 50 (Just 50), board 2 50 (Just 50)] `shouldBe` True
      annuminasStageTwoReady [board 0 40 (Just 40), board 1 40 (Just 40), board 2 40 (Just 40)] `shouldBe` True

    it "does not treat a missing stage or a duplicate board as another completed team" do
      annuminasStageTwoReady [board 0 50 (Just 50), board 0 50 (Just 50), board 2 50 (Just 50)] `shouldBe` False
      annuminasStageTwoReady [board 0 50 (Just 50), board 1 50 (Just 50), EpicBoard (GroupOrdinal 2) "Missing" [] False Nothing] `shouldBe` False
      annuminasStageTwoReady [board 0 50 (Just 50), board 1 50 (Just 50), board 2 50 Nothing] `shouldBe` False
