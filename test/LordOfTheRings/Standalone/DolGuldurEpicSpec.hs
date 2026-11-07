module LordOfTheRings.Standalone.DolGuldurEpicSpec (spec) where

import Data.Set qualified as Set
import Data.UUID qualified as UUID
import LordOfTheRings.Epic.Types
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur (attackOnDolGuldur)
import LordOfTheRings.Scenario.Types (scenarioEpic)
import TestImport

replicaAt :: GroupOrdinal -> GameT ()
replicaAt ordinal = do
  let shared = fromJustNote "invalid event" $ newDolGuldurEvent 3
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica ordinal shared [] [] [] mempty mempty})

currentReplica :: GameT EpicReplica
currentReplica = getsGame $ fromJustNote "missing replica" . scenarioEpic . toAttrs . (.scenario)

spec :: Spec
spec = describe "Dol Guldur durable cross-team effects" do
  it "blocks the origin's continuation until the destination acknowledges the full effect" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    replicaAt $ GroupOrdinal 0
    run $ Run [RequestEpicEffect (GroupOrdinal 1) (toCardCode Quests.theTowerOfSorcery) EpicDolDiscardToOne ScenarioTarget, ScenarioSpecific "dolGuldur.resolveSiege" Null]
    outgoingReplica <- currentReplica
    Set.size outgoingReplica.epicPendingRequests `shouldBe` 1
    outgoingReplica.epicPendingDeltas `shouldBe` []
    let request = fromJustNote "missing request" $ headMay outgoingReplica.epicPendingCommands
    nonce <- case request.epicCommandEffect of
      EpicResolveWithReply (GroupOrdinal 0) nonce EpicDolDiscardToOne -> pure nonce
      other -> error $ "unexpected request: " <> show other
    void $ runDebug $ ApplyEpicCommand $ EpicCommand UUID.nil (GroupOrdinal 1) (GroupOrdinal 0) (toCardCode Quests.theTowerOfSorcery) (EpicAcknowledge nonce) ScenarioTarget
    current <- currentReplica
    current.epicPendingRequests `shouldBe` mempty
    length current.epicPendingDeltas `shouldBe` 2

  it "waits for the recipient's discard choices before acknowledging and ignores duplicate deliveries" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    replicaAt $ GroupOrdinal 1
    run $ DrawCards $ DrawingFrom pid 3 GameSource
    let nonce = fromJustNote "invalid id" $ UUID.fromString "00000000-0000-0000-0000-000000000001"
        command = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 1) (toCardCode Quests.theTowerOfSorcery) (EpicResolveWithReply (GroupOrdinal 0) nonce EpicDolDiscardToOne) ScenarioTarget
    run $ ApplyEpicCommand command
    (.epicPendingCommands) <$> currentReplica `shouldReturn` []
    chooseOptionMatching "discard first card" pid (const True)
    (.epicPendingCommands) <$> currentReplica `shouldReturn` []
    chooseOptionMatching "discard second card" pid (const True)
    length . (.hand) <$> getPlayer pid `shouldReturn` 1
    commands <- (.epicPendingCommands) <$> currentReplica
    map (.epicCommandEffect) commands `shouldBe` [EpicAcknowledge nonce]
    map (.epicCommandTo) commands `shouldBe` [GroupOrdinal 0]
    run $ ApplyEpicCommand command
    (.epicPendingCommands) <$> currentReplica `shouldReturn` commands
    length . (.hand) <$> getPlayer pid `shouldReturn` 1

  it "acknowledges an already eliminated player without resolving the targeted search" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    replicaAt $ GroupOrdinal 1
    g <- getGame
    let removed = g & playersL . ix pid %~ (\p -> p {playerEliminated = True})
    putGame removed
    let nonce = fromJustNote "invalid id" $ UUID.fromString "00000000-0000-0000-0000-000000000001"
        command = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 1) (toCardCode Quests.theTowerOfSorcery) (EpicResolveWithReply (GroupOrdinal 0) nonce EpicDolSearchCard) (PlayerTarget pid)
    run $ ApplyEpicCommand command
    commands <- (.epicPendingCommands) <$> currentReplica
    map (.epicCommandEffect) commands `shouldBe` [EpicAcknowledge nonce]
