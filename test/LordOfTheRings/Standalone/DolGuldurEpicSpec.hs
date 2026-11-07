module LordOfTheRings.Standalone.DolGuldurEpicSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Set qualified as Set
import Data.UUID qualified as UUID
import LordOfTheRings.Epic.DolGuldur
import LordOfTheRings.Epic.Runner (epicRoundPhaseOf)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.AttackOnDolGuldur (attackOnDolGuldur)
import LordOfTheRings.Scenario.Types (scenarioEpic, scenarioQuestDeck)
import TestImport

replicaAt :: GroupOrdinal -> GameT ()
replicaAt ordinal = do
  let shared = fromJustNote "invalid event" $ newDolGuldurEvent 3
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica ordinal shared [] [] [] mempty mempty Nothing})

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

  for_ [EpicSubmitDolClaim $ DolClaim (DolEpoch 3 DolInitialStages) $ toCardCode Quests.theSiegeOfDolGuldur, EpicDolStepDone (DolEpoch 4 DolRotateStages) DolCityResolved] $ \effect ->
    it ("routes a coordinator request through the durable outbox even at group zero: " <> show effect) . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
      replicaAt $ GroupOrdinal 0
      run $ RequestEpicEffect (GroupOrdinal 0) (toCardCode Quests.theSiegeOfDolGuldur) effect ScenarioTarget
      replica <- currentReplica
      Set.size replica.epicPendingRequests `shouldBe` 1
      let command = fromJustNote "missing self-directed coordinator request" $ headMay replica.epicPendingCommands
      command.epicCommandTo `shouldBe` GroupOrdinal 0
      case command.epicCommandEffect of
        EpicResolveWithReply (GroupOrdinal 0) nonce inner -> do
          inner `shouldBe` effect
          Set.member nonce replica.epicPendingRequests `shouldBe` True
        other -> error $ "unexpected coordinator request: " <> show other

  for_ [(Quests.crossingTheAnduinDolGuldur, DolInitialStages), (Quests.theSiegeOfDolGuldur, DolRotateStages)] $ \(priorDef, kind) ->
    it ("submits the chosen stage with its current round and transition kind: " <> show kind) . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
      replicaAt $ GroupOrdinal 1
      _ <- putQuestStage priorDef
      roundNumber <- getsGame (.round)
      let selected = toCardCode Quests.theTowerOfSorcery
      run $ ScenarioSpecific "dolGuldur.claimStage" $ toJSON selected
      replica <- currentReplica
      map (.epicCommandTo) replica.epicPendingCommands `shouldBe` [GroupOrdinal 0]
      case map (.epicCommandEffect) replica.epicPendingCommands of
        [EpicResolveWithReply (GroupOrdinal 1) _ (EpicSubmitDolClaim request)] ->
          request `shouldBe` DolClaim (DolEpoch roundNumber kind) selected
        other -> error $ "unexpected native claim: " <> show other
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode priorDef

  it "rebuilds a rejected claim from canonical reservations and preserves unrelated requests" . gameTestIn attackOnDolGuldur testDeck $ \pid -> do
    replicaAt $ GroupOrdinal 0
    _ <- putQuestStage Quests.crossingTheAnduinDolGuldur
    cards <- traverse genEncounterCard [Quests.theSiegeOfDolGuldur, Quests.theTowerOfSorcery, Quests.battleUnderTheTrees, Quests.dungeonsOfTheNecromancer]
    roundNumber <- getsGame (.round)
    let epoch = DolEpoch roundNumber DolInitialStages
        emptyTransition = either (error . show) id $ beginDolTransition epoch (GroupOrdinal 0) [(GroupOrdinal 0, Nothing), (GroupOrdinal 1, Nothing), (GroupOrdinal 2, Nothing)]
        reserved = either (error . show) id $ acceptDolClaim (GroupOrdinal 1) (DolClaim epoch $ toCardCode Quests.theSiegeOfDolGuldur) emptyTransition
        nonce = UUID.nil
        unrelated = fromJustNote "invalid nonce" $ UUID.fromString "00000000-0000-0000-0000-000000000009"
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (\a -> a
      { scenarioQuestDeck = cards
      , scenarioEpic = fmap (\r -> r
          { epicPendingRequests = Set.fromList [nonce, unrelated]
          , epicShared = r.epicShared {dolGuldurCoordination = Just $ EpicDolState reserved [] mempty}
          }) a.scenarioEpic
      })
    run $ RejectDolStageClaim nonce
    (.epicPendingRequests) <$> currentReplica `shouldReturn` Set.singleton unrelated
    choices <- maybe [] questionChoices <$> getQuestion pid
    length choices `shouldBe` 3
    void $ runDebug $ RejectDolStageClaim nonce
    maybe [] questionChoices <$> getQuestion pid `shouldReturn` choices

  for_ [EpicRoundResolving, EpicRoundEnded] $ \resumedPhase ->
    it ("restores the old round phase before claim acknowledgements: " <> show resumedPhase) . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
      replicaAt $ GroupOrdinal 0
      current <- getGame
      putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = fmap (\r -> r
        { epicRoundState = Just $ EpicRoundState current.round EpicRoundRevealed
        , epicPendingRequests = Set.singleton UUID.nil
        }) a.scenarioEpic})
      run $ Run
        [ ResumeEpicQuestAdvance resumedPhase
        , ResolveEpicEffect (toCardCode Quests.theSiegeOfDolGuldur) (EpicAcknowledge UUID.nil) ScenarioTarget
        , AwaitEpicRound
        ]
      (.epicPendingRequests) <$> currentReplica `shouldReturn` mempty
      getsGame epicRoundPhaseOf `shouldReturn` Just resumedPhase
      getsGame (.round) `shouldReturn` current.round
      void $ runDebug $ ResumeEpicQuestAdvance EpicRoundIdle
      getsGame epicRoundPhaseOf `shouldReturn` Just resumedPhase

  it "prepares a rotation after the old round ended while preserving both physical quest cards" . gameTestIn attackOnDolGuldur testDeck $ \_ -> do
    replicaAt $ GroupOrdinal 0
    _ <- putQuestStage Quests.theSiegeOfDolGuldur
    prior <- getActiveQuest
    replacement <- genEncounterCard Quests.theTowerOfSorcery
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (\a -> a
      { scenarioQuestDeck = [replacement]
      , scenarioEpic = fmap (\r -> r {epicRoundState = Just $ EpicRoundState current.round EpicRoundEnded}) a.scenarioEpic
      })
    run $ PrepareEpicQuestAdvance EpicLeaveQuest replacement
    getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundPrepared
    placed <- getActiveQuest
    placed.cardId `shouldBe` replacement.id
    placed.flipped `shouldBe` False
    getsGame (map (.id) . scenarioQuestDeck . toAttrs . (.scenario)) `shouldReturn` [prior.cardId]

  it "loads shared event rows created before Dol coordinator persistence" do
    let shared = fromJustNote "invalid event" $ newDolGuldurEvent 3
        legacy = case toJSON shared of
          Object fields -> Object $ KeyMap.delete "dolGuldurCoordination" fields
          value -> value
    eitherDecode (encode legacy) `shouldBe` Right shared
