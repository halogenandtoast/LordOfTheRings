module LordOfTheRings.Standalone.DolGuldurSupportTransferSpec (spec) where

import Data.Set qualified as Set
import Data.UUID qualified as UUID
import Data.List qualified as List
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (toCard)
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Helpers.Character (getDefense)
import LordOfTheRings.Matcher (getAlly, getHero, getObjective)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (objectiveTokens, objectiveExhausted, objectiveUnderneath)
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import System.Random (mkStdGen)
import TestImport

replicaFor :: Int -> [EpicBoard] -> GameT ()
replicaFor ordinal boards = do
  generator <- asks gameRandomGen
  liftIO $ writeIORef generator $ mkStdGen (900 + ordinal)
  let shared = fromJustNote "invalid Dol Guldur event" $ newDolGuldurEvent 3
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal ordinal) shared [] boards [] mempty mempty Nothing})

commands :: GameT [EpicCommand]
commands = getsGame $ maybe [] epicPendingCommands . scenarioEpic . toAttrs . (.scenario)

flushCommands :: GameT [EpicCommand]
flushCommands = do
  outgoing <- commands
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioEpic = fmap (\r -> r {epicPendingCommands = []}) a.scenarioEpic})
  pure outgoing

hostFor :: PlayerId -> GameT Target
hostFor pid = do
  card <- genEncounterCard Objectives.hostOfLorien
  run $ PutObjectiveAllyIntoPlayUnder pid card
  getsGame $ toTarget . fromJustNote "Host missing" . find ((== card.id) . allyCardId . toAttrs) . toList . (.entities.allies)

powerFor :: PlayerId -> GameT Target
powerFor pid = do
  card <- genEncounterCard Objectives.powerOfLorien
  run $ PutObjectiveIntoPlayAttached card $ PlayerTarget pid
  getsGame $ toTarget . fromJustNote "Power missing" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.objectives)

supportFor :: Bool -> PlayerId -> GameT Target
supportFor isHost = if isHost then hostFor else powerFor

supportPlacement :: Target -> GameT Placement
supportPlacement = \case
  AllyTarget aid -> (.placement) <$> getAlly aid
  ObjectiveTarget oid -> (.placement) <$> getObjective oid
  _ -> error "not a Lorien support"

supportCount :: Bool -> GameT Int
supportCount isHost = getsGame $ \g -> if isHost then length g.entities.allies else length g.entities.objectives

recipientGame :: IO (Game, PlayerId)
recipientGame = do
  result <- newIORef Nothing
  gameTest $ \pid -> do
    replicaFor 1 []
    current <- getGame
    liftIO $ writeIORef result $ Just (current, pid)
  fromJustNote "missing recipient game" <$> readIORef result

data Departure = Departure
  { departingGame :: Game
  , departingQueue :: [Message]
  , departingCommand :: EpicCommand
  , departingTarget :: Target
  , departingHero :: Target
  , departingOwner :: PlayerId
  }

departSupport :: Bool -> Game -> PlayerId -> IO Departure
departSupport isHost recipient recipientId = do
  result <- newIORef Nothing
  gameTest $ \pid -> do
    replicaFor 0 [epicBoardFor (GroupOrdinal 1) "Recipient" recipient]
    withoutErrata pid
    hero <- heroOf pid "01012"
    _ <- putAttachmentOn pid Attachments.hornOfGondor hero
    target <- supportFor isHost pid
    run $ PlaceTokens GameSource target Resource 3
    run $ PlaceTokens GameSource target Progress 7
    run $ Exhaust target
    when isHost do
      run $ PlaceTokens GameSource target Damage 2
      run $ CommitToQuest target
    run $ CreateEffect $ Effect GameSource target [DefenseModifier 2] UntilEndOfRound
    let code = toCardCode $ if isHost then Objectives.hostOfLorien else Objectives.powerOfLorien
    run $ Run [ResolveEpicEffect code (EpicTransferLorienSupport (GroupOrdinal 1) recipientId pid) target, PlaceTokens GameSource hero Resource 1]
    supportCount isHost `shouldReturn` 0
    -- Old Horn of Gondor answers any character leaving play. Moving the Host
    -- neither answers that response nor grants its resource.
    getDamage hero `shouldReturn` 0
    case hero of
      HeroTarget hid -> (.resources) <$> getHero hid `shouldReturn` 0
      _ -> error "expected hero"
    getEncounterDiscard `shouldReturn` []
    [command] <- flushCommands
    current <- getGame
    queue <- peekQueue
    restored <- either error pure $ eitherDecode $ encode current
    restoredQueue <- either error pure $ eitherDecode $ encode queue
    let departure = Departure restored restoredQueue command target hero pid
    liftIO $ writeIORef result $ Just departure
  fromJustNote "source did not emit its durable transfer" <$> readIORef result

packetFrom :: EpicCommand -> EpicLorienSupportTransfer
packetFrom command = case command.epicCommandEffect of
  EpicResolveWithReply _ _ (EpicReceiveLorienSupport packet _) -> packet
  _ -> error "expected wrapped Lorien transfer"

spec :: Spec
spec = describe "Dol Guldur Epic support transfers" do
  for_ [False, True] $ \isHost -> do
    it ("does not offer the setup response in standard mode, Host: " <> show isHost) . gameTest $ \pid -> do
      target <- supportFor isHost pid
      run $ CheckWindows [mkWhen RoundEnds]
      offersAbilityOf pid target `shouldReturn` False

    it ("gives control locally without refreshing or clearing tokens, Host: " <> show isHost) . twoPlayerTest testDeck $ \one two -> do
      replicaFor 0 []
      target <- supportFor isHost one
      run $ PlaceTokens GameSource target Resource 2
      run $ Exhaust target
      run $ CheckWindows [mkWhen RoundEnds]
      offersAbilityOf one target `shouldReturn` True
      offersAbilityOf two target `shouldReturn` False
      chooseAbilityOf one target
      chooseOptionMatching "give to the other player" one (const True)
      supportPlacement target `shouldReturn` if isHost then InPlayArea two else AttachedTo (PlayerTarget two)
      case target of
        AllyTarget aid -> do
          ally <- getAlly aid
          allyExhausted (toAttrs ally) `shouldBe` True
          countTokens Resource (allyTokens $ toAttrs ally) `shouldBe` 2
          ally.owner `shouldBe` one
        ObjectiveTarget oid -> do
          objective <- getObjective oid
          objectiveExhausted (toAttrs objective) `shouldBe` True
          countTokens Resource (objectiveTokens $ toAttrs objective) `shouldBe` 2
        _ -> error "expected support target"
      commands `shouldReturn` []

    it ("offers other local and remote players and excludes the current controller, Host: " <> show isHost) . twoPlayerTest testDeck $ \one _ -> do
      (remote, _) <- liftIO recipientGame
      replicaFor 0 [epicBoardFor (GroupOrdinal 1) "Other stage" remote]
      target <- supportFor isHost one
      run $ CheckWindows [mkWhen RoundEnds]
      chooseAbilityOf one target
      length . maybe [] questionChoices <$> getQuestion one `shouldReturn` 2

    it ("rejects a stale transfer after the current controller has changed, Host: " <> show isHost) . twoPlayerTest testDeck $ \one two -> do
      replicaFor 0 []
      target <- supportFor isHost one
      let code = toCardCode $ if isHost then Objectives.hostOfLorien else Objectives.powerOfLorien
      run $ ResolveEpicEffect code (EpicTransferLorienSupport (GroupOrdinal 0) two two) target
      supportPlacement target `shouldReturn` if isHost then InPlayArea one else AttachedTo (PlayerTarget one)

    it ("preserves physical identity and state across games, retries and save/reload, Host: " <> show isHost) do
      (receiver, receiverId) <- recipientGame
      departure <- departSupport isHost receiver receiverId
      let packet = packetFrom departure.departingCommand
          restoredCommand = either error id $ eitherDecode $ encode departure.departingCommand
      reply <- newIORef Nothing
      receiverEnv <- newGameEnv receiver (const $ pure ()) 901
      runGameT receiverEnv do
        run $ ApplyEpicCommand restoredCommand
        supportCount isHost `shouldReturn` 1
        supportPlacement departure.departingTarget `shouldReturn` if isHost then InPlayArea receiverId else AttachedTo (PlayerTarget receiverId)
        case departure.departingTarget of
          AllyTarget aid -> do
            host <- getAlly aid
            (toCard host).id `shouldBe` packet.epicLorienCard.id
            host.owner `shouldBe` departure.departingOwner
            allyExhausted (toAttrs host) `shouldBe` True
            allyCommitted (toAttrs host) `shouldBe` True
            allyTokens (toAttrs host) `shouldBe` packet.epicLorienTokens
            getDefense (AllyTarget aid) `shouldReturn` 5
          ObjectiveTarget oid -> do
            power <- getObjective oid
            (toCard power).id `shouldBe` packet.epicLorienCard.id
            objectiveTokens (toAttrs power) `shouldBe` packet.epicLorienTokens
            objectiveExhausted (toAttrs power) `shouldBe` True
          _ -> error "expected support target"
        getsGame (filter ((== departure.departingTarget) . effectTarget) . gameEffects) `shouldReturn`
          filter ((== departure.departingTarget) . effectTarget) packet.epicLorienEffects
        [ack] <- flushCommands
        current <- getGame
        restored <- either error pure $ eitherDecode $ encode current
        putGame restored
        run $ ApplyEpicCommand restoredCommand
        supportCount isHost `shouldReturn` 1
        commands `shouldReturn` []
        liftIO $ writeIORef reply $ Just ack
      ack <- fromJustNote "recipient did not acknowledge" <$> readIORef reply
      originEnv <- newGameEnv departure.departingGame (const $ pure ()) 900
      runGameT originEnv do
        setQueue departure.departingQueue
        void $ runDebug $ ApplyEpicCommand ack
        supportCount isHost `shouldReturn` 0
        case departure.departingHero of
          HeroTarget hid -> (.resources) <$> getHero hid `shouldReturn` 1
          _ -> error "expected hero"
        Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
        replica.epicPendingRequests `shouldBe` Set.empty
        void $ runDebug $ ApplyEpicCommand ack
        case departure.departingHero of
          HeroTarget hid -> (.resources) <$> getHero hid `shouldReturn` 1
          _ -> error "expected hero"

  it "carries facedown Power cards privately through the durable outbox" do
    (receiver, receiverId) <- recipientGame
    result <- newIORef Nothing
    gameTest $ \pid -> do
      replicaFor 0 [epicBoardFor (GroupOrdinal 1) "Other" receiver]
      target <- powerFor pid
      secret <- genPlayerCard Events.sneakAttack (Just pid)
      run $ PlaceUnderneath target [PlayerCard secret]
      run $ ResolveEpicEffect (toCardCode Objectives.powerOfLorien) (EpicTransferLorienSupport (GroupOrdinal 1) receiverId pid) target
      publicBeforeFlush <- getGame
      show (encode $ publicGame publicBeforeFlush) `shouldSatisfy` (not . List.isInfixOf (show secret.id))
      [command] <- flushCommands
      case packetFrom command of
        EpicLorienPowerTransfer {epicLorienUnderneath = under} -> under `shouldBe` [Left secret]
        _ -> error "expected Power packet"
      current <- getGame
      show (encode $ publicGame current) `shouldSatisfy` (not . List.isInfixOf (show secret.id))
      liftIO $ writeIORef result $ Just (command, secret.id, target)
    (command, secretId, target) <- fromJustNote "missing private transfer" <$> readIORef result
    env <- newGameEnv receiver (const $ pure ()) 902
    runGameT env do
      run $ ApplyEpicCommand command
      case target of
        ObjectiveTarget oid -> length . objectiveUnderneath . toAttrs <$> getObjective oid `shouldReturn` 1
        _ -> error "expected objective"
      current <- getGame
      show (encode $ publicGame current) `shouldSatisfy` (not . List.isInfixOf (show secretId))

  it "allows acknowledged cross-team effects to target a live player" . gameTest $ \pid -> do
    replicaFor 1 []
    hero <- heroOf pid "01012"
    let command = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 1) (toCardCode Objectives.dolGuldurAttack)
          (EpicResolveWithReply (GroupOrdinal 0) UUID.nil EpicDolDrawResource) (PlayerTarget pid)
    run $ ApplyEpicCommand command
    commands `shouldReturn` []
    chooseTarget pid hero
    getsGame (sum . map (.resources) . toList . (.entities.heroes)) `shouldReturn` 1
    [ack] <- commands
    ack.epicCommandEffect `shouldBe` EpicAcknowledge UUID.nil
    getDamage hero `shouldReturn` 0

  it "acknowledges a vanished player target so the source cannot wait forever" . twoPlayerTest testDeck $ \one two -> do
    replicaFor 1 []
    current <- getGame
    putGame $ current & playersL . ix two . eliminatedL .~ True
    let command = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 1) (toCardCode Objectives.dolGuldurAttack)
          (EpicResolveWithReply (GroupOrdinal 0) UUID.nil EpicDolDrawResource) (PlayerTarget two)
    run $ ApplyEpicCommand command
    [ack] <- commands
    ack.epicCommandEffect `shouldBe` EpicAcknowledge UUID.nil
    getQuestion one `shouldReturn` Nothing


  for_ [False, True] $ \isHost ->
    it ("returns a support safely if the selected remote player is eliminated before delivery, Host: " <> show isHost) do
      receiverRef <- newIORef Nothing
      twoPlayerTest testDeck $ \one _ -> do
        replicaFor 1 []
        current <- getGame
        liftIO $ writeIORef receiverRef $ Just (current, one)
      (receiver, receiverId) <- fromJustNote "missing recipient" <$> readIORef receiverRef
      departure <- departSupport isHost receiver receiverId
      returnedRef <- newIORef Nothing
      receiverEnv <- newGameEnv receiver (const $ pure ()) 910
      runGameT receiverEnv do
        run $ EliminatePlayer receiverId
        run $ ApplyEpicCommand departure.departingCommand
        supportCount isHost `shouldReturn` 0
        [returnRequest] <- flushCommands
        current <- getGame
        queue <- peekQueue
        liftIO $ writeIORef returnedRef $ Just (returnRequest, current, queue)
      (returnRequest, receiverWaiting, receiverQueue) <- fromJustNote "missing return request" <$> readIORef returnedRef
      returnedOriginRef <- newIORef Nothing
      originEnv <- newGameEnv departure.departingGame (const $ pure ()) 911
      runGameT originEnv do
        setQueue departure.departingQueue
        void $ runDebug $ ApplyEpicCommand returnRequest
        supportCount isHost `shouldReturn` 1
        supportPlacement departure.departingTarget `shouldReturn` if isHost then InPlayArea departure.departingOwner else AttachedTo (PlayerTarget departure.departingOwner)
        [returnAck] <- flushCommands
        current <- getGame
        queue <- peekQueue
        liftIO $ writeIORef returnedOriginRef $ Just (returnAck, current, queue)
      (returnAck, originWaiting, originQueue) <- fromJustNote "missing return acknowledgement" <$> readIORef returnedOriginRef
      originalAckRef <- newIORef Nothing
      resumedReceiverEnv <- newGameEnv receiverWaiting (const $ pure ()) 912
      runGameT resumedReceiverEnv do
        setQueue receiverQueue
        void $ runDebug $ ApplyEpicCommand returnAck
        [originalAck] <- flushCommands
        Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
        replica.epicPendingRequests `shouldBe` Set.empty
        liftIO $ writeIORef originalAckRef $ Just originalAck
      originalAck <- fromJustNote "missing original acknowledgement" <$> readIORef originalAckRef
      resumedOriginEnv <- newGameEnv originWaiting (const $ pure ()) 913
      runGameT resumedOriginEnv do
        setQueue originQueue
        void $ runDebug $ ApplyEpicCommand originalAck
        supportCount isHost `shouldReturn` 1
        Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
        replica.epicPendingRequests `shouldBe` Set.empty
        case departure.departingHero of
          HeroTarget hid -> (.resources) <$> getHero hid `shouldReturn` 1
          _ -> error "expected hero"
