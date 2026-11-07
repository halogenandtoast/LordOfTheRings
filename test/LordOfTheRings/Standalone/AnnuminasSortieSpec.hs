module LordOfTheRings.Standalone.AnnuminasSortieSpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Card (toCard)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getThreat)
import LordOfTheRings.Outcome (Outcome (Won, Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (scenarioEpic)
import LordOfTheRings.Standalone.AnnuminasCitySpec (stageCity, setReplica)
import LordOfTheRings.Standalone.AnnuminasLocationsSpec (stageLocation)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import LordOfTheRings.EpicTestHelpers (commandEffect, acknowledgeEpicCommand)
import TestImport

sortie :: GameT Target
sortie = do
  target <- putQuestStage Quests.leadTheSortie
  run $ Flip target
  resolveRevealed
  pure target

remoteEnemy :: EncounterCard -> Target -> EpicTarget
remoteEnemy card target = EpicTarget target card EpicEnemy False 0 False False False

spec :: Spec
spec = describe "Annúminas Lead the Sortie" do
  it "adds the set-aside Ram and Lieutenant without revealing an extra encounter in solo play" . gameTest $ \_ -> do
    ram <- genEncounterCard Locations.batteringRam
    lieutenant <- genEncounterCard Enemies.lieutenantOfAngmar
    run $ SetAside ram
    run $ SetAside lieutenant
    _ <- setEncounterDeck $ replicate 2 Locations.oldForestRoad
    _ <- sortie
    getsGame (map ((.cardId) . toAttrs) . toList . (.entities.locations)) `shouldReturn` [ram.id]
    getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [lieutenant.id]
    length <$> getEncounterDeck `shouldReturn` 2

  it "reveals one encounter for each other player after shuffling" . twoPlayerTest testDeck $ \_ _ -> do
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    _ <- sortie
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    length <$> getEncounterDeck `shouldReturn` 1

  it "blocks all progress while the Ram is in play, then permits it without defeating the quest" . gameTest $ \_ -> do
    target <- sortie
    ram <- stageLocation Locations.batteringRam
    run $ PlaceTokens GameSource target Progress 10
    (.progress) <$> getActiveQuest `shouldReturn` 0
    run $ RemoveFromGame $ LocationTarget ram
    run $ PlaceTokens GameSource target Progress 10
    (.progress) <$> getActiveQuest `shouldReturn` 10
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.leadTheSortie
    getsGame (.outcome) `shouldReturn` Nothing

  it "wins only at round end when the Lieutenant is in the victory display" . gameTest $ \_ -> do
    _ <- sortie
    lieutenant <- stageEnemy Enemies.lieutenantOfAngmar
    run $ AddToVictoryDisplay $ EnemyTarget lieutenant
    getsGame (.outcome) `shouldReturn` Nothing
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Won

  it "resolves its passive victory before forced city damage" . gameTest $ \_ -> do
    _ <- stageCity 1
    _ <- sortie
    lieutenant <- stageEnemy Enemies.lieutenantOfAngmar
    run $ AddToVictoryDisplay $ EnemyTarget lieutenant
    _ <- stageEnemy Enemies.hostOfAngmar
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Won

  it "publishes a durable shared win before its local queue is cleared" . gameTest $ \_ -> do
    setReplica []
    _ <- sortie
    lieutenant <- stageEnemy Enemies.lieutenantOfAngmar
    run $ AddToVictoryDisplay $ EnemyTarget lieutenant
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Won
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    annuminasOutcome replica.epicShared `shouldBe` Just Won
    map (.deltaKey) replica.epicPendingDeltas `shouldBe` [AnnuminasVictory]

  it "recognizes the Lieutenant in another team's victory display" . gameTest $ \_ -> do
    let stage = EpicStage 0 [EpicQuest (toCardCode Quests.leadTheSortie) 3 10 Nothing True] [toCardCode Enemies.lieutenantOfAngmar] False 0 False
    setReplica [EpicBoard (GroupOrdinal 1) "Other" [] False (Just stage)]
    _ <- sortie
    run $ CheckWindows [mkWhen RoundEnds]
    getsGame (.outcome) `shouldReturn` Just Won

  it "does not offer the rescue response without an eligible enemy at another stage" . gameTest $ \pid -> do
    target <- sortie
    _ <- stageEnemy Enemies.wargOfCarnDum
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False

  it "rejects unique and victory-point enemies as remote discard targets" . gameTest $ \pid -> do
    host <- genEncounterCard Enemies.hostOfAngmar
    lieutenant <- genEncounterCard Enemies.lieutenantOfAngmar
    let targets = [remoteEnemy host (CardIdTarget host.id), (remoteEnemy lieutenant (CardIdTarget lieutenant.id)) {epicTargetUnique = True}]
    setReplica [EpicBoard (GroupOrdinal 1) "Other" targets False Nothing]
    target <- sortie
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False

  it "requires a revealable encounter to pay the full response cost" . gameTest $ \pid -> do
    warg <- genEncounterCard Enemies.wargOfCarnDum
    setReplica [EpicBoard (GroupOrdinal 1) "Other" [remoteEnemy warg (CardIdTarget warg.id)] False Nothing]
    target <- sortie
    _ <- setEncounterDeck []
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.oldForestRoad
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False

  it "pays every player's threat and resolves its reveal before selecting the remote enemy" . twoPlayerTest testDeck $ \one two -> do
    warg <- genEncounterCard Enemies.wargOfCarnDum
    setReplica [EpicBoard (GroupOrdinal 1) "Other" [remoteEnemy warg (CardIdTarget warg.id)] False Nothing]
    target <- sortie
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf one target
    getThreat one `shouldReturn` 1
    getThreat two `shouldReturn` 1
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    chooseOptionMatching "choose remote enemy" one (const True)
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map commandEffect replica.epicPendingCommands `shouldBe` [EpicDiscardEnemy]
    map (.epicCommandTo) replica.epicPendingCommands `shouldBe` [GroupOrdinal 1]

  it "discards a remote enemy through the recipient's native leave-play processing" . gameTest $ \_ -> do
    setReplica []
    eid <- stageEnemy Enemies.wargOfCarnDum
    let command = EpicCommand UUID.nil (GroupOrdinal 1) (GroupOrdinal 0) (toCardCode Quests.leadTheSortie) EpicDiscardEnemy (EnemyTarget eid)
    run $ ApplyEpicCommand command
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.wargOfCarnDum]
    run $ ApplyEpicCommand command
    length <$> getEncounterDiscard `shouldReturn` 1

  it "publishes a group loss and propagates shared victories to a team without this quest" . gameTest $ \_ -> do
    setReplica []
    run $ GameOver Lost
    Just lostReplica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.deltaKey) lostReplica.epicPendingDeltas `shouldBe` [AnnuminasDefeat]
    annuminasOutcome lostReplica.epicShared `shouldBe` Just Lost

  it "ends another team when it receives the accepted shared victory" . gameTest $ \_ -> do
    setReplica []
    let initial = fromJustNote "invalid event" $ newAnnuminasEvent 3
        (won, _) = applySharedDelta (SharedDelta UUID.nil (GroupOrdinal 1) AnnuminasVictory 1) initial
    run $ SyncEpic won
    getsGame (.outcome) `shouldReturn` Just Won
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    replica.epicPendingDeltas `shouldBe` []

  describe "Stop the Attack" do
    let stopAttack = do
          target <- putQuestStage Quests.stopTheAttack
          run $ Flip target
          resolveRevealed
          pure target
    it "adds the physical set-aside Tunnel without adding the Lieutenant" . gameTest $ \_ -> do
      tunnel <- genEncounterCard Locations.siegeTunnel
      lieutenant <- genEncounterCard Enemies.lieutenantOfAngmar
      run $ SetAside tunnel
      run $ SetAside lieutenant
      _ <- stopAttack
      getsGame (map ((.cardId) . toAttrs) . toList . (.entities.locations)) `shouldReturn` [tunnel.id]
      getsGame (length . (.entities.enemies)) `shouldReturn` 0

    it "reveals only for the other players" . twoPlayerTest testDeck $ \_ _ -> do
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      _ <- stopAttack
      getsGame (length . (.entities.locations)) `shouldReturn` 1
      length <$> getEncounterDeck `shouldReturn` 1

    it "blocks progress for the Tunnel and allows progress once it leaves play" . gameTest $ \_ -> do
      target <- stopAttack
      tunnel <- stageLocation Locations.siegeTunnel
      run $ PlaceTokens GameSource target Progress 10
      (.progress) <$> getActiveQuest `shouldReturn` 0
      run $ AddToVictoryDisplay $ LocationTarget tunnel
      run $ PlaceTokens GameSource target Progress 10
      (.progress) <$> getActiveQuest `shouldReturn` 10
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.stopTheAttack

    it "wins at round end with the Lieutenant in the victory display" . gameTest $ \_ -> do
      _ <- stopAttack
      lieutenant <- stageEnemy Enemies.lieutenantOfAngmar
      run $ AddToVictoryDisplay $ EnemyTarget lieutenant
      getsGame (.outcome) `shouldReturn` Nothing
      run $ CheckWindows [mkWhen RoundEnds]
      getsGame (.outcome) `shouldReturn` Just Won

    it "pays for the response and sends a discard to another stage" . gameTest $ \pid -> do
      warg <- genEncounterCard Enemies.wargOfCarnDum
      setReplica [EpicBoard (GroupOrdinal 1) "Other" [remoteEnemy warg (CardIdTarget warg.id)] False Nothing]
      target <- stopAttack
      _ <- setEncounterDeck [Locations.oldForestRoad]
      run $ CheckWindows [mkWhen RoundEnds]
      chooseAbilityOf pid target
      getThreat pid `shouldReturn` 1
      getsGame (length . (.entities.locations)) `shouldReturn` 1
      chooseOptionMatching "choose remote enemy" pid (const True)
      Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      map commandEffect replica.epicPendingCommands `shouldBe` [EpicDiscardEnemy]

  describe "Rescue the Captives" do
    let rescue = do
          target <- putQuestStage Quests.rescueTheCaptivesAnnuminas
          run $ Flip target
          resolveRevealed
          pure target
    it "adds Enemy Camp and captures the chosen physical hero under the main quest" . gameTest $ \pid -> do
      camp <- genEncounterCard Locations.enemyCamp
      run $ SetAside camp
      hero <- getHeroWithCode "01012"
      run $ PlaceTokens GameSource (HeroTarget hero.id) Resource 3
      run $ PlaceTokens GameSource (HeroTarget hero.id) Damage 1
      _ <- rescue
      chooseTarget pid hero
      getsGame (map ((.cardId) . toAttrs) . toList . (.entities.locations)) `shouldReturn` [camp.id]
      current <- getGame
      let captives = concatMap (questUnderneath . toAttrs) current.entities.quests
      map (.id) captives `shouldBe` [(toCard hero).id]
      [c.owner | PlayerCard c <- captives] `shouldBe` [Just pid]
      getsGame (length . (.entities.heroes)) `shouldReturn` 2

    it "asks each player to capture one of their own heroes after the extra reveal" . twoPlayerTest testDeck $ \one two -> do
      own <- heroOf one "01012"
      other <- heroOf two "01007"
      _ <- setEncounterDeck [Locations.oldForestRoad]
      _ <- rescue
      getsGame (length . (.entities.locations)) `shouldReturn` 1
      chooseTarget one own
      chooseTarget two other
      getsGame (length . (.entities.heroes)) `shouldReturn` 4
      captives <- getsGame $ concatMap (questUnderneath . toAttrs) . (.entities.quests)
      [c.owner | PlayerCard c <- captives] `shouldSatisfy` (\owners -> length owners == 2 && Just one `elem` owners && Just two `elem` owners)

    it "sends a durable capture command to each other stage exactly once" . gameTest $ \pid -> do
      let remote = EpicBoard (GroupOrdinal 1) "Other" [] False Nothing
      setReplica [remote, remote, EpicBoard (GroupOrdinal 2) "Third" [] False Nothing]
      hero <- heroOf pid "01012"
      _ <- rescue
      chooseTarget pid hero
      Just initial <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      map (.epicCommandTo) initial.epicPendingCommands `shouldBe` [GroupOrdinal 1]
      traverse_ acknowledgeEpicCommand initial.epicPendingCommands
      Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      map commandEffect replica.epicPendingCommands `shouldBe` [EpicCaptureHero, EpicCaptureHero]
      map (.epicCommandTo) replica.epicPendingCommands `shouldBe` [GroupOrdinal 1, GroupOrdinal 2]

    it "captures at the recipient's main quest and ignores a duplicate delivery" . gameTest $ \pid -> do
      setReplica []
      _ <- putQuestStage Quests.stopTheAttack
      hero <- heroOf pid "01012"
      let command = EpicCommand UUID.nil (GroupOrdinal 1) (GroupOrdinal 0) (toCardCode Quests.rescueTheCaptivesAnnuminas) EpicCaptureHero ScenarioTarget
      run $ ApplyEpicCommand command
      chooseTarget pid hero
      run $ ApplyEpicCommand command
      getsGame (length . (.entities.heroes)) `shouldReturn` 2
      length <$> getsGame (concatMap (questUnderneath . toAttrs) . (.entities.quests)) `shouldReturn` 1

    it "blocks progress while Enemy Camp is in play and shares the round-end victory" . gameTest $ \pid -> do
      hero <- heroOf pid "01012"
      target <- rescue
      chooseTarget pid hero
      camp <- stageLocation Locations.enemyCamp
      run $ PlaceTokens GameSource target Progress 10
      (.progress) <$> getActiveQuest `shouldReturn` 0
      run $ AddToVictoryDisplay $ LocationTarget camp
      run $ PlaceTokens GameSource target Progress 10
      (.progress) <$> getActiveQuest `shouldReturn` 10
      lieutenant <- stageEnemy Enemies.lieutenantOfAngmar
      run $ AddToVictoryDisplay $ EnemyTarget lieutenant
      run $ CheckWindows [mkWhen RoundEnds]
      getsGame (.outcome) `shouldReturn` Just Won
