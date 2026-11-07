module LordOfTheRings.Standalone.AnnuminasEpicPreparationSpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (EffectDuration (UntilEndOfRound, UntilEndOfPhaseOf, UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (FewerStagingReveals))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (scenarioEpic, scenarioSetAside, questDeckL)
import LordOfTheRings.Standalone.AnnuminasCitySpec (setReplica)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (RoundEnds, RoundBegins), mkWhen)
import TestImport

waiting :: [CardDef]
waiting = [Enemies.lieutenantOfAngmar, Locations.batteringRam, Locations.enemyCamp, Locations.siegeTunnel]
  <> replicate 3 Enemies.hostOfAngmar <> replicate 3 Objectives.dunedainOfAnnuminas

spec :: Spec
spec = describe "Annúminas Epic preparation quests" do
  it "sets aside all ten special cards and stages one enemy per player without revealing surge" . twoPlayerTest testDeck $ \pid _ -> do
    target <- putQuestStage Quests.raiseTheLevee
    cards <- setEncounterDeck $ waiting <> replicate 3 Enemies.wargOfCarnDum
    run $ PerformSetup target
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    sort (map (.id) aside) `shouldBe` sort (map (.id) $ take 10 cards)
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` replicate 2 (toCardCode Enemies.wargOfCarnDum)
    length <$> getEncounterDeck `shouldReturn` 1
    getEncounterDiscard `shouldReturn` []
    getQuestion pid `shouldReturn` Nothing
    (.flipped) <$> getActiveQuest `shouldReturn` True

  it "accepts either an enemy or a location for each player's setup discard" . twoPlayerTest testDeck $ \_ _ -> do
    target <- putQuestStage Quests.raiseTheLevee
    _ <- setEncounterDeck [Enemies.wargOfCarnDum, Locations.hillsOfEvendim]
    run $ PerformSetup target
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    getsGame (length . (.entities.locations)) `shouldReturn` 1
    getEncounterDeck `shouldReturn` []
    getEncounterDiscard `shouldReturn` []

  for_ [1 .. 4] $ \size ->
    it ("sets up different enemies and permits the required Vanguard last for " <> show size <> " players") do
      seats <- for [1 .. size] $ \seat -> either (error . show) id <$> loadPlayer ("Player " <> tshow seat) testDeck
      env <- newGameEnv (newTableIn passageThroughMirkwood $ fromJustNote "empty table" $ nonEmpty seats) (const $ pure ()) 1
      runGameT env do
        target <- putQuestStage Quests.delayTheEnemyAnnuminas
        _ <- setEncounterDeck $ waiting <> concatMap (replicate 2)
          [Enemies.wargOfCarnDum, Enemies.orcOfAngmar, Enemies.angmarWargRider, Enemies.vanguardOfCarnDum]
        run $ PerformSetup target
        players <- getsGame (.playerOrder)
        for_ (zip [1 ..] players) $ \(seat, pid) ->
          chooseOptionMatching "choose Vanguard last" pid \case
            TargetLabel _ msgs -> any (\case
              AddToStaging card -> (toCardCode card == toCardCode Enemies.vanguardOfCarnDum) == (seat == size)
              _ -> False) msgs
            _ -> False
        enemies <- getsGame (map toCardCode . toList . (.entities.enemies))
        length enemies `shouldBe` size
        length (nub enemies) `shouldBe` size
        enemies `shouldSatisfy` elem (toCardCode Enemies.vanguardOfCarnDum)
        length <$> getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 10
        (.flipped) <$> getActiveQuest `shouldReturn` True

  for_ [Quests.raiseTheLevee, Quests.delayTheEnemyAnnuminas] $ \def ->
    it ("advances on the fourth round start: " <> show (toCardCode def)) . gameTest $ \_ -> do
      target <- putQuestStage def
      run $ Flip target
      next <- genEncounterCard Quests.runningOutOfTime
      current <- getGame
      putGame $ current & scenarioL %~ overAttrs (questDeckL .~ [next])
      replicateM_ 3 $ run $ CheckWindows [mkWhen RoundBegins]
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode def
      run $ CheckWindows [mkWhen RoundBegins]
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.runningOutOfTime

  it "removes progress as an effect, withholds the then benefit after a partial removal, and grants an ally after a full removal" . gameTest $ \pid -> do
    target <- putQuestStage Quests.raiseTheLevee
    run . SetAside =<< genEncounterCard Objectives.dunedainOfAnnuminas
    run $ Flip target
    run $ PlaceTokens GameSource target Progress 9
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    (.progress) <$> getActiveQuest `shouldReturn` 0
    getsGame (length . (.entities.allies)) `shouldReturn` 0
    run $ PlaceTokens GameSource target Progress 10
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    (.progress) <$> getActiveQuest `shouldReturn` 0
    getsGame (map (.owner) . toList . (.entities.allies)) `shouldReturn` [pid]
    getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` []
    getEncounterDeck `shouldReturn` []

  it "grants one local ally and queues exactly one grant for every other team" . gameTest $ \pid -> do
    setReplica [EpicBoard (GroupOrdinal 1) "Other" [] False Nothing, EpicBoard (GroupOrdinal 1) "Other" [] False Nothing]
    run . SetAside =<< genEncounterCard Objectives.dunedainOfAnnuminas
    target <- putQuestStage Quests.raiseTheLevee
    run $ Flip target
    run $ PlaceTokens GameSource target Progress 10
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    getsGame (length . (.entities.allies)) `shouldReturn` 1
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.epicCommandEffect) replica.epicPendingCommands `shouldBe` [EpicGrantDunedain]

  it "delays only other teams and does not grant the benefit after a partial removal" . gameTest $ \pid -> do
    setReplica [EpicBoard (GroupOrdinal 1) "Other" [] False Nothing]
    target <- putQuestStage Quests.delayTheEnemyAnnuminas
    run $ Flip target
    run $ PlaceTokens GameSource target Progress 9
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    Just initial <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    initial.epicPendingCommands `shouldBe` []
    run $ PlaceTokens GameSource target Progress 10
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.epicCommandEffect) replica.epicPendingCommands `shouldBe` [EpicDelayStaging]
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (FewerStagingReveals 1))

  it "persists a received delay across round and combat cleanup, then expires it after the next quest phase" . gameTest $ \_ -> do
    run $ ResolveEpicEffect (toCardCode Quests.delayTheEnemyAnnuminas) EpicDelayStaging ScenarioTarget
    run $ EndEffects UntilEndOfRound
    run $ EndEffects $ UntilEndOfPhaseOf CombatPhase
    getModifiers GameTarget >>= (`shouldSatisfy` elem (FewerStagingReveals 1))
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    run $ EndEffects $ UntilEndOfPhaseOf QuestPhase
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (FewerStagingReveals 1))

  it "can reduce a solo team's staging to zero and is used only for that next quest phase" . gameTest $ \_ -> do
    _ <- putQuestStage Quests.runningOutOfTime
    _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
    run $ ResolveEpicEffect (toCardCode Quests.delayTheEnemyAnnuminas) EpicDelayStaging ScenarioTarget
    run Staging
    length <$> getEncounterDeck `shouldReturn` 2
    run $ EndEffects $ UntilEndOfPhaseOf QuestPhase
    run $ EndEffects UntilEndOfPhase
    run Staging
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "delivers a grant exactly once after save and reload and uses the receiving team's current first player" . twoPlayerTest testDeck $ \_ other -> do
    let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
        sent = EpicCommand UUID.nil (GroupOrdinal 0) (GroupOrdinal 1) (toCardCode Quests.raiseTheLevee) EpicGrantDunedain ScenarioTarget
    current <- getGame
    putGame $ current & firstPlayerL .~ other & scenarioL %~ overAttrs (\a -> a {scenarioEpic = Just $ EpicReplica (GroupOrdinal 1) shared [] [] [] mempty mempty Nothing})
    replicateM_ 2 $ run . SetAside =<< genEncounterCard Objectives.dunedainOfAnnuminas
    run $ ApplyEpicCommand sent
    saved <- getGame
    restored <- either error pure $ eitherDecode $ encode saved
    putGame =<< preloadModifiers restored
    run $ ApplyEpicCommand sent
    getsGame (map (.owner) . toList . (.entities.allies)) `shouldReturn` [other]
    length <$> getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 1

  it "persists duplicate protection for a received staging reduction" . gameTest $ \_ -> do
    setReplica []
    let sent = EpicCommand UUID.nil (GroupOrdinal 1) (GroupOrdinal 0) (toCardCode Quests.delayTheEnemyAnnuminas) EpicDelayStaging ScenarioTarget
    run $ ApplyEpicCommand sent
    saved <- getGame
    restored <- either error pure $ eitherDecode $ encode saved
    putGame =<< preloadModifiers restored
    run $ ApplyEpicCommand sent
    mods <- getModifiers GameTarget
    length (filter (== FewerStagingReveals 1) mods) `shouldBe` 1

  for_ [Quests.raiseTheLevee, Quests.delayTheEnemyAnnuminas] $ \def ->
    it ("does not offer a progress-removal response with no progress: " <> show (toCardCode def)) . gameTest $ \pid -> do
      target <- putQuestStage def
      run $ Flip target
      run $ CheckWindows [mkWhen RoundEnds]
      offersAbilityOf pid target `shouldReturn` False

  it "discards a deck with no enemy or location without resolving treachery text or refilling the deck" . twoPlayerTest testDeck $ \_ _ -> do
    target <- putQuestStage Quests.raiseTheLevee
    _ <- setEncounterDeck $ replicate 2 Treacheries.wearyWarriors
    run $ PerformSetup target
    getEncounterDeck `shouldReturn` []
    length <$> getEncounterDiscard `shouldReturn` 2
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    getsGame (map (.damage) . toList . (.entities.heroes)) `shouldReturn` replicate 6 0
    (.flipped) <$> getActiveQuest `shouldReturn` True
