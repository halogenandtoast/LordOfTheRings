module LordOfTheRings.Standalone.AnnuminasOpeningSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardType (..), cdCardType, toCardDef)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (preloadModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (Blank))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (cityStrength)
import LordOfTheRings.Scenario.Types (questDeckL, scenarioSetAside)
import LordOfTheRings.Standalone.AnnuminasCitySpec (stageCity, setReplica)
import LordOfTheRings.Window (WindowType (RoundBegins, RoundEnds), mkWhen)
import TestImport

waiting :: [CardDef]
waiting = [Enemies.lieutenantOfAngmar, Locations.batteringRam, Locations.enemyCamp, Locations.siegeTunnel]
  <> replicate 3 Enemies.hostOfAngmar <> replicate 3 Objectives.dunedainOfAnnuminas

resources :: GameT Int
resources = countTokens Resource . questTokens . toAttrs <$> getActiveQuest

spec :: Spec
spec = describe "Annúminas opening quest" do
  it "sets aside every printed copy and requires the Gate in a solo setup" . gameTest $ \pid -> do
    target <- putQuestStage Quests.rebuildTheDefenses
    cards <- setEncounterDeck $ waiting <> [Locations.gateOfAnnuminas, Locations.gateOfAnnuminas, Locations.cityWallAnnuminas]
    run $ PerformSetup target
    choices <- maybe [] questionChoices <$> getQuestion pid
    length choices `shouldBe` 2
    let gate = fromJustNote "missing gate" $ atMay cards 10
    chooseTarget pid (CardIdTarget gate.id)
    (.flipped) <$> getActiveQuest `shouldReturn` True
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    sort (map (.id) aside) `shouldBe` sort (map (.id) $ take 10 cards)
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.gateOfAnnuminas]
    length <$> getEncounterDeck `shouldReturn` 2

  for_ [False, True] $ \gateFirst ->
    it ("allows either player to choose the required Gate and prevents duplicate titles: " <> show gateFirst) . twoPlayerTest testDeck $ \one two -> do
      target <- putQuestStage Quests.rebuildTheDefenses
      cards <- setEncounterDeck [Locations.gateOfAnnuminas, Locations.gateOfAnnuminas, Locations.cityWallAnnuminas, Locations.cityWallAnnuminas, Locations.hillsOfEvendim]
      run $ PerformSetup target
      let setupCard n = fromJustNote "missing setup card" $ atMay cards n
      chooseTarget one $ CardIdTarget (setupCard $ if gateFirst then 0 else 2).id
      offered <- maybe [] questionChoices <$> getQuestion two
      length offered `shouldBe` if gateFirst then 3 else 2
      chooseTarget two $ CardIdTarget (setupCard $ if gateFirst then 2 else 0).id
      getsGame (sort . map toCardCode . toList . (.entities.locations)) `shouldReturn` sort (map toCardCode [Locations.gateOfAnnuminas, Locations.cityWallAnnuminas])
      (.flipped) <$> getActiveQuest `shouldReturn` True
      length <$> getEncounterDeck `shouldReturn` 3

  for_ [1 .. 4] $ \size ->
    it ("allows the Gate to be chosen last with " <> show size <> " players") do
      seats <- for [1 .. size] $ \seat -> either (error . show) id <$> loadPlayer ("Player " <> tshow seat) testDeck
      env <- newGameEnv (newTableIn passageThroughMirkwood $ fromJustNote "empty table" $ nonEmpty seats) (const $ pure ()) 1
      runGameT env do
        target <- putQuestStage Quests.rebuildTheDefenses
        _ <- setEncounterDeck $ concatMap (replicate 2)
          [Locations.cityWallAnnuminas, Locations.hillsOfEvendim, Locations.shoresOfLakeNenuial, Locations.arnorBattlefield, Locations.gateOfAnnuminas]
        run $ PerformSetup target
        players <- getsGame (.playerOrder)
        for_ (zip [1 ..] players) $ \(seat, pid) ->
          chooseOptionMatching "choose distinct setup locations, Gate last" pid \case
            TargetLabel _ msgs -> any (\case
              AddToStaging card -> (toCardCode card == toCardCode Locations.gateOfAnnuminas) == (seat == size)
              _ -> False) msgs
            _ -> False
        locations <- getsGame (map toCardCode . toList . (.entities.locations))
        length locations `shouldBe` size
        length (nub locations) `shouldBe` size
        locations `shouldSatisfy` elem (toCardCode Locations.gateOfAnnuminas)
        (.flipped) <$> getActiveQuest `shouldReturn` True

  it "persists a partially completed setup search" . twoPlayerTest testDeck $ \one two -> do
    target <- putQuestStage Quests.rebuildTheDefenses
    cards <- setEncounterDeck [Locations.cityWallAnnuminas, Locations.gateOfAnnuminas, Locations.hillsOfEvendim]
    run $ PerformSetup target
    chooseTarget one $ CardIdTarget (fromJustNote "missing wall" $ headMay cards).id
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    chooseTarget two $ CardIdTarget (fromJustNote "missing gate" $ atMay cards 1).id
    (.flipped) <$> getActiveQuest `shouldReturn` True
    getsGame (length . (.entities.locations)) `shouldReturn` 2

  it "places one resource at the beginning of each round and advances upon the fourth" . gameTest $ \_ -> do
    target <- putQuestStage Quests.rebuildTheDefenses
    run $ Flip target
    next <- genEncounterCard Quests.runningOutOfTime
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (questDeckL .~ [next])
    for_ [1 .. 3] $ \expected -> do
      run $ CheckWindows [mkWhen RoundBegins]
      resources `shouldReturn` expected
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.rebuildTheDefenses
    run $ CheckWindows [mkWhen RoundBegins]
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.runningOutOfTime

  it "advances and opens the next stage's arrival before collecting the round's resources" . gameTest $ \_ -> do
    target <- putQuestStage Quests.rebuildTheDefenses
    run $ Flip target
    run $ PlaceTokens GameSource target Resource 3
    next <- genEncounterCard Quests.runningOutOfTime
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (questDeckL .~ [next])
    run BeginRound
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.runningOutOfTime
    getsGame (map (.resources) . toList . (.entities.heroes)) `shouldReturn` [0, 0, 0]

  it "cannot be defeated by quest progress and has no printed progress requirement" . gameTest $ \_ -> do
    target <- putQuestStage Quests.rebuildTheDefenses
    run $ Flip target
    run $ PlaceTokens GameSource target Progress 100
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.rebuildTheDefenses
    (.progress) <$> getActiveQuest `shouldReturn` 100

  it "offers no round effects before flipping or while blank" . gameTest $ \pid -> do
    target <- putQuestStage Quests.rebuildTheDefenses
    run $ CheckWindows [mkWhen RoundBegins]
    resources `shouldReturn` 0
    run $ Flip target
    run $ CreateEffect $ Effect GameSource target [Blank] UntilEndOfRound
    run $ CheckWindows [mkWhen RoundBegins]
    resources `shouldReturn` 0
    run $ PlaceTokens GameSource target Progress 10
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False

  it "requires the full ten progress and pays exactly ten for the city's ten strength" . gameTest $ \pid -> do
    _ <- stageCity 20
    target <- putQuestStage Quests.rebuildTheDefenses
    run $ Flip target
    run $ PlaceTokens GameSource target Progress 9
    run $ CheckWindows [mkWhen RoundEnds]
    offersAbilityOf pid target `shouldReturn` False
    cityStrength `shouldReturn` 20
    run $ PlaceTokens GameSource target Progress 1
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    (.progress) <$> getActiveQuest `shouldReturn` 0
    cityStrength `shouldReturn` 30

  it "pays ten progress regardless of player count and contributes to the shared city" . twoPlayerTest testDeck $ \one _ -> do
    setReplica []
    target <- putQuestStage Quests.rebuildTheDefenses
    run $ Flip target
    run $ PlaceTokens GameSource target Progress 25
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf one target
    (.progress) <$> getActiveQuest `shouldReturn` 15
    cityStrength `shouldReturn` 60
    offersAbilityOf one target `shouldReturn` False

  it "registers scenario quests separately from player side quests" do
    for_ [Quests.rebuildTheDefenses, Quests.investigatingTheCrime, Quests.runningOutOfTime, Quests.apprehendingTheKiller] $ \def -> do
      Map.lookup (toCardCode def) Quests.allQuestCards `shouldBe` Just def
      Map.lookup (toCardCode def) Quests.allPlayerSideQuestCards `shouldBe` Nothing
    map (cdCardType . toCardDef) (toList Quests.allPlayerSideQuestCards) `shouldSatisfy` all (== PlayerSideQuestType)
    pure () :: IO ()
