module LordOfTheRings.Standalone.AnnuminasSetupSpec (spec) where

import LordOfTheRings.Card (CardType (QuestType, SetupType), EncounterCard (..), IsCard (toCard), cdCardType, toCardDef)
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas (theSiegeOfAnnuminas)
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (cityStrength)
import LordOfTheRings.Scenario.Types (scenarioEpic, scenarioMeta, scenarioQuestDeck, scenarioSetAside)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy)
import TestImport

installScenario :: Maybe Int -> GameT ()
installScenario ordinal = do
  let shared = fromJustNote "invalid Annúminas event" $ newAnnuminasEvent 3
      replica n = EpicReplica (GroupOrdinal n) shared [] [] [] mempty mempty Nothing
  current <- getGame
  putGame $ current & scenarioL .~ overAttrs (\a -> a {scenarioEpic = replica <$> ordinal}) theSiegeOfAnnuminas

setStageTwo :: GameT ()
setStageTwo = do
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioMeta = toJSON (2 :: Int)})

-- Complete the actual setup queue, including every starting-hand question and
-- the opening quest's Setup choice, without replacing the encounter deck.
finishSetupChoices :: Int -> GameT ()
finishSetupChoices remaining = do
  questions <- getsGame (.question)
  case headMay (mapToList questions) of
    Nothing -> pure ()
    Just (pid, _) | remaining > 0 -> do
      chooseOptionMatching "complete native Annúminas setup" pid (const True)
      finishSetupChoices (remaining - 1)
    _ -> error "Annúminas setup did not settle"

nativeSetupTable :: Int -> Maybe Int -> GameT () -> IO ()
nativeSetupTable size ordinal body = do
  seats <- for [1 .. size] $ \seat -> either (error . show) id <$> loadPlayer ("Defender " <> tshow seat) testDeck
  env <- newGameEnv (newTableIn theSiegeOfAnnuminas $ fromJustNote "empty Annúminas table" $ nonEmpty seats) (const $ pure ()) 1
  runGameT env do
    installScenario ordinal
    run Setup
    finishSetupChoices 100
    body

spec :: Spec
spec = describe "Annúminas scenario setup" do
  it "starts Standard with the AB quest deck and a City at strength twenty" . gameTest $ \_ -> do
    installScenario Nothing
    run $ DoStep 1 Setup
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.rebuildTheDefenses
    getsGame (map toCardCode . scenarioQuestDeck . toAttrs . (.scenario)) `shouldReturn` map toCardCode [Quests.defendTheCityAnnuminas, Quests.leadTheSortie]
    cityStrength `shouldReturn` 20
    getsGame (length . (.entities.objectives)) `shouldReturn` 1
    getEncounterDeck >>= (`shouldSatisfy` all ((/= toCardCode Objectives.annuminas) . toCardCode))

  for_ [(0, Quests.rebuildTheDefenses, Quests.defendTheCityAnnuminas, Quests.leadTheSortie), (1, Quests.raiseTheLevee, Quests.guardTheGateAnnuminas, Quests.rescueTheCaptivesAnnuminas), (2, Quests.delayTheEnemyAnnuminas, Quests.manTheWallsAnnuminas, Quests.stopTheAttack)] $ \(ordinal, initial, middle, finalStage) ->
    it ("assigns the correct Epic quest deck and keeps the City only at AB: " <> show ordinal) . gameTest $ \_ -> do
      installScenario $ Just ordinal
      run $ DoStep 1 Setup
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode initial
      getsGame (map toCardCode . scenarioQuestDeck . toAttrs . (.scenario)) `shouldReturn` map toCardCode [middle, finalStage]
      getsGame (length . (.entities.objectives)) `shouldReturn` (if ordinal == 0 then 1 else 0)
      cityStrength `shouldReturn` 50
      getEncounterDeck >>= (`shouldSatisfy` all ((/= toCardCode Objectives.annuminas) . toCardCode))

  it "shuffles all remaining Dúnedain and at least one Host before Standard stage two's revelation" . gameTest $ \_ -> do
    installScenario Nothing
    heroes <- (replicateM 3 $ genEncounterCard Objectives.dunedainOfAnnuminas) :: GameT [EncounterCard]
    hosts <- (replicateM 3 $ genEncounterCard Enemies.hostOfAngmar) :: GameT [EncounterCard]
    traverse_ (run . SetAside) $ heroes <> hosts
    _ <- setEncounterDeck []
    card <- genEncounterCard Quests.defendTheCityAnnuminas
    run $ AdvanceToQuest card
    deck <- getEncounterDeck
    length (filter ((== toCardCode Objectives.dunedainOfAnnuminas) . toCardCode) deck) `shouldBe` 3
    length (filter ((== toCardCode Enemies.hostOfAngmar) . toCardCode) deck) `shouldBe` 1
    getsGame (length . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 2

  it "leaves Epic reinforcements set aside at the stage-two transition" . gameTest $ \_ -> do
    installScenario $ Just 0
    reserve <- traverse genEncounterCard [Objectives.dunedainOfAnnuminas, Enemies.hostOfAngmar]
    traverse_ (run . SetAside) reserve
    _ <- setEncounterDeck []
    card <- genEncounterCard Quests.defendTheCityAnnuminas
    run $ AdvanceToQuest card
    getEncounterDeck `shouldReturn` []
    getsGame (scenarioSetAside . toAttrs . (.scenario)) >>= (`shouldSatisfy` all (`elem` reserve))
    getsGame (length . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` 2

  it "deals nine City damage once for a Standard player's elimination" . twoPlayerTest testDeck $ \one _ -> do
    installScenario Nothing
    run $ DoStep 1 Setup
    run $ EliminatePlayer one
    cityStrength `shouldReturn` 11
    run $ EliminatePlayer one
    cityStrength `shouldReturn` 11

  it "does not apply the Standard elimination penalty in Epic mode" . twoPlayerTest testDeck $ \one _ -> do
    installScenario $ Just 0
    run $ DoStep 1 Setup
    run $ EliminatePlayer one
    cityStrength `shouldReturn` 50

  it "discards staged Hosts at B for nine damage each, leaving engaged Hosts in play" . gameTest $ \pid -> do
    installScenario $ Just 0
    setStageTwo
    _ <- stageEnemy Enemies.hostOfAngmar
    engaged <- stageEnemy Enemies.hostOfAngmar
    run $ EngageEnemy pid engaged
    run $ UseCardAbility pid ScenarioSource 1 []
    cityStrength `shouldReturn` 41
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.hostOfAngmar]

  for_ [1, 2] $ \ordinal ->
    it ("moves staged Hosts in descending alphabetical stage order: " <> show ordinal) . gameTest $ \pid -> do
      installScenario $ Just ordinal
      setStageTwo
      _ <- stageEnemy Enemies.hostOfAngmar
      run $ UseCardAbility pid ScenarioSource 1 []
      getsGame (length . (.entities.enemies)) `shouldReturn` 0
      Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
      map (.epicCommandTo) replica.epicPendingCommands `shouldBe` [GroupOrdinal $ ordinal - 1]
      cityStrength `shouldReturn` 50

  for_ (Nothing : map Just [0, 1, 2]) $ \ordinal ->
    for_ [1 .. 4] $ \size ->
      it ("runs the complete printed opening setup with " <> show size <> " players at " <> show ordinal) . nativeSetupTable size ordinal $ do
        currentQuest <- getActiveQuest
        (.flipped) currentQuest `shouldBe` True
        let expected = case ordinal of
              Just 1 -> Quests.raiseTheLevee
              Just 2 -> Quests.delayTheEnemyAnnuminas
              _ -> Quests.rebuildTheDefenses
        toCardCode currentQuest `shouldBe` toCardCode expected
        cityStrength `shouldReturn` (if isJust ordinal then 50 else 20)
        attrs <- getsGame $ toAttrs . (.scenario)
        length attrs.scenarioSetAside `shouldBe` 10
        entities <- getsGame (.entities)
        let stagedCards = map toCardCode (toList entities.enemies) <> map toCardCode (toList entities.locations)
            supply = attrs.encounterDeck <> attrs.encounterDiscard <> attrs.scenarioSetAside
              <> [MkEncounterCard (toCard e).id (toCardCode e) | e <- toList entities.enemies]
              <> [MkEncounterCard (toCard l).id (toCardCode l) | l <- toList entities.locations]
              <> [MkEncounterCard (toCard o).id (toCardCode o) | o <- toList entities.objectives]
        length stagedCards `shouldBe` size
        when (ordinal /= Just 1) $ length (nub stagedCards) `shouldBe` size
        when (ordinal `elem` [Nothing, Just 0]) $ stagedCards `shouldSatisfy` elem (toCardCode Locations.gateOfAnnuminas)
        when (ordinal == Just 2) $ stagedCards `shouldSatisfy` elem (toCardCode Enemies.vanguardOfCarnDum)
        -- The other Epic teams remove their physical City during setup.
        length supply `shouldBe` (if ordinal `elem` [Just 1, Just 2] then 45 else 46)
        length (nub $ map (.id) supply) `shouldBe` length supply
        supply `shouldSatisfy` all (\card -> cdCardType (toCardDef card) `notElem` [QuestType, SetupType])
        getsGame (.outcome) `shouldReturn` Nothing
