module LordOfTheRings.Standalone.AnnuminasSetupSpec (spec) where

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
      replica n = EpicReplica (GroupOrdinal n) shared [] [] [] mempty mempty
  current <- getGame
  putGame $ current & scenarioL .~ overAttrs (\a -> a {scenarioEpic = replica <$> ordinal}) theSiegeOfAnnuminas

setStageTwo :: GameT ()
setStageTwo = do
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (\a -> a {scenarioMeta = toJSON (2 :: Int)})

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
