module LordOfTheRings.Standalone.AnnuminasGuardGateSpec (spec) where

import Data.UUID qualified as UUID
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Scenario.Scenarios.TheSiegeOfAnnuminas.Helpers (annuminasVictoryCopies)
import LordOfTheRings.Scenario.Types (scenarioEpic, questDeckL)
import LordOfTheRings.Standalone.AnnuminasCitySpec (setReplica)
import LordOfTheRings.Standalone.PrancingPonySpec (stageEnemy)
import LordOfTheRings.Window (WindowType (RoundEnds), mkWhen)
import LordOfTheRings.EpicTestHelpers (commandEffect)
import TestImport

gate :: GameT Target
gate = do
  target <- putQuestStage Quests.guardTheGateAnnuminas
  run $ Flip target
  resolveRevealed
  pure target

points :: GameT Int
points = getQuestPoints . toAttrs =<< getActiveQuest

boardWith :: Int -> Text -> [CardCode] -> EpicBoard
boardWith ordinal name victory = EpicBoard (GroupOrdinal ordinal) name [] False $ Just $
  EpicStage 0 [EpicQuest (toCardCode Quests.guardTheGateAnnuminas) 2 0 (Just 50) True] victory False 0 False

spec :: Spec
spec = describe "Annúminas Guard the Gate" do
  for_ [1 .. 4] $ \size ->
    it ("adds max(1, players-1) enemies without revealing their surge, " <> show size <> " players") do
      seats <- for [1 .. size] $ \seat -> either (error . show) id <$> loadPlayer ("Player " <> tshow seat) testDeck
      env <- newGameEnv (newTableIn passageThroughMirkwood $ fromJustNote "empty table" $ nonEmpty seats) (const $ pure ()) 1
      runGameT env do
        _ <- setEncounterDeck $ replicate 4 Enemies.wargOfCarnDum
        _ <- gate
        getsGame (length . (.entities.enemies)) `shouldReturn` max 1 (size - 1)
        length <$> getEncounterDeck `shouldReturn` 4 - max 1 (size - 1)
        getEncounterDiscard `shouldReturn` []

  it "shuffles the encounter discard before looking for its required enemy" . gameTest $ \_ -> do
    card <- genEncounterCard Enemies.wargOfCarnDum
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run $ AddToEncounterDiscard [card]
    _ <- gate
    getsGame (map ((.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` [card.id]
    getsGame (length . (.entities.locations)) `shouldReturn` 0

  it "reduces its quest points by ten per physical Host in any victory display, with fresh local state" . gameTest $ \_ -> do
    _ <- gate
    let host = toCardCode Enemies.hostOfAngmar
        staleOwn = boardWith 0 "Own" [host, host, host]
        other = boardWith 1 "Other" [host, host]
    setReplica [staleOwn, other, other, boardWith 2 "Walls" []]
    run $ Run []
    annuminasVictoryCopies host `shouldReturn` 2
    points `shouldReturn` 30
    eid <- stageEnemy Enemies.hostOfAngmar
    run $ AddToVictoryDisplay $ EnemyTarget eid
    points `shouldReturn` 20
    annuminasVictoryCopies host `shouldReturn` 3

  it "uses its modified progress requirement to defeat the stage" . gameTest $ \_ -> do
    target <- gate
    eid <- stageEnemy Enemies.hostOfAngmar
    run $ AddToVictoryDisplay $ EnemyTarget eid
    next <- genEncounterCard Quests.apprehendingTheKiller
    current <- getGame
    putGame $ current & scenarioL %~ overAttrs (questDeckL .~ [next])
    run $ PlaceTokens GameSource target Progress 39
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.guardTheGateAnnuminas
    run $ PlaceTokens GameSource target Progress 1
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.apprehendingTheKiller

  it "adds exactly one staging reveal and loses its text while blank" . gameTest $ \_ -> do
    target <- gate
    _ <- setEncounterDeck $ replicate 4 Locations.oldForestRoad
    run Staging
    getsGame (length . (.entities.locations)) `shouldReturn` 2
    run $ CreateEffect $ Effect GameSource target [Blank] UntilEndOfRound
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (AdditionalStagingReveals 1))
    run Staging
    getsGame (length . (.entities.locations)) `shouldReturn` 3

  it "lets each player at the chosen local stage select only a hero they control" . twoPlayerTest testDeck $ \one two -> do
    target <- gate
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf one target
    chooseLabel "$label.epic.thisGroup" one
    own <- heroOf one "01012"
    foreignHero <- heroOf two "01012"
    choices <- maybe [] questionChoices <$> getQuestion one
    length choices `shouldBe` 3
    choices `shouldSatisfy` all (\case TargetLabel t _ -> t /= foreignHero; _ -> False)
    chooseTarget one own
    chooseTarget two foreignHero
    resources <- getsGame (map (.resources) . toList . (.entities.heroes))
    sum resources `shouldBe` 2
    length (filter (== 1) resources) `shouldBe` 2

  it "offers remote stages, preserves the selection through reload, and sends the resource grant only there" . gameTest $ \pid -> do
    setReplica [boardWith 1 "Other" [], EpicBoard (GroupOrdinal 2) "Finished" [] False Nothing]
    target <- gate
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    choices <- maybe [] questionChoices <$> getQuestion pid
    length choices `shouldBe` 2
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    chooseOptionMatching "choose Other" pid $ \case Label label _ -> "Other" `isInfixOf` label; _ -> False
    getsGame (sum . map (.resources) . toList . (.entities.heroes)) `shouldReturn` 0
    Just replica <- getsGame $ scenarioEpic . toAttrs . (.scenario)
    map (.epicCommandTo) replica.epicPendingCommands `shouldBe` [GroupOrdinal 1]
    map commandEffect replica.epicPendingCommands `shouldBe` [EpicGrantHeroResources]

  it "excludes heroes barred from encounter-card resources while permitting a player-card-only restriction" . gameTest $ \pid -> do
    target <- gate
    blocked <- heroOf pid "01012"
    allowed <- heroOf pid "01007"
    run $ CreateEffect $ Effect GameSource blocked [CannotGainResourcesFromCardEffects] UntilEndOfRound
    run $ CreateEffect $ Effect GameSource allowed [CannotGainResourcesFromPlayerCardEffects] UntilEndOfRound
    run $ CheckWindows [mkWhen RoundEnds]
    chooseAbilityOf pid target
    chooseLabel "$label.epic.thisGroup" pid
    choices <- maybe [] questionChoices <$> getQuestion pid
    length choices `shouldBe` 2
    chooseTarget pid allowed
    getsGame (sum . map (.resources) . toList . (.entities.heroes)) `shouldReturn` 1

  it "delivers a grant to a team without this quest, saves the pending hero choice, and does not repeat on retry" . gameTest $ \pid -> do
    setReplica []
    let command = EpicCommand UUID.nil (GroupOrdinal 1) (GroupOrdinal 0) (toCardCode Quests.guardTheGateAnnuminas) EpicGrantHeroResources ScenarioTarget
    run $ ApplyEpicCommand command
    current <- getGame
    restored <- either error pure $ eitherDecode $ encode current
    putGame =<< preloadModifiers restored
    hero <- heroOf pid "01012"
    chooseTarget pid hero
    run $ ApplyEpicCommand command
    getsGame (sum . map (.resources) . toList . (.entities.heroes)) `shouldReturn` 1
    getQuestion pid `shouldReturn` Nothing
