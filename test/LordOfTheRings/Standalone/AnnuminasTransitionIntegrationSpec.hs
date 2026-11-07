module LordOfTheRings.Standalone.AnnuminasTransitionIntegrationSpec (spec) where

import LordOfTheRings.Ability.Types (isForcedAbility)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (IsCard (toCard))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Epic.Board (epicBoardFor)
import LordOfTheRings.Epic.Coordinator
import LordOfTheRings.Epic.Runner (epicRoundPhaseOf)
import LordOfTheRings.Epic.Types
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Window (getWindowAbilities)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotTakeDamage))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (scenarioQuestDeck)
import LordOfTheRings.Standalone.AnnuminasRoundIntegrationSpec
  (RoundTeam, external, teamAt, teamGames, reloadTeams, roundStep, freezeTeams, flushTeams, finishTeamChoices, makeTeam)
import LordOfTheRings.Window (WindowType (RoundBegins), mkWhen)
import TestImport

heroResources :: GameT Int
heroResources = getsGame $ sum . map (.resources) . toList . (.entities.heroes)

currentBoards :: [RoundTeam] -> IO [EpicBoard]
currentBoards teams = do
  states <- teamGames teams
  pure [epicBoardFor ordinal "" state | ((ordinal, _), state) <- zip teams states]

-- These fixtures use the native stage-two actors, their real remaining quest
-- cards and the same event planner as production. No encounter deck is swapped
-- once the final-stage revelations start.
prepareTeams :: [Int] -> IO ([RoundTeam], [EncounterCard], [Target])
prepareTeams progress = do
  teams <- traverse makeTeam [0, 1, 2]
  choices <- for (zip3 teams progress [Quests.leadTheSortie, Quests.rescueTheCaptivesAnnuminas, Quests.stopTheAttack]) $ \((_, env), amount, finalStage) -> runGameT env do
    target <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource target Progress amount
    selected <- genEncounterCard finalStage
    state <- getGame
    putGame $ state & scenarioL %~ overAttrs (\a -> a {scenarioQuestDeck = [selected]})
    pure selected
  heroes <- for teams $ \(_, env) -> runGameT env do
    pid <- getLead
    heroOf pid "01012"
  pure (teams, choices, heroes)

finishPriorRound :: SharedEventState -> [RoundTeam] -> IO SharedEventState
finishPriorRound shared teams = do
  for_ teams $ \(_, env) -> runGameT env $ run EndRound
  roundStep teams `shouldReturn` Just FreezeEpicTeams
  freezeTeams teams
  afterStages <- foldM (\latest ordinal -> do
    roundStep teams `shouldReturn` Just (ResolveEpicTeam $ GroupOrdinal ordinal)
    runGameT (teamAt ordinal teams) $ external BeginEpicRoundEnd >> finishTeamChoices 30
    flushTeams 30 latest teams) shared [0, 1, 2]
  roundStep teams `shouldReturn` Just ExpireEpicTeams
  for_ teams $ \(_, env) -> runGameT env $ external FinishEpicRoundEffects
  roundStep teams `shouldReturn` Just BeginEpicTeams
  pure afterStages

beginAndFreeze :: Int -> [RoundTeam] -> IO ()
beginAndFreeze retainedResources teams = do
  for_ teams $ \(_, env) -> runGameT env $ external BeginEpicRoundBeginning
  for_ teams $ \(_, env) -> runGameT env do
    getsGame (.round) `shouldReturn` 1
    heroResources `shouldReturn` retainedResources
  roundStep teams `shouldReturn` Just FreezeEpicBeginningTeams
  snapshots <- for teams $ \(_, env) -> runGameT env $ filter isForcedAbility <$> getWindowAbilities [mkWhen RoundBegins]
  for_ (zip teams snapshots) $ \((_, env), abilities) -> runGameT env $ external $ FreezeEpicRoundBeginning abilities
  roundStep teams `shouldReturn` Just ReleaseEpicBeginningTeams

spec :: Spec
spec = describe "Annúminas synchronized final-stage transition" do
  it "prepares all three final quests before cross-team capture, then resolves the old Walls trigger before releasing any resources" do
    (initial, selected, heroes) <- prepareTeams [50, 50, 50]
    oldQuestIds <- for initial $ \(_, env) -> runGameT env $ toId <$> getActiveQuest
    heroCardIds <- for (zip initial heroes) $ \((_, env), target) -> runGameT env do
      pid <- getLead
      character <- getsGame $ fromJustNote "hero missing" . find ((== target) . toTarget) . toList . (.entities.heroes)
      run $ PlaceTokens GameSource target Resource 5
      run $ PlaceTokens GameSource target Damage 2
      _ <- putAttachmentOn pid Attachments.celebriansStone target
      pure (toCard character).id
    hostCard <- runGameT (teamAt 0 initial) do
      host <- genEncounterCard Enemies.hostOfAngmar
      run $ SetAside host
      traverse_ (\def -> run . SetAside =<< genEncounterCard def) [Locations.batteringRam, Enemies.lieutenantOfAngmar]
      pure host
    runGameT (teamAt 1 initial) $ run . SetAside =<< genEncounterCard Locations.enemyCamp
    runGameT (teamAt 2 initial) $ run . SetAside =<< genEncounterCard Locations.siegeTunnel
    let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
    synchronized <- flushTeams 30 shared initial
    afterRound <- finishPriorRound synchronized initial
    expired <- reloadTeams initial
    beginAndFreeze 5 expired
    ready <- reloadTeams expired
    boards <- currentBoards ready
    annuminasStageTwoReady boards `shouldBe` True
    for_ (zip ready selected) $ \((_, env), card) -> runGameT env $ external $ PrepareEpicQuestAdvance EpicDefeatQuest card
    prepared <- reloadTeams ready
    roundStep prepared `shouldReturn` Just RevealEpicTeams
    for_ (zip prepared selected) $ \((_, env), card) -> runGameT env do
      active <- getActiveQuest
      active.stage `shouldBe` 3
      active.flipped `shouldBe` False
      (toCard active).id `shouldBe` card.id
      getsGame (length . (.entities.heroes)) `shouldReturn` 3
    for_ prepared $ \(_, env) -> runGameT env $ external RevealEpicQuestAdvance
    -- Resolve C first while B and F still await their own When Revealed click.
    -- Its remote captures must use the already-installed stage-three quests.
    runGameT (teamAt 1 prepared) do
      resolveRevealed
      pid <- getLead
      chooseTarget pid $ fromJustNote "missing Gate hero" $ atMay heroes 1
    firstCapture <- flushTeams 30 afterRound prepared
    capturing <- reloadTeams prepared
    runGameT (teamAt 0 capturing) do
      pid <- getLead
      chooseTarget pid $ fromJustNote "missing City hero" $ atMay heroes 0
    secondCapture <- flushTeams 30 firstCapture capturing
    capturingLast <- reloadTeams capturing
    runGameT (teamAt 2 capturingLast) do
      pid <- getLead
      chooseTarget pid $ fromJustNote "missing Walls hero" $ atMay heroes 2
    capturedShared <- flushTeams 30 secondCapture capturingLast
    captured <- reloadTeams capturingLast
    for_ (zip3 captured heroCardIds oldQuestIds) $ \((_, env), capturedId, oldQuestId) -> runGameT env do
      active <- getActiveQuest
      active.stage `shouldBe` 3
      [card.id | PlayerCard card <- questUnderneath $ toAttrs active] `shouldBe` [capturedId]
      state <- getGame
      let oldQuest = fromJustNote "missing cached stage two" $ lookup oldQuestId $ mapToList $ gameResolvingQuests state
      questUnderneath (toAttrs oldQuest) `shouldBe` []
      getsGame (length . (.entities.heroes)) `shouldReturn` 2
      getsGame (length . (.entities.attachments)) `shouldReturn` 0
      heroResources `shouldReturn` 0
    runGameT (teamAt 0 captured) resolveRevealed
    runGameT (teamAt 2 captured) resolveRevealed
    afterRevelations <- flushTeams 30 capturedShared captured
    revealed <- reloadTeams captured
    roundStep revealed `shouldReturn` Just ReleaseEpicBeginningTeams
    for_ revealed $ \(_, env) -> runGameT env $ external ReleaseEpicRoundBeginning
    -- Man the Walls' source quest is gone, but its previously frozen forced
    -- trigger requests the physical Host from B. Even B must wait for that reply.
    roundStep revealed `shouldReturn` Nothing
    runGameT (teamAt 0 revealed) do
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundBegan
      heroResources `shouldReturn` 0
    runGameT (teamAt 2 revealed) do
      getsGame epicRoundPhaseOf `shouldReturn` Just EpicRoundBeginResolving
      heroResources `shouldReturn` 0
    _ <- flushTeams 30 afterRevelations revealed
    began <- reloadTeams revealed
    roundStep began `shouldReturn` Just ReleaseEpicResources
    runGameT (teamAt 2 began) do
      hosts <- getsGame $ filter ((== toCardCode Enemies.hostOfAngmar) . toCardCode) . toList . (.entities.enemies)
      map ((.cardId) . toAttrs) hosts `shouldBe` [hostCard.id]
    for_ began $ \(_, env) -> runGameT env $ heroResources `shouldReturn` 0
    for_ began $ \(_, env) -> runGameT env $ external BeginEpicRoundResources
    finished <- reloadTeams began
    for_ finished $ \(_, env) -> runGameT env do
      getsGame (.round) `shouldReturn` 1
      heroResources `shouldReturn` 2
      getsGame gameFrozenRoundBeginsAbilities `shouldReturn` Nothing
    runGameT (teamAt 0 finished) do
      lieutenant <- getsGame $ toId . fromJustNote "Lieutenant missing" . find ((== toCardCode Enemies.lieutenantOfAngmar) . toCardCode) . toList . (.entities.enemies)
      getModifiers lieutenant >>= (`shouldSatisfy` elem CannotTakeDamage)

  it "retains all three stage-two quests and their progress when one team is incomplete" do
    (initial, _, _) <- prepareTeams [50, 49, 50]
    oldQuestIds <- for initial $ \(_, env) -> runGameT env $ toId <$> getActiveQuest
    let shared = fromJustNote "invalid event" $ newAnnuminasEvent 3
    synchronized <- flushTeams 30 shared initial
    afterRound <- finishPriorRound synchronized initial
    expired <- reloadTeams initial
    beginAndFreeze 0 expired
    boards <- currentBoards expired
    annuminasStageTwoReady boards `shouldBe` False
    for_ expired $ \(_, env) -> runGameT env $ external ReleaseEpicRoundBeginning
    _ <- flushTeams 30 afterRound expired
    began <- reloadTeams expired
    roundStep began `shouldReturn` Just ReleaseEpicResources
    for_ (zip3 began oldQuestIds [50, 49, 50]) $ \((_, env), oldId, progress) -> runGameT env do
      active <- getActiveQuest
      toId active `shouldBe` oldId
      active.stage `shouldBe` 2
      active.progress `shouldBe` progress
      questUnderneath (toAttrs active) `shouldBe` []
    for_ began $ \(_, env) -> runGameT env $ external BeginEpicRoundResources
    runGameT (teamAt 1 began) do
      target <- toTarget <$> getActiveQuest
      external $ PlaceTokens GameSource target Progress 1
      active <- getActiveQuest
      active.stage `shouldBe` 2
      active.progress `shouldBe` 50
    for_ began $ \(_, env) -> runGameT env do
      (.stage) <$> getActiveQuest `shouldReturn` 2
      heroResources `shouldReturn` 3
