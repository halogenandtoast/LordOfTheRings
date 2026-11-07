module LordOfTheRings.LostRealmEngineSpec (spec) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Criteria (Criterion (FewerInVictoryDisplay))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Entities (treacheriesL)
import LordOfTheRings.Helpers.Window (passesCriteria, windowMatchesM)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (
  currentQuestIsSideQuest,
  getCurrentQuest,
  getEncounterSideQuests,
  getSideQuests,
  getTimeCounters,
  progressPlacedThisRound,
  questCardsInPlay,
 )
import LordOfTheRings.Matcher.Types (cardIs)
import LordOfTheRings.Matcher.Window (WindowMatcher (SideQuestRevealedWindow, TreacheryWithTraitRevealedWindow), windowMatches)
import LordOfTheRings.Message.Lifted (
  chooseNewCurrentQuest,
  chooseOrRunOneM,
  labeled,
  raiseThreat,
  removeProgressFromEachQuest,
  valourLabeled,
 )
import LordOfTheRings.Modifier (ModifierType (CannotBeCurrentQuest, ProgressPerRoundLimit))
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Trait (Trait (Hazard, Sorcery))
import LordOfTheRings.Treachery (newTreachery)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), WindowType (PhaseEnds, Revealed), mkAfter, mkWhen)
import TestImport

-- | Flies and Spiders as the main quest, and Search the Ruins revealed from the encounter
-- deck, the Old Forest Road under it for its surge.
withSideQuest :: GameT (Target, Target)
withSideQuest = do
  main <- putQuestStage Quests.fliesAndSpiders
  _ <- setEncounterDeck [Quests.searchTheRuins, Locations.oldForestRoad]
  run RevealEncounterCard
  side <- getsGame (toTarget . fromJustNote "no side quest" . find ((== toCardCode Quests.searchTheRuins) . toCardCode) . toList . (.entities.quests))
  pure (main, side)

progressOn :: Target -> GameT Int
progressOn t = getsGame (maybe 0 (.progress) . find ((== t) . toTarget) . toList . (.entities.quests))

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

spec :: Spec
spec = describe "The Lost Realm engine" do
  describe "Encounter side quests" do
    it "are revealed into the staging area, and surge as their keyword says" . gameTest $ \_ -> do
      (main, side) <- withSideQuest
      getSideQuests `shouldReturn` [side]
      getEncounterSideQuests `shouldReturn` [side]
      questCardsInPlay `shouldReturn` 2
      getCurrentQuest `shouldReturn` Just main
      -- the surge revealed the Old Forest Road
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.oldForestRoad]

    it "may be chosen as the current quest in the quest phase" . gameTest $ \pid -> do
      (_, side) <- withSideQuest
      run $ BeginPhase QuestPhase
      chooseTarget pid side
      getCurrentQuest `shouldReturn` Just side
      currentQuestIsSideQuest `shouldReturn` True

    it "take the progress as the current quest, and go to the victory display once defeated" . gameTest $ \pid -> do
      (main, side) <- withSideQuest
      run $ RaiseThreat pid 10
      run $ SetCurrentQuest side
      threat0 <- threatOf pid
      run $ PlaceProgressOnQuest 20
      -- Search the Ruins' response lowers each player's threat by 3
      threatOf pid `shouldReturn` threat0 - 3
      victoryDisplay `shouldReturn` [toCardCode Quests.searchTheRuins]
      getEncounterSideQuests `shouldReturn` []
      getCurrentQuest `shouldReturn` Just main
      progressOn main `shouldReturn` 0

    it "keep their text active in the staging area" . gameTest $ \pid -> do
      _ <- withSideQuest
      threat0 <- threatOf pid
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      threatOf pid `shouldReturn` threat0 + 2

    it "are not offered as the current quest when they cannot be, nor is the main quest" . gameTest $ \pid -> do
      (main, side) <- withSideQuest
      run $ CreateEffect $ Effect GameSource main [CannotBeCurrentQuest] UntilEndOfRound
      run $ BeginPhase QuestPhase
      Just question <- getQuestion pid
      [() | Label "$label.questAgainstMainQuest" _ <- questionChoices question] `shouldBe` []
      [t | TargetLabel t _ <- questionChoices question] `shouldBe` [side]

    it "give way to another quest card a card chooses as the current quest" . gameTest $ \pid -> do
      (main, side) <- withSideQuest
      run $ SetCurrentQuest side
      runQueueT $ chooseNewCurrentQuest pid GameTarget
      void mainLoop
      Just question <- getQuestion pid
      [t | TargetLabel t _ <- questionChoices question] `shouldBe` [main]
      chooseTarget pid main
      getCurrentQuest `shouldReturn` Just main

    it "open the window for a side quest revealed" . gameTest $ \_ -> do
      (_, side) <- withSideQuest
      windowMatches (SideQuestRevealedWindow After) (mkAfter $ Revealed side) `shouldBe` True
      windowMatches (SideQuestRevealedWindow After) (mkAfter $ Revealed GameTarget) `shouldBe` False

    it "can be discarded or removed from the game" . gameTest $ \_ -> do
      (_, side) <- withSideQuest
      run $ Discard GameSource side
      getSideQuests `shouldReturn` []
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Quests.searchTheRuins]

  describe "Progress on quest cards" do
    it "is counted each round, and kept to a quest card's limit for the round" . gameTest $ \_ -> do
      main <- putQuestStage Quests.fliesAndSpiders
      run $ CreateEffect $ Effect GameSource main [ProgressPerRoundLimit 3] UntilEndOfRound
      run $ PlaceTokens GameSource main Progress 2
      run $ PlaceTokens GameSource main Progress 2
      progressOn main `shouldReturn` 3
      progressPlacedThisRound main `shouldReturn` 3

    it "is removed from each quest card in play" . gameTest $ \_ -> do
      (main, side) <- withSideQuest
      run $ PlaceTokens GameSource main Progress 1
      run $ PlaceTokens GameSource side Progress 3
      runQueueT $ removeProgressFromEachQuest GameSource 2
      void mainLoop
      progressOn main `shouldReturn` 0
      progressOn side `shouldReturn` 1

  describe "Creating an individual staging area" do
    it "retains an empty home stage in a solo game" . gameTest $ \pid -> do
      home <- putQuestStage Quests.fliesAndSpiders
      run $ SeparatePlayer pid
      getCurrentQuest `shouldReturn` Nothing
      g <- getGame
      let separation = fromJustNote "no separation" $ gameSeparation g
      map areaPlayers (separationAreas separation) `shouldBe` [[], [pid]]
      map toTarget (concatMap (toList . (.quests) . areaHeldEntities) $ separationAreas separation) `shouldBe` [home]
      map areaPlayers (areasInTurnOrder g separation) `shouldBe` [[pid]]
      areaLead g separation (headEx $ separationAreas separation) `shouldBe` pid
      let publicAreas = parseMaybe (withObject "game" (.: "areas")) (publicGame g) :: Maybe [Value]
          areaPlayersAndCards = traverse (withObject "area" $ \o -> (,) <$> o .: "players" <*> o .: "cards")
          parsedAreas = publicAreas >>= parseMaybe areaPlayersAndCards :: Maybe [([PlayerId], [CardId])]
      fmap (map (\(players, cards) -> (players, length cards))) parsedAreas `shouldBe` Just [([pid], 0), ([], 1)]
      let (following, _) = eachAreaThen (const [Run []]) id (Run []) g
      following `shouldBe` Run [FocusArea pid, Run []]
      case eitherDecode (encode g) :: Either String Game of
        Left err -> liftIO $ expectationFailure err
        Right restored -> do
          toJSON (gameSeparation restored) `shouldBe` toJSON (gameSeparation g)
          map areaPlayers (areasInTurnOrder restored separation) `shouldBe` [[pid]]

    it "takes characters and engaged enemies while leaving staging cards at home" . twoPlayerTest testDeck $ \one two -> do
      home <- putQuestStage Quests.fliesAndSpiders
      road <- genEncounterCard Locations.oldForestRoad
      foe <- genEncounterCard Enemies.forestSpider
      run $ AddToStaging road
      run $ AddToStaging foe
      eid <- getsGame (toId . headEx . toList . (.entities.enemies))
      run $ PlaceEnemy eid (EngagedWith two)
      heroes <- getsGame (filter ((== two) . (.owner)) . toList . (.entities.heroes))
      run $ SeparatePlayer two
      getsGame (map toId . toList . (.entities.heroes)) `shouldReturn` map toId heroes
      getsGame (map toId . toList . (.entities.enemies)) `shouldReturn` [eid]
      getsGame (null . (.entities.locations)) `shouldReturn` True
      run $ FocusArea one
      getCurrentQuest `shouldReturn` Just home
      getsGame (null . (.entities.enemies)) `shouldReturn` True
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "splits players successively without losing the original stage or an existing area" . twoPlayerTest testDeck $ \one two -> do
      home <- putQuestStage Quests.fliesAndSpiders
      run $ SeparatePlayer two
      away <- putQuestStage Quests.fliesAndSpiders
      run $ FocusArea one
      getCurrentQuest `shouldReturn` Just home
      run $ SeparatePlayer one
      getCurrentQuest `shouldReturn` Nothing
      g <- getGame
      let separation = fromJustNote "no separation" $ gameSeparation g
      map areaPlayers (separationAreas separation) `shouldBe` [[], [one], [two]]
      separationFirstPlayer separation `shouldBe` one
      map toTarget (concatMap (toList . (.quests) . areaHeldEntities) $ separationAreas separation) `shouldBe` [home, away]
      map areaPlayers (areasInTurnOrder g separation) `shouldBe` [[one], [two]]
      run $ FocusArea two
      getCurrentQuest `shouldReturn` Just away
      getsGame (keys . (.players)) `shouldReturn` [two]
      run $ FocusArea one
      getsGame (keys . (.players)) `shouldReturn` [one]

  describe "Resolving rules at an empty quest stage" do
    it "changes the empty stage without changing the player's private stage" . gameTest $ \pid -> do
      home@(QuestTarget qid) <- putQuestStage Quests.fliesAndSpiders
      run $ SeparatePlayer pid
      away <- putQuestStage Quests.fliesAndSpiders
      run $ PlaceTokens GameSource away Progress 1
      run $ FocusQuestArea qid
      getCurrentQuest `shouldReturn` Just home
      getsGame (null . (.players)) `shouldReturn` True
      getsGame (null . (.entities.heroes)) `shouldReturn` True
      run $ PlaceTokens GameSource home Progress 2
      run $ FocusArea pid
      getCurrentQuest `shouldReturn` Just away
      progressOn away `shouldReturn` 1
      getsGame (length . (.entities.heroes)) `shouldReturn` 3
      run $ JoinQuestArea pid qid
      getCurrentQuest `shouldReturn` Just home
      progressOn home `shouldReturn` 2
      getsGame (isNothing . gameSeparation) `shouldReturn` True

    it "moves an empty stage's location to the victory display without eliminating players" . gameTest $ \pid -> do
      QuestTarget qid <- putQuestStage Quests.fliesAndSpiders
      run . AddToStaging =<< genEncounterCard Locations.hollowCircle
      lid <- getsGame (toId . headEx . toList . (.entities.locations))
      run $ SeparatePlayer pid
      away <- putQuestStage Quests.fliesAndSpiders
      run $ FocusQuestArea qid
      run $ AddToVictoryDisplay (LocationTarget lid)
      victoryDisplay `shouldReturn` [toCardCode Locations.hollowCircle]
      getsGame (.outcome) `shouldReturn` Nothing
      run $ FocusArea pid
      getCurrentQuest `shouldReturn` Just away
      getsGame (length . (.players)) `shouldReturn` 1

    it "does not repeat entry effects while focusing an empty stage" . gameTest $ \pid -> do
      QuestTarget qid <- putQuestStage Quests.fliesAndSpiders
      run . AddToStaging =<< genEncounterCard Locations.ravenChiefsCamp
      lid <- getsGame (toId . headEx . toList . (.entities.locations))
      run $ RemoveTokens GameSource (LocationTarget lid) TimeCounter 1
      run $ SeparatePlayer pid
      _ <- putQuestStage Quests.fliesAndSpiders
      run $ FocusQuestArea qid
      getTimeCounters (LocationTarget lid) `shouldReturn` 2
      getsGame (Map.member lid . gameLeftPlayLocations) `shouldReturn` False
      run $ FocusArea pid
      run $ FocusQuestArea qid
      getTimeCounters (LocationTarget lid) `shouldReturn` 2

    it "restores an empty stage in focus from a saved game and can return its player directly" . gameTest $ \pid -> do
      home@(QuestTarget qid) <- putQuestStage Quests.fliesAndSpiders
      run $ SeparatePlayer pid
      _ <- putQuestStage Quests.fliesAndSpiders
      run $ FocusQuestArea qid
      current <- getGame
      case eitherDecode (encode current) of
        Left err -> liftIO $ expectationFailure err
        Right restored -> putGame restored
      restored <- getGame
      let separation = fromJustNote "no separation" $ gameSeparation restored
      map areaPlayers (filter (isFocusedArea restored) $ separationAreas separation) `shouldBe` [[]]
      let publicAreas = parseMaybe (withObject "game" (.: "areas")) (publicGame restored) :: Maybe [Value]
          readAreas = traverse (withObject "area" $ \o -> (,) <$> o .: "players" <*> o .: "focused")
          parsedAreas = publicAreas >>= parseMaybe readAreas :: Maybe [([PlayerId], Bool)]
      parsedAreas `shouldBe` Just [([pid], False), ([], True)]
      let (following, _) = eachAreaThen (const [Run []]) id (Run []) restored
      following `shouldBe` Run [FocusArea pid, Run []]
      getCurrentQuest `shouldReturn` Just home
      run $ JoinQuestArea pid qid
      getCurrentQuest `shouldReturn` Just home
      getsGame (length . (.entities.heroes)) `shouldReturn` 3
      getsGame (isNothing . gameSeparation) `shouldReturn` True

    it "preserves two other private stages while visiting the empty home stage" . twoPlayerTest testDeck $ \one two -> do
      home@(QuestTarget qid) <- putQuestStage Quests.fliesAndSpiders
      run $ SeparatePlayer two
      awayTwo <- putQuestStage Quests.fliesAndSpiders
      run $ FocusArea one
      run $ SeparatePlayer one
      awayOne <- putQuestStage Quests.fliesAndSpiders
      run $ FocusQuestArea qid
      getCurrentQuest `shouldReturn` Just home
      run $ PlaceTokens GameSource home Progress 3
      run $ FocusArea two
      getCurrentQuest `shouldReturn` Just awayTwo
      run $ FocusArea one
      getCurrentQuest `shouldReturn` Just awayOne
      run $ FocusQuestArea qid
      progressOn home `shouldReturn` 3
      current <- getGame
      let separation = fromJustNote "no separation" $ gameSeparation current
      length (filter (isFocusedArea current) $ separationAreas separation) `shouldBe` 1
      map areaPlayers (areasInTurnOrder current separation) `shouldBe` [[one], [two]]

  describe "Returning to a quest's staging area" do
    it "returns to an empty solo stage and keeps its quest progress" . gameTest $ \pid -> do
      home@(QuestTarget qid) <- putQuestStage Quests.fliesAndSpiders
      run $ PlaceTokens GameSource home Progress 2
      run $ SeparatePlayer pid
      _ <- putQuestStage Quests.fliesAndSpiders
      run $ JoinQuestArea pid qid
      getCurrentQuest `shouldReturn` Just home
      progressOn home `shouldReturn` 2
      getsGame (isNothing . gameSeparation) `shouldReturn` True
      getsGame (length . (.entities.quests)) `shouldReturn` 1
      getLead `shouldReturn` pid

    it "moves locations without leaving-play or re-entry effects" . gameTest $ \pid -> do
      QuestTarget qid <- putQuestStage Quests.fliesAndSpiders
      run . AddToStaging =<< genEncounterCard Locations.ravenChiefsCamp
      lid <- getsGame (toId . headEx . toList . (.entities.locations))
      run $ RemoveTokens GameSource (LocationTarget lid) TimeCounter 1
      getTimeCounters (LocationTarget lid) `shouldReturn` 2
      run $ SeparatePlayer pid
      getsGame (Map.member lid . gameLeftPlayLocations) `shouldReturn` False
      _ <- putQuestStage Quests.fliesAndSpiders
      run $ JoinQuestArea pid qid
      getTimeCounters (LocationTarget lid) `shouldReturn` 2
      getsGame (Map.member lid . gameLeftPlayLocations) `shouldReturn` False

    it "joins an occupied stage with its characters and both sets of staging cards" . twoPlayerTest testDeck $ \one two -> do
      home@(QuestTarget qid) <- putQuestStage Quests.fliesAndSpiders
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      run $ SeparatePlayer two
      _ <- putQuestStage Quests.fliesAndSpiders
      run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      eid <- getsGame (toId . headEx . toList . (.entities.enemies))
      run $ PlaceEnemy eid (EngagedWith two)
      run . AddToStaging =<< genEncounterCard Enemies.forestSpider
      run $ FocusArea one
      run $ JoinQuestArea two qid
      getCurrentQuest `shouldReturn` Just home
      getsGame (length . (.players)) `shouldReturn` 2
      getsGame (length . (.entities.heroes)) `shouldReturn` 6
      getsGame (length . (.entities.locations)) `shouldReturn` 2
      placements <- getsGame (map (.placement) . toList . (.entities.enemies))
      placements `shouldSatisfy` (\ps -> length ps == 2 && all (`elem` ps) [EngagedWith two, StagingArea])
      getsGame (isNothing . gameSeparation) `shouldReturn` True
      getLead `shouldReturn` one

    it "returns separately after saving while preserving the other private stage" . twoPlayerTest testDeck $ \one two -> do
      home@(QuestTarget qid) <- putQuestStage Quests.fliesAndSpiders
      run $ SeparatePlayer two
      away <- putQuestStage Quests.fliesAndSpiders
      run $ FocusArea one
      run $ SeparatePlayer one
      _ <- putQuestStage Quests.fliesAndSpiders
      current <- getGame
      case eitherDecode (encode current) of
        Left err -> liftIO $ expectationFailure err
        Right restored -> putGame restored
      run $ JoinQuestArea one qid
      getCurrentQuest `shouldReturn` Just home
      getsGame (length . (.players)) `shouldReturn` 1
      getsGame (isJust . gameSeparation) `shouldReturn` True
      run $ FocusArea two
      getCurrentQuest `shouldReturn` Just away
      run $ JoinQuestArea two qid
      getCurrentQuest `shouldReturn` Just home
      getsGame (length . (.players)) `shouldReturn` 2
      getsGame (isNothing . gameSeparation) `shouldReturn` True
      getLead `shouldReturn` one

    it "keeps side quests from the returning area" . gameTest $ \pid -> do
      home@(QuestTarget qid) <- putQuestStage Quests.fliesAndSpiders
      run $ SeparatePlayer pid
      (_, side) <- withSideQuest
      run $ JoinQuestArea pid qid
      getSideQuests `shouldReturn` [side]
      getCurrentQuest `shouldReturn` Just home
      getsGame (length . (.entities.quests)) `shouldReturn` 2

    it "leaves the game unchanged if the quest is already in the player's area" . gameTest $ \pid -> do
      _ <- putQuestStage Quests.fliesAndSpiders
      run $ SeparatePlayer pid
      QuestTarget qid <- putQuestStage Quests.fliesAndSpiders
      current <- getGame
      toJSON (joinQuestArea pid qid current) `shouldBe` toJSON current

  describe "Time counters" do
    it "are not placed again as a separated area comes back into focus" . twoPlayerTest testDeck $ \one two -> do
      run $ SeparatePlayers [[one], [two]]
      run . AddToStaging =<< genEncounterCard Locations.ravenChiefsCamp
      camp <- getsGame (toTarget . fromJustNote "no camp" . headMay . toList . (.entities.locations))
      getTimeCounters camp `shouldReturn` 3
      run $ FocusArea two
      run $ FocusArea one
      getTimeCounters camp `shouldReturn` 3

  describe "Revealed treacheries" do
    it "open the window for a treachery of the trait" . gameTest $ \_ -> do
      t <- newTreachery =<< genEncounterCard Treacheries.caveIn
      putGame . (entitiesL . treacheriesL %~ insertEntity t) =<< getGame
      windowMatchesM (TreacheryWithTraitRevealedWindow After Hazard) (mkAfter $ Revealed $ toTarget t) `shouldReturn` True
      windowMatchesM (TreacheryWithTraitRevealedWindow After Sorcery) (mkAfter $ Revealed $ toTarget t) `shouldReturn` False

  describe "Valour" do
    let trigger pid = runQueueT $ chooseOrRunOneM pid do
          labeled "questAgainstMainQuest" $ raiseThreat pid 1
          valourLabeled pid $ raiseThreat pid 5
    it "resolves a card's other trigger without asking below 40 threat" . gameTest $ \pid -> do
      threat0 <- threatOf pid
      trigger pid
      void mainLoop
      threatOf pid `shouldReturn` threat0 + 1

    it "offers the Valour trigger at 40 threat or higher" . gameTest $ \pid -> do
      run . RaiseThreat pid . (40 -) =<< threatOf pid
      trigger pid
      void mainLoop
      chooseLabel "$label.valour" pid
      threatOf pid `shouldReturn` 45

  describe "The victory display" do
    it "takes a card from the encounter discard pile" . gameTest $ \_ -> do
      card <- genEncounterCard Locations.oldForestRoad
      run $ AddToEncounterDiscard [card]
      run $ AddToVictoryDisplay (CardIdTarget card.id)
      victoryDisplay `shouldReturn` [toCardCode Locations.oldForestRoad]
      getEncounterDiscard `shouldReturn` []
      passesCriteria (FewerInVictoryDisplay 1 $ cardIs Locations.oldForestRoad) `shouldReturn` False
      passesCriteria (FewerInVictoryDisplay 2 $ cardIs Locations.oldForestRoad) `shouldReturn` True
