module LordOfTheRings.Standalone.FogQuestsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getCurrentQuest, getThreat)
import LordOfTheRings.Modifier (ModifierType (Blank, CannotBeDefeated, SkipQuestPhase))
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (questDeckL, scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds, RoundEnds), mkWhen)
import TestImport

progressOn :: Target -> GameT Int
progressOn target = getsGame (maybe 0 (.progress) . find ((== target) . toTarget) . toList . (.entities.quests))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))

nextStages :: [CardDef] -> GameT ()
nextStages defs = do
  cards <- traverse genEncounterCard defs
  current <- getGame
  putGame $ current & scenarioL %~ overAttrs (questDeckL .~ cards)

hollowAside :: GameT EncounterCard
hollowAside = do
  card <- genEncounterCard Locations.hollowCircle
  run $ SetAside card
  pure card

acrossStage :: GameT Target
acrossStage = do
  target <- putQuestStage Quests.acrossTheDowns
  _ <- hollowAside
  _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
  nextStages [Quests.fliesAndSpiders]
  run $ Flip target
  resolveRevealed
  pure target

spec :: Spec
spec = describe "Fog opening quests" do
  describe "The House of Tom Bombadil" do
    it "sets aside Hollow Circle, shuffles the remaining physical cards, and turns over" . gameTest $ \_ -> do
      target <- putQuestStage Quests.theHouseOfTomBombadil
      cards <- setEncounterDeck [Locations.hollowCircle, Locations.oldForestRoad, Locations.northDownsFog]
      run $ PerformSetup target
      waiting <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      sort (map (.id) waiting) `shouldBe` sort (map (.id) $ take 1 cards)
      sort . map (.id) <$> getEncounterDeck `shouldReturn` sort (map (.id) $ drop 1 cards)
      (.flipped) <$> getActiveQuest `shouldReturn` True
      getsGame (null . (.entities.locations)) `shouldReturn` True

    it "skips staging and character commitment during the quest phase" . gameTest $ \pid -> do
      target <- putQuestStage Quests.theHouseOfTomBombadil
      cards <- setEncounterDeck [Locations.oldForestRoad]
      run $ Flip target
      run $ BeginPhase QuestPhase
      getEncounterDeck `shouldReturn` cards
      getQuestion pid `shouldReturn` Nothing
      getModifiers GameTarget >>= (`shouldSatisfy` elem SkipQuestPhase)

    it "advances at round end into stage 2 and resolves its arrival" . gameTest $ \_ -> do
      target <- putQuestStage Quests.theHouseOfTomBombadil
      _ <- hollowAside
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      nextStages [Quests.acrossTheDowns, Quests.fliesAndSpiders]
      run $ Flip target
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      progressOn target `shouldReturn` 0
      run $ CheckWindows [mkWhen RoundEnds]
      resolveRevealed
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.acrossTheDowns
      getsGame (map toCardCode . toList . (.entities.locations)) >>= (`shouldSatisfy` elem (toCardCode Locations.hollowCircle))
      length <$> getEncounterDeck `shouldReturn` 1
      getModifiers GameTarget >>= (`shouldSatisfy` notElem SkipQuestPhase)

    it "has no side B effects while unflipped or blank" . gameTest $ \_ -> do
      target <- putQuestStage Quests.theHouseOfTomBombadil
      getModifiers GameTarget >>= (`shouldSatisfy` notElem SkipQuestPhase)
      run $ CheckWindows [mkWhen RoundEnds]
      progressOn target `shouldReturn` 0
      run $ Flip target
      run $ CreateEffect $ Effect GameSource target [Blank] UntilEndOfRound
      getModifiers GameTarget >>= (`shouldSatisfy` notElem SkipQuestPhase)
      run $ CheckWindows [mkWhen RoundEnds]
      progressOn target `shouldReturn` 0

  describe "Across the Downs" do
    it "adds the set-aside Hollow Circle and gives each player their own reveal, including surge" . twoPlayerTest testDeck $ \one two -> do
      target <- putQuestStage Quests.acrossTheDowns
      hollow <- hollowAside
      firstHero <- heroOf one "01012"
      secondHero <- heroOf two "01012"
      current <- getGame
      putGame $ current & phaseL .~ QuestPhase
      run $ CommitToQuest firstHero
      run $ CommitToQuest secondHero
      _ <- setEncounterDeck [Treacheries.frozenByFear, Treacheries.dreadfulSong, Treacheries.dreadfulSong, Locations.oldForestRoad]
      initialOne <- getThreat one
      initialTwo <- getThreat two
      run $ Flip target
      resolveRevealed
      hosts <- getsGame (map (.placement) . toList . (.entities.treacheries))
      length (filter (== AttachedTo firstHero) hosts) `shouldBe` 1
      length (filter (== AttachedTo secondHero) hosts) `shouldBe` 1
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.oldForestRoad]
      getThreat one `shouldReturn` initialOne + 1
      getThreat two `shouldReturn` initialTwo + 1
      getsGame (any ((== hollow.id) . (.cardId) . toAttrs) . toList . (.entities.locations)) `shouldReturn` True
      getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` []
      getsGame gameRevealer `shouldReturn` Nothing

    it "raises each player's threat at refresh end without repeating at the round-end window" . twoPlayerTest testDeck $ \one two -> do
      _ <- acrossStage
      initialOne <- getThreat one
      initialTwo <- getThreat two
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      getThreat one `shouldReturn` initialOne + 1
      getThreat two `shouldReturn` initialTwo + 1
      run $ CheckWindows [mkWhen RoundEnds]
      getThreat one `shouldReturn` initialOne + 1
      getThreat two `shouldReturn` initialTwo + 1

    it "opens Elfhelm's response to threat raised by a quest card" . gameTest $ \pid -> do
      elf <- putAllyIntoPlay pid Allies.elfhelm
      _ <- acrossStage
      initial <- getThreat pid
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      offersAbilityOf pid elf `shouldReturn` True
      chooseAbilityOf pid elf
      getThreat pid `shouldReturn` initial

    it "keeps progress until Hollow Circle enters the victory display, then advances immediately" . gameTest $ \_ -> do
      target <- acrossStage
      run $ PlaceTokens GameSource target Progress 14
      progressOn target `shouldReturn` 14
      getCurrentQuest `shouldReturn` Just target
      lid <- getsGame (toId . fromJustNote "Hollow Circle missing" . find ((== toCardCode Locations.hollowCircle) . toCardCode) . toList . (.entities.locations))
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.fliesAndSpiders
      victoryDisplay `shouldReturn` [toCardCode Locations.hollowCircle]

    it "does not count a discarded Hollow Circle as being in the victory display" . gameTest $ \_ -> do
      target <- acrossStage
      lid <- getsGame (toId . fromJustNote "Hollow Circle missing" . find ((== toCardCode Locations.hollowCircle) . toCardCode) . toList . (.entities.locations))
      run $ Discard GameSource (LocationTarget lid)
      run $ PlaceTokens GameSource target Progress 14
      getCurrentQuest `shouldReturn` Just target
      progressOn target `shouldReturn` 14
      getModifiers target >>= (`shouldSatisfy` elem CannotBeDefeated)

    it "loses its victory restriction and refresh effect while blank" . gameTest $ \pid -> do
      target <- acrossStage
      initial <- getThreat pid
      run $ CreateEffect $ Effect GameSource target [Blank] UntilEndOfRound
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      getThreat pid `shouldReturn` initial
      getModifiers target >>= (`shouldSatisfy` notElem CannotBeDefeated)
      run $ PlaceTokens GameSource target Progress 14
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.fliesAndSpiders

    it "restores its progress and victory restriction from a saved game" . gameTest $ \_ -> do
      target <- acrossStage
      run $ PlaceTokens GameSource target Progress 14
      current <- getGame
      case eitherDecode (encode current) of
        Left err -> liftIO $ expectationFailure err
        Right restored -> putGame =<< preloadModifiers restored
      progressOn target `shouldReturn` 14
      getModifiers target >>= (`shouldSatisfy` elem CannotBeDefeated)
      getsGame (Map.size . (.entities.quests)) `shouldReturn` 1
