module LordOfTheRings.VoiceOfIsengard.TheFordsOfIsenSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getLocationQuestPoints)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getTimeCounters)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theFordsOfIsen)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Decklist (barredHeroes)
import TestImport

fordsTest :: (PlayerId -> GameT ()) -> IO ()
fordsTest = gameTestIn theFordsOfIsen testDeck

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

-- | The searches for Dunland enemies take the first one offered, or find none.
searchFindsNothing :: PlayerId -> GameT ()
searchFindsNothing pid =
  getQuestion pid >>= \case
    Just (QuestionLabel "$label.searchFoundNothing" _ _ _) -> chooseLabel "$label.continue" pid >> searchFindsNothing pid
    Just (QuestionLabel l _ _ _)
      | l `elem` ["$label.theFordsOfIsen.searchDunland", "$label.searchChoose"] ->
          chooseOptionMatching "a Dunland enemy" pid (const True) >> searchFindsNothing pid
    _ -> pure ()

setup :: PlayerId -> GameT ()
setup pid = flipFirstQuest pid >> searchFindsNothing pid

locationOf :: CardDef -> GameT LocationId
locationOf def =
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

staged :: CardDef -> GameT LocationId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

grimaAlly :: GameT (Maybe AllyAttrs)
grimaAlly =
  getsGame (fmap toAttrs . find ((== toCardCode Objectives.grimaTheFordsOfIsen) . toCardCode) . toList . (.entities.allies))

handOf :: PlayerId -> GameT [Card]
handOf pid = (.hand) <$> getPlayer pid

-- | The stage in play, turned to its B side, its When Revealed resolved.
flippedStage :: PlayerId -> CardDef -> GameT Target
flippedStage pid def = do
  q <- putQuestStage def
  run $ Flip q
  resolveWhenRevealed pid
  searchFindsNothing pid
  pure q

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Cards put into the hand, the modifiers read again after.
fillHand :: PlayerId -> Int -> GameT ()
fillHand pid n = do
  replicateM_ n $ putCardIntoHand pid Enemies.forestSpider
  run $ CheckWindows []

spec :: Spec
spec = describe "The Fords of Isen" do
  it "is a scenario the game can be dealt" . fordsTest $ \_ ->
    isJust (lookupScenario "the-fords-of-isen") `shouldBe` True

  it "bars the Gríma hero, as its Gríma objective-ally is unique" . fordsTest $ \_ -> do
    barredHeroes "the-fords-of-isen" False [deckList ["07002", "01012", "01007"] []] `shouldBe` ["Gríma"]
    barredHeroes "to-catch-an-orc" False [deckList ["07002", "01012", "01007"] []] `shouldBe` []

  it "stages The Islet with Gríma attached, and gives the stage 5 time counters" . fordsTest $ \pid -> do
    setup pid
    islet <- locationOf Locations.theIslet
    (.placement) <$> getLocation islet `shouldReturn` StagingArea
    grima <- getsGame (map (.placement) . toList . (.entities.objectives))
    grima `shouldBe` [AttachedTo (LocationTarget islet)]
    (getTimeCounters . toTarget =<< getActiveQuest) `shouldReturn` 5
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Objectives.grimaTheFordsOfIsen)

  it "cannot advance until The Islet leaves play and Gríma joins the first player" . fordsTest $ \pid -> do
    setup pid
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 6
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.fightAtTheFords
    run . Discard . toTarget =<< locationOf Locations.theIslet
    fmap (.placement) <$> grimaAlly `shouldReturn` Just (InPlayArea pid)
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.dunlendingAttack

  it "is lost as the last time counter leaves Fight at the Fords" . fordsTest $ \pid -> do
    setup pid
    q <- toTarget <$> getActiveQuest
    run $ RemoveTokens GameSource q TimeCounter 5
    outcome `shouldReturn` Just Lost

  it "is lost if Gríma the ally leaves play, and he exhausts to draw a card" . fordsTest $ \pid -> do
    setup pid
    run . Discard . toTarget =<< locationOf Locations.theIslet
    grima <- fromJustNote "no Gríma" <$> grimaAlly
    _ <- setEncounterDeck []
    drawn <- length <$> handOf pid
    run $ ActionWindow pid
    chooseAbilityOf pid (toTarget grima)
    length <$> handOf pid `shouldReturn` drawn + 1
    passActions pid
    run $ Discard (toTarget grima)
    outcome `shouldReturn` Just Lost

  it "is lost if Gríma the ally leaves play other than by discard" . fordsTest $ \pid -> do
    setup pid
    run . Discard . toTarget =<< locationOf Locations.theIslet
    grima <- fromJustNote "no Gríma" <$> grimaAlly
    run $ ShuffleIntoOwnersDeck (toTarget grima)
    outcome `shouldReturn` Just Lost

  it "bars resources from card effects while Fords of Isen is staged, and fills hands to 5 as it becomes active" . fordsTest $ \pid -> do
    fords <- staged Locations.fordsOfIsen
    hero <- getHeroWithCode "01012"
    run $ PlaceTokens (PlayerSource pid) (toTarget hero) Resource 1
    getHeroResources hero.id `shouldReturn` 0
    fillHand pid 2
    run $ TravelTo fords
    length <$> handOf pid `shouldReturn` 5

  it "gives The King's Road 3 more quest points with 3 cards in a hand" . fordsTest $ \pid -> do
    road <- staged Locations.theKingsRoad
    getLocationQuestPoints road `shouldReturn` 2
    fillHand pid 3
    getLocationQuestPoints road `shouldReturn` 5

  it "draws a card for Pillaging and Burning, then raises threat by the hand" . fordsTest $ \pid -> do
    fillHand pid 1
    threat <- (.threat) <$> getPlayer pid
    reveal [Treacheries.pillagingAndBurning]
    length <$> handOf pid `shouldReturn` 2
    (.threat) <$> getPlayer pid `shouldReturn` threat + 2

  it "draws Ill Tidings into the first player's hand, where it stays, surging from 5 cards" . fordsTest $ \pid -> do
    fillHand pid 3
    reveal [Treacheries.illTidings]
    hand <- handOf pid
    map toCardCode hand `shouldSatisfy` elem (toCardCode Treacheries.illTidings)
    run $ DiscardRandomFromHand pid 4
    map toCardCode <$> handOf pid `shouldReturn` [toCardCode Treacheries.illTidings]
    fillHand pid 3
    threat <- (.threat) <$> getPlayer pid
    reveal [Treacheries.illTidings, Treacheries.pillagingAndBurning]
    raised <- (.threat) <$> getPlayer pid
    raised `shouldSatisfy` (> threat)

  it "removes a time counter from the current quest for Down from the Hills" . fordsTest $ \pid -> do
    q <- flippedStage pid Quests.dunlendingAttack
    reveal [Treacheries.downFromTheHills]
    chooseLabel "$label.downFromTheHills.removeTimeCounter" pid
    getTimeCounters q `shouldReturn` 1

  it "deals as much damage as each hand holds as Dunlending Attack runs out of time" . fordsTest $ \pid -> do
    q <- flippedStage pid Quests.dunlendingAttack
    fillHand pid 2
    hero <- heroOf pid "01012"
    run $ RemoveTokens GameSource q TimeCounter 2
    -- assigned first, then dealt together, as Dunland Raider's (FAQ 3.117)
    chooseTarget pid hero
    getCharacterDamage hero `shouldReturn` 0
    chooseTarget pid hero
    getCharacterDamage hero `shouldReturn` 2
    getTimeCounters q `shouldReturn` 2

  it "adds the enemies discarded from the encounter deck as Hold the Fords runs out of time, and wins only with none in play" . fordsTest $ \pid -> do
    q <- flippedStage pid Quests.holdTheFords
    fillHand pid 2
    _ <- setEncounterDeck [Enemies.forestSpider, Treacheries.illTidings, Enemies.forestSpider]
    run $ RemoveTokens GameSource q TimeCounter 3
    enemies <- getsGame (toList . (.entities.enemies))
    map (.placement) enemies `shouldBe` [StagingArea]
    getTimeCounters q `shouldReturn` 3
    run $ PlaceTokens GameSource q Progress 16
    outcome `shouldReturn` Nothing
    for_ enemies \e -> run $ Discard (toTarget e)
    outcome `shouldReturn` Just Won
