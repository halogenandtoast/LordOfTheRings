module LordOfTheRings.Dreadnaught.EngineSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (cdCardType, cdOtherSide, toCardDef)
import LordOfTheRings.Card.CardType (CardType (ObjectiveAllyType))
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Keyword (Keyword (Massive, Pillage))
import LordOfTheRings.Matcher (getPillage)
import LordOfTheRings.Message.Lifted qualified as Lifted
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Sailing (isCorsairDeckCard)
import LordOfTheRings.Scenario.Types (corsairDeckL)
import LordOfTheRings.Scenario.Types qualified as Scenario
import LordOfTheRings.Window (WindowType (EnemyAttacking), mkAfter)
import TestImport

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "enemy missing"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.enemies)
    )

pureIt :: String -> IO () -> Spec
pureIt = it

spec :: Spec
spec = describe "Dreadnaught engine" do
  pureIt "keeps Sea Monster in the encounter deck and the Corsairs in their own deck" do
    monster <- genEncounterCard Enemies.seaMonster
    corsair <- genEncounterCard Enemies.corsairPlunderer
    ship <- genEncounterCard Enemies.warGalley
    isCorsairDeckCard monster `shouldBe` False
    isCorsairDeckCard corsair `shouldBe` True
    isCorsairDeckCard ship `shouldBe` False

  pureIt "captures the new Corsair Skirmisher printing with the reciprocal ship side" do
    cdOtherSide Enemies.corsairSkirmisherDreadnaught
      `shouldBe` Just (toCardCode Objectives.corsairSkirmisherShipDreadnaught)
    cdOtherSide Objectives.corsairSkirmisherShipDreadnaught
      `shouldBe` Just (toCardCode Enemies.corsairSkirmisherDreadnaught)
    cdCardType (toCardDef Objectives.corsairSkirmisherShipDreadnaught) `shouldBe` ObjectiveAllyType

  it "adds gained Pillage to the printed amount and blanking removes the printed amount" . gameTest $ \_ -> do
    eid <- staged Enemies.corsairPlunderer
    getPillage eid `shouldReturn` 2
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [GainKeyword (Pillage 3)] UntilEndOfRound
    getPillage eid `shouldReturn` 5
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    getPillage eid `shouldReturn` 3

  it "Pillage discards only available deck cards without recycling the player's discard" . gameTest $ \pid -> do
    eid <- staged Enemies.corsairPlunderer
    card <- PlayerCard <$> genPlayerCard Events.sneakAttack (Just pid)
    g <- getGame
    putGame $ g & playersL . ix pid . deckL .~ [card]
    run $ ResolvePillage eid pid
    player <- getPlayer pid
    player.deck `shouldBe` []
    player.discard `shouldSatisfy` elem card
    getQuestion pid `shouldReturn` Nothing

  it "Massive grants engagement and staging restrictions while the keyword is active" . gameTest $ \_ -> do
    eid <- staged Enemies.brashCorsairDreadnaught
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [GainKeyword Massive] UntilEndOfRound
    ms <- getModifiers eid
    ms `shouldSatisfy` elem CannotLeaveStagingArea
    ms `shouldSatisfy` any (\case EngagedWithEachPlayer _ -> True; _ -> False)

  it "finishes engagement checks with the Massive Dreadnaught still in staging" . gameTest $ \pid -> do
    eid <- staged Enemies.dreadnaught
    run $ EngagementCheck pid 0
    enemies <- getsGame (toList . (.entities.enemies))
    map (\e -> (toId e, e.placement)) enemies `shouldBe` [(eid, StagingArea)]
    peekQueue `shouldReturn` []

  it "engages ordinary enemies while leaving the Dreadnaught in staging" . gameTest $ \pid -> do
    boss <- staged Enemies.dreadnaught
    spider <- staged Enemies.forestSpider
    run $ RaiseThreat pid 30
    run $ EngagementCheck pid 0
    enemies <- getsGame (toList . (.entities.enemies))
    sort (map (\e -> (toId e, e.placement)) enemies)
      `shouldBe` sort [(boss, StagingArea), (spider, EngagedWith pid)]
    peekQueue `shouldReturn` []

  it "does not queue a single-enemy engagement check for the Massive Dreadnaught" . gameTest $ \_ -> do
    eid <- staged Enemies.dreadnaught
    runOnly $ EnemyEngagementChecks eid
    peekQueue `shouldReturn` []

  it "allows engagement again after a gained Massive keyword expires" . gameTest $ \pid -> do
    eid <- staged Enemies.forestSpider
    run $ RaiseThreat pid 30
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [GainKeyword Massive] UntilEndOfRound
    run $ EngagementCheck pid 0
    enemies <- getsGame (toList . (.entities.enemies))
    map (.placement) enemies `shouldBe` [StagingArea]
    run $ EndEffects UntilEndOfRound
    run $ EngagementCheck pid 0
    changed <- getsGame (toList . (.entities.enemies))
    map (.placement) changed `shouldBe` [EngagedWith pid]

  it "immediately recycles a Dreadnaught Corsair discarded while its deck is empty" . gameTest $ \_ -> do
    card <- genEncounterCard Enemies.corsairPlunderer
    g <- getGame
    putGame
      $ g
      & scenarioL
      %~ overAttrs ((Scenario.idL .~ "the-hunt-for-the-dreadnaught") . (corsairDeckL ?~ []))
    run $ AddToEncounterDiscard [card]
    attrs <- getsGame (toAttrs . (.scenario))
    fmap (map toCardCode) attrs.corsairDeck `shouldBe` Just [toCardCode card]
    attrs.corsairDiscard `shouldBe` []
    getEncounterDiscard `shouldReturn` []

  it "cancelling an attack also cancels its pending Pillage" . gameTest $ \pid -> do
    eid <- staged Enemies.corsairPlunderer
    originalDeck <- (.deck) <$> getPlayer pid
    pushAll [ResolvePillage eid pid, CheckWindows [mkAfter $ EnemyAttacking eid pid]]
    runQueueT $ Lifted.cancelAttack eid
    void mainLoop
    (.deck) <$> getPlayer pid `shouldReturn` originalDeck

  it "Fierce Marauder discards a resource for each event its own Pillage discards" . gameTest $ \pid -> do
    eid <- staged Enemies.fierceMarauder
    hero <- toId <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource (toTarget hero) Resource 2
    event <- PlayerCard <$> genPlayerCard Events.sneakAttack (Just pid)
    g <- getGame
    putGame $ g & playersL . ix pid . deckL .~ [event]
    run $ ResolvePillage eid pid
    getHeroResources hero `shouldReturn` 1

  it "resolves Pillage after an actual defended enemy attack" . gameTest $ \pid -> do
    eid <- staged Enemies.corsairPlunderer
    top <- replicateM 2 $ PlayerCard <$> genPlayerCard Allies.guardOfTheCitadel (Just pid)
    g <- getGame
    putGame $ g & playersL . ix pid . deckL .~ top
    defender <- toTarget <$> getHeroWithCode "01012"
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    (.deck) <$> getPlayer pid `shouldReturn` []
    discarded <- (.discard) <$> getPlayer pid
    discarded `shouldSatisfy` (\cards -> all (`elem` cards) top)
