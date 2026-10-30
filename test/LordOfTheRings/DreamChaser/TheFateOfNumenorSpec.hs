module LordOfTheRings.DreamChaser.TheFateOfNumenorSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardType (LocationType), cdCardType, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (CannotDrawCards, CannotSearchDeck))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theFateOfNumenor)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), setAsideL, unchartedDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

fateTest :: (PlayerId -> GameT ()) -> IO ()
fateTest = gameTestIn theFateOfNumenor testDeck

locations :: GameT [LocationAttrs]
locations = getsGame (map toAttrs . toList . (.entities.locations))

lostIslandsInStaging :: GameT [LocationAttrs]
lostIslandsInStaging = filter (\l -> l.placement == StagingArea && locationCardCode l == toCardCode Locations.lostIsland) <$> locations

unchartedDeck :: GameT [CardCode]
unchartedDeck = getsGame (map toCardCode . scenarioUnchartedDeck . toAttrs . (.scenario))

setUnchartedDeck :: [CardDef] -> GameT ()
setUnchartedDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (unchartedDeckL .~ cards)

-- | The location card added to the staging area, Lost Island side faceup, and its id.
staged :: CardDef -> GameT LocationId
staged def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such location" . fmap toId . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

-- | The player answers the search Shrine to Morgoth makes him do with its first choice.
answerSearch :: PlayerId -> GameT ()
answerSearch pid = chooseOptionMatching "answer the search" pid (const True)

-- | Make the Uncharted location active: it flips over as it does.
travelTo :: LocationId -> GameT ()
travelTo lid = run $ PlaceLocation lid ActiveLocation

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

progressOn :: Target -> GameT Int
progressOn t = getsGame (maybe 0 (.progress) . find ((== t) . toTarget) . toList . (.entities.quests))

-- | Explore the Island in play on its B side, its setup done with the given Uncharted deck.
exploring :: PlayerId -> [CardDef] -> GameT Target
exploring pid deck = do
  setUnchartedDeck deck
  q <- putQuestStage Quests.exploreTheIsland
  run $ Flip q
  passActions pid
  pure q

spec :: Spec
spec = describe "The Fate of Númenor" do
  it "is a scenario the game can be dealt" . fateTest $ \_ ->
    isJust (lookupScenario "the-fate-of-numenor") `shouldBe` True

  it "prepares the Uncharted deck, sets Shrine to Morgoth aside, adds 2 Lost Islands and gives Calphon to the first player" . fateTest $ \pid -> do
    flipFirstQuest pid
    length <$> lostIslandsInStaging `shouldReturn` 2
    -- 2 Lush Jungle, 2 Forbidden Coast, 3 Steep Plateau, 2 each of Ruins of Numenor's three
    length <$> unchartedDeck `shouldReturn` 11
    deck <- getEncounterDeck
    filter ((== LocationType) . cdCardType . toCardDef) deck `shouldBe` []
    getsGame (map toCardCode . (.scenarioSetAside) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.shrineToMorgoth]
    calphon <- getsGame (find ((== toCardCode Objectives.calphon) . toCardCode) . toList . (.entities.allies))
    (allyPlacement . toAttrs <$> calphon) `shouldBe` Just (InPlayArea pid)

  -- \* Explore the Island

  it "takes no progress but from its own effects" . fateTest $ \pid -> do
    q <- exploring pid []
    run $ PlaceTokens GameSource q Progress 5
    progressOn q `shouldReturn` 0

  it "takes progress equal to an explored location's printed quest points" . fateTest $ \pid -> do
    q <- exploring pid [Locations.cursedTemple, Locations.cursedTemple, Locations.floodedRuins]
    lid <- fromJustNote "no Lost Island" . headMay . map locationId <$> lostIslandsInStaging
    travelTo lid
    l <- getsGame (fmap toAttrs . find ((== lid) . toId) . toList . (.entities.locations))
    (locationCardCode <$> l) `shouldBe` Just (toCardCode Locations.cursedTemple)
    run $ PlaceTokens GameSource (toTarget lid) Progress 3
    progressOn q `shouldReturn` 3
    -- back into the Uncharted deck, as an explored Uncharted location goes
    unchartedDeck `shouldReturn` [toCardCode Locations.cursedTemple]

  it "adds a Lost Island from the Uncharted deck as an Uncharted location becomes active" . fateTest $ \pid -> do
    _ <- exploring pid [Locations.cursedTemple, Locations.cursedTemple, Locations.floodedRuins]
    lid <- fromJustNote "no Lost Island" . headMay . map locationId <$> lostIslandsInStaging
    travelTo lid
    length <$> lostIslandsInStaging `shouldReturn` 2
    unchartedDeck `shouldReturn` []

  -- \* A Fateful Discovery

  it "adds 2 Lost Islands and Shrine to Morgoth, Lost Island side faceup, and shuffles the Uncharted locations as it is revealed" . fateTest $ \pid -> do
    setUnchartedDeck [Locations.cursedTemple, Locations.floodedRuins]
    shrine <- genEncounterCard Locations.shrineToMorgoth
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [shrine])
    island <- staged Locations.drownedGraves
    run $ PlaceTokens GameSource (toTarget island) Progress 2
    q <- putQuestStage Quests.aFatefulDiscovery
    run $ Flip q
    resolveWhenRevealed pid
    ls <- lostIslandsInStaging
    length ls `shouldBe` 4
    sort (mapMaybe (.otherSide) ls)
      `shouldBe` sort (map toCardCode [Locations.cursedTemple, Locations.floodedRuins, Locations.shrineToMorgoth, Locations.drownedGraves])
    map (.progress) ls `shouldBe` [0, 0, 0, 0]

  it "adds an explored Uncharted location to the victory display rather than the Uncharted deck" . fateTest $ \pid -> do
    q <- putQuestStage Quests.aFatefulDiscovery
    run $ Flip q
    resolveWhenRevealed pid
    lid <- staged Locations.cursedTemple
    travelTo lid
    run $ PlaceTokens GameSource (toTarget lid) Progress 3
    victoryDisplay `shouldReturn` [toCardCode Locations.cursedTemple]
    unchartedDeck `shouldReturn` []

  it "wins the game as Shrine to Morgoth is added to the victory display" . fateTest $ \pid -> do
    q <- putQuestStage Quests.aFatefulDiscovery
    run $ Flip q
    resolveWhenRevealed pid
    lid <- staged Locations.shrineToMorgoth
    _ <- setEncounterDeck []
    travelTo lid
    answerSearch pid
    run $ PlaceTokens GameSource (toTarget lid) Progress 8
    getsGame (.outcome) `shouldReturn` Just Won

  -- \* Its cards

  it "loses the game as Calphon leaves play" . fateTest $ \pid -> do
    flipFirstQuest pid
    calphon <- getsGame (fromJustNote "no Calphon" . find ((== toCardCode Objectives.calphon) . toCardCode) . toList . (.entities.allies))
    run $ Discard (toTarget calphon)
    getsGame (.outcome) `shouldReturn` Just Lost

  it "keeps the players from drawing or searching their decks while Shrine to Morgoth is active, and engages each with an Undead enemy as it flips" . fateTest $ \pid -> do
    _ <- setEncounterDeck [Enemies.drownedDead]
    lid <- staged Locations.shrineToMorgoth
    travelTo lid
    answerSearch pid
    mods <- getModifiers (PlayerTarget pid)
    (CannotDrawCards `elem` mods, CannotSearchDeck `elem` mods) `shouldBe` (True, True)
    engaged <- getsGame (filter (\e -> e.placement == EngagedWith pid) . toList . (.entities.enemies))
    map toCardCode engaged `shouldBe` [toCardCode Enemies.drownedDead]

  it "adds a Lost Island and shuffles them as Ruins of Ages Past is revealed" . fateTest $ \_ -> do
    setUnchartedDeck [Locations.floodedRuins]
    island <- staged Locations.cursedTemple
    run $ PlaceTokens GameSource (toTarget island) Progress 1
    _ <- setEncounterDeck [Treacheries.ruinsOfAgesPast]
    run RevealEncounterCard
    resolveRevealed
    ls <- lostIslandsInStaging
    length ls `shouldBe` 2
    map (.progress) ls `shouldBe` [0, 0]

  it "makes Flooded Ruins add its threat to the staging area while it is active" . fateTest $ \_ -> do
    lid <- staged Locations.floodedRuins
    getStagingThreat `shouldReturn` 2
    travelTo lid
    getStagingThreat `shouldReturn` 3

  it "keeps the deck of the player engaged with Soulless Cadaver from being searched" . fateTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Enemies.soullessCadaver
    eid <- getsGame (fromJustNote "no cadaver" . fmap toId . find ((== toCardCode Enemies.soullessCadaver) . toCardCode) . toList . (.entities.enemies))
    run $ EngageEnemy pid eid
    elem CannotSearchDeck <$> getModifiers (PlayerTarget pid) `shouldReturn` True

  it "returns Lush Jungle to the staging area as it flips, unless each player exhausts each ready hero" . fateTest $ \pid -> do
    lid <- staged Locations.lushJungle
    travelTo lid
    chooseLabel "$label.lushJungle.return" pid
    l <- getsGame (fmap toAttrs . find ((== lid) . toId) . toList . (.entities.locations))
    ((.placement) <$> l) `shouldBe` Just StagingArea
    (locationCardCode <$> l) `shouldBe` Just (toCardCode Locations.lushJungle)

  it "places each ally Curse of the Downfallen destroys on the bottom of its owner's deck" . fateTest $ \pid -> do
    scout <- putAllyIntoPlay pid Allies.snowbournScout
    cardId <- getsGame (fmap (allyCardId . toAttrs) . find ((== scout) . toTarget) . toList . (.entities.allies))
    _ <- setEncounterDeck [Treacheries.curseOfTheDownfallen]
    run RevealEncounterCard
    resolveRevealed
    bottom <- lastMay . (.deck) <$> getPlayer pid
    ((.id) <$> bottom) `shouldBe` cardId

  it "discards the events of the player Na'asiyah engages, a resource on her for each" . fateTest $ \pid -> do
    _ <- putCardIntoHand pid Events.aTestOfWill
    _ <- putCardIntoHand pid Events.sneakAttack
    run . AddToStaging =<< genEncounterCard Enemies.naasiyah
    eid <- getsGame (fromJustNote "no Na'asiyah" . fmap toId . find ((== toCardCode Enemies.naasiyah) . toCardCode) . toList . (.entities.enemies))
    run $ EngageEnemy pid eid
    hand <- (.hand) <$> getPlayer pid
    filter ((`elem` map toCardCode [Events.aTestOfWill, Events.sneakAttack]) . toCardCode) hand `shouldBe` []
    getsGame (fmap (countTokens Resource . enemyTokens . toAttrs) . find ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` Just 2
