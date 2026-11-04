module LordOfTheRings.DreamChaser.TheCityOfCorsairsSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (TheCityOfCorsairs))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Heading (Heading (..))
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getLocationThreat)
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (GainKeyword))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theCityOfCorsairs)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), corsairDeckL, corsairDiscardL, encounterDeckL, encounterDiscardL, headingL, questDeckL, setAsideL)
import LordOfTheRings.Sphere (Sphere (Neutral))
import LordOfTheRings.Trait (Trait (Ship))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

corsairsTest :: (PlayerId -> GameT ()) -> IO ()
corsairsTest = gameTestIn theCityOfCorsairs testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

withScenario :: (ScenarioAttrs -> ScenarioAttrs) -> GameT ()
withScenario f = do
  g <- (scenarioL %~ overAttrs f) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

genCards :: [CardDef] -> GameT [EncounterCard]
genCards = traverse genEncounterCard

-- | The stage in play on its B side.
flippedStage :: CardDef -> GameT Target
flippedStage def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  pure q

-- | The stage put into play and revealed, its When Revealed resolved.
revealedStage :: PlayerId -> CardDef -> GameT ()
revealedStage pid def = do
  q <- putQuestStage def
  run $ Flip q
  resolveWhenRevealed pid

questsInPlay :: GameT [CardCode]
questsInPlay = getsGame (map toCardCode . toList . (.entities.quests))

enemies :: GameT [EnemyAttrs]
enemies = getsGame (map toAttrs . toList . (.entities.enemies))

enemyOf :: CardDef -> GameT EnemyAttrs
enemyOf def = fromJustNote "no such enemy" . find ((== toCardCode def) . enemyCardCode) <$> enemies

resourcesOn :: CardDef -> GameT Int
resourcesOn def = countTokens Resource . enemyTokens <$> enemyOf def

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyId <$> enemyOf def

codes :: [EncounterCard] -> [CardCode]
codes = sort . map toCardCode

spec :: Spec
spec = describe "The City of Corsairs" do
  it "is a scenario the game can be dealt" . corsairsTest $ \_ ->
    isJust (lookupScenario "the-city-of-corsairs") `shouldBe` True

  it "sets The City of Corsairs aside as the second encounter deck, prepares the Corsair deck and the fleet, and stages the Stormcaller and Southern Belfalas" . corsairsTest $ \pid -> do
    flipFirstQuest pid
    chooseOptionMatching "choose a ship" pid (const True)
    ships <- getsGame (map toCardCode . toList . (.entities.allies))
    ships `shouldSatisfy` elem (toCardCode Objectives.dreamChaser)
    s <- scenarioAttrs
    s.heading `shouldBe` Just Sunny
    map enemyCardCode <$> enemies `shouldReturn` [toCardCode Enemies.stormcallerTheCityOfCorsairs]
    getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.southernBelfalas]
    length s.scenarioSetAside `shouldBe` 17
    s.scenarioSetAside `shouldSatisfy` all ((== Just TheCityOfCorsairs) . cdEncounterSet . toCardDef)
    -- Umbar Slaver and the Corsair Raiders
    let corsairs = fromMaybe [] s.corsairDeck
    length corsairs `shouldBe` 10
    corsairs `shouldSatisfy` all (\c -> Ship `notMember` cdCardTraits (toCardDef c))
    s.encounterDeck `shouldSatisfy` all (\c -> cdCardType (toCardDef c) /= EnemyType || Ship `member` cdCardTraits (toCardDef c))
    map toCardCode s.questDeck `shouldBe` map toCardCode [Quests.fateOfTheDreamChaser, Quests.theHavenOfUmbar, Quests.finalConfrontation]

  describe "Umbar Slaver" do
    it "takes 1 resource for each payment the engaged player makes, however many heroes pay it" . corsairsTest $ \pid -> do
      slaver <- staged Enemies.umbarSlaver
      run $ PlaceEnemy slaver (EngagedWith pid)
      heroes <- getsGame (map toTarget . toList . (.entities.heroes))
      for_ heroes \h -> run $ PlaceTokens GameSource h Resource 2
      run $ SpendResourcesFromDistinctHeroes pid Neutral 2 []
      for_ (take 2 heroes) $ chooseTarget pid
      resourcesOn Enemies.umbarSlaver `shouldReturn` 1
      -- only the third hero has 2 left, and pays unasked
      run $ SpendResourcesFromOneHero pid Neutral 2
      resourcesOn Enemies.umbarSlaver `shouldReturn` 2

    it "takes 1 resource for a card's cost, paid as one payment" . corsairsTest $ \pid -> do
      slaver <- staged Enemies.umbarSlaver
      run $ PlaceEnemy slaver (EngagedWith pid)
      _ <- playWith pid "01007" 2 Allies.wanderingTook
      map toCardCode . toList <$> getsGame (.entities.allies) `shouldReturn` [toCardCode Allies.wanderingTook]
      resourcesOn Enemies.umbarSlaver `shouldReturn` 1

  describe "The Coast of Umbar" do
    it "takes no progress while off-course" . corsairsTest $ \_ -> do
      q <- flippedStage Quests.theCoastOfUmbar
      withScenario (headingL ?~ Cloudy)
      run $ PlaceTokens GameSource q Progress 5
      (.progress) <$> getActiveQuest `shouldReturn` 0
      withScenario (headingL ?~ Sunny)
      run $ PlaceTokens GameSource q Progress 5
      (.progress) <$> getActiveQuest `shouldReturn` 5

    it "advances once the Stormcaller is destroyed, and remembers it" . corsairsTest $ \_ -> do
      _ <- flippedStage Quests.theCoastOfUmbar
      laterStages <- genCards [Quests.fateOfTheDreamChaser, Quests.theHavenOfUmbar]
      withScenario (questDeckL .~ laterStages)
      stormcaller <- staged Enemies.stormcallerTheCityOfCorsairs
      run $ Destroy GameSource (EnemyTarget stormcaller)
      questsInPlay `shouldReturn` [toCardCode Quests.fateOfTheDreamChaser]
      (.meta) <$> scenarioAttrs `shouldReturn` toJSON True

  describe "Fate of the Dream-chaser" do
    it "discards the Ship enemies, Ship objectives and Ocean locations, and makes the second encounter deck the active one" . corsairsTest $ \pid -> do
      run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.dreamChaser
      _ <- staged Enemies.slaveShip
      _ <- staged Enemies.umbarSlaver
      run . AddToStaging =<< genEncounterCard Locations.corsairWaters
      deck <- genCards [Treacheries.battleHardened, Locations.starlitSea]
      discarded <- genCards [Locations.watchTower, Treacheries.windsOfWrath]
      corsairs <- genCards [Enemies.umbarRaider]
      corsairDiscard <- genCards [Enemies.raidLeader]
      secondDeck <- genCards [Locations.streetsOfUmbar, Enemies.captainSahirTheCityOfCorsairs]
      laterStages <- genCards [Quests.theHavenOfUmbar]
      withScenario
        $ (encounterDeckL .~ deck)
        . (encounterDiscardL .~ discarded)
        . (corsairDeckL ?~ corsairs)
        . (corsairDiscardL .~ corsairDiscard)
        . (setAsideL .~ secondDeck)
        . (questDeckL .~ laterStages)
      revealedStage pid Quests.fateOfTheDreamChaser
      getsGame (length . (.entities.allies)) `shouldReturn` 0
      map enemyCardCode <$> enemies `shouldReturn` [toCardCode Enemies.umbarSlaver]
      getsGame (length . (.entities.locations)) `shouldReturn` 0
      s <- scenarioAttrs
      codes s.encounterDeck
        `shouldBe` sort (map toCardCode [Locations.streetsOfUmbar, Enemies.captainSahirTheCityOfCorsairs, Enemies.umbarRaider, Enemies.raidLeader, Treacheries.battleHardened, Locations.watchTower])
      s.encounterDiscard `shouldBe` []
      s.corsairDeck `shouldBe` Nothing
      s.scenarioSetAside `shouldBe` []
      questsInPlay `shouldReturn` [toCardCode Quests.theHavenOfUmbar]

    it "has each player assign the damage on his Ship objectives among his other characters" . corsairsTest $ \pid -> do
      run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.dreamChaser
      ship <- getsGame (toTarget . fromJustNote "no ship" . headMay . toList . (.entities.allies))
      run $ PlaceTokens GameSource ship Damage 3
      revealedStage pid Quests.fateOfTheDreamChaser
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> "$label.dealDamageDivided" `isPrefixOf` l; _ -> False)

  describe "The Haven of Umbar" do
    it "sets aside The Shattered Monument and Captain Sahír, stages an enemy and a location, and gives each Raider a resource if the Stormcaller lives" . corsairsTest $ \pid -> do
      _ <- setEncounterDeck [Locations.theShatteredMonument, Enemies.captainSahirTheCityOfCorsairs, Enemies.umbarSlaver, Locations.streetsOfUmbar]
      revealedStage pid Quests.theHavenOfUmbar
      codes . scenarioSetAside <$> scenarioAttrs `shouldReturn` sort (map toCardCode [Locations.theShatteredMonument, Enemies.captainSahirTheCityOfCorsairs])
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.streetsOfUmbar]
      resourcesOn Enemies.umbarSlaver `shouldReturn` 1

    it "gives the Raiders nothing if the Stormcaller was destroyed during stage 1" . corsairsTest $ \pid -> do
      withScenario (\s -> s {scenarioMeta = toJSON True})
      _ <- setEncounterDeck [Enemies.umbarSlaver]
      revealedStage pid Quests.theHavenOfUmbar
      resourcesOn Enemies.umbarSlaver `shouldReturn` 0

  describe "Final Confrontation" do
    it "puts Captain Sahír into play engaged with the first player, makes him indestructible while another enemy has resources, moves the resources of the destroyed to him, and is won when he is destroyed" . corsairsTest $ \pid -> do
      setAside' <- genCards [Locations.theShatteredMonument, Enemies.captainSahirTheCityOfCorsairs]
      withScenario (setAsideL .~ setAside')
      _ <- setEncounterDeck [Enemies.umbarSlaver]
      revealedStage pid Quests.finalConfrontation
      chooseOptionMatching "find an enemy" pid (const True)
      getsGame (map toCardCode . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.theShatteredMonument]
      sahir <- enemyOf Enemies.captainSahirTheCityOfCorsairs
      sahir.enemyPlacement `shouldBe` EngagedWith pid
      slaver <- enemyOf Enemies.umbarSlaver
      slaver.enemyPlacement `shouldBe` StagingArea
      resourcesOn Enemies.umbarSlaver `shouldReturn` 1
      resourcesOn Enemies.captainSahirTheCityOfCorsairs `shouldReturn` 1
      getModifiers (EnemyTarget sahir.enemyId) >>= (`shouldSatisfy` elem (GainKeyword Indestructible))
      run $ Destroy GameSource (EnemyTarget slaver.enemyId)
      resourcesOn Enemies.captainSahirTheCityOfCorsairs `shouldReturn` 2
      getModifiers (EnemyTarget sahir.enemyId) >>= (`shouldSatisfy` notElem (GainKeyword Indestructible))
      run $ Destroy GameSource (EnemyTarget sahir.enemyId)
      getsGame (.outcome) `shouldReturn` Just Won

  describe "Encounter cards" do
    it "Watch Tower, in the staging area, adds a resource to each Raider enemy entering play" . corsairsTest $ \_ -> do
      run . AddToStaging =<< genEncounterCard Locations.watchTower
      _ <- staged Enemies.umbarSlaver
      resourcesOn Enemies.umbarSlaver `shouldReturn` 1

    it "City of Corsairs' threat is the most resources on a Raider enemy, and Umbar Slaver's threat grows with them" . corsairsTest $ \_ -> do
      run . AddToStaging =<< genEncounterCard Locations.cityOfCorsairs
      city <- getsGame (fromJustNote "no city" . fmap toId . headMay . toList . (.entities.locations))
      slaver <- staged Enemies.umbarSlaver
      run $ PlaceTokens GameSource (EnemyTarget slaver) Resource 3
      getLocationThreat city `shouldReturn` 3
      getEnemyThreat slaver `shouldReturn` 5
