module LordOfTheRings.DreamChaser.SailingEngineSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (EnemyAttrs (enemyTokens))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Heading (Heading (..), HeadingShift (..))
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Keyword (Keyword (Boarding))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getBoarding)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Card (CardType (AllyType))
import LordOfTheRings.Matcher.Types (CardMatcher (CardWithType))
import LordOfTheRings.Phase (Phase (QuestPhase, ResourcePhase))
import LordOfTheRings.Window (WindowType (PhaseBegins), mkAfter)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (voyageAcrossBelegaer)
import LordOfTheRings.Scenario.Types (corsairDeckL, headingL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

sailingTest :: (PlayerId -> GameT ()) -> IO ()
sailingTest = gameTestIn voyageAcrossBelegaer testDeck

-- | The heading set as given, the modifiers read again from it.
setHeading :: Heading -> GameT ()
setHeading h = do
  g <- (scenarioL %~ overAttrs (headingL ?~ h)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

heading :: GameT (Maybe Heading)
heading = getsGame ((.heading) . toAttrs . (.scenario))

corsairDeck :: GameT (Maybe [CardCode])
corsairDeck = getsGame (fmap (map toCardCode) . (.corsairDeck) . toAttrs . (.scenario))

corsairDiscard :: GameT [CardCode]
corsairDiscard = getsGame (map toCardCode . (.corsairDiscard) . toAttrs . (.scenario))

setCorsairDeck :: [CardDef] -> GameT ()
setCorsairDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (corsairDeckL ?~ cards)

-- | A ship of the fleet put into play under the player.
ship :: PlayerId -> CardDef -> GameT Target
ship pid def = do
  card <- genEncounterCard def
  run $ PutObjectiveAllyIntoPlayUnder pid card
  allyOf def

allyOf :: CardDef -> GameT Target
allyOf def = getsGame (toTarget . fromJustNote "no such ally" . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemiesEngagedWith :: PlayerId -> GameT [CardCode]
enemiesEngagedWith pid = getsGame (map toCardCode . filter ((== EngagedWith pid) . (.placement)) . toList . (.entities.enemies))

beravor :: GameT Target
beravor = toTarget <$> getHeroWithCode "01012"

-- | Commit the characters to the Sailing test, then look at what it finds.
commitAndLook :: PlayerId -> [Target] -> GameT ()
commitAndLook pid cs = do
  for_ cs (chooseTarget pid)
  chooseLabel "$label.sailingTest.done" pid
  chooseLabel "$label.continue" pid

spec :: Spec
spec = describe "The Dream-chaser engine" do
  describe "The heading" do
    it "shifts one setting at a time, never past on-course nor the worst setting" . sailingTest $ \_ -> do
      setHeading Sunny
      run $ ShiftHeading GameSource ShiftOnCourse
      heading `shouldReturn` Just Sunny
      replicateM_ 4 $ run $ ShiftHeading GameSource ShiftOffCourse
      heading `shouldReturn` Just Stormy
      run $ ShiftHeading GameSource ShiftOnCourse
      heading `shouldReturn` Just Rainy

  describe "The Sailing test" do
    it "comes before characters commit, while the stage has the Sailing keyword" . sailingTest $ \pid -> do
      _ <- putQuestStage Quests.departureFromTheHavens
      setHeading Sunny
      run $ BeginPhase QuestPhase
      passActions pid
      heading `shouldReturn` Just Cloudy
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> "$label.sailingTest.commit" `isPrefixOf` l; _ -> False)

    it "shifts off-course, then on-course for each success icon among the cards the characters look at" . sailingTest $ \pid -> do
      setHeading Rainy
      _ <- setEncounterDeck [Locations.calmWaters, Locations.rollingSeas, Enemies.scoutingShip]
      b <- beravor
      run SailingTest
      heading `shouldReturn` Just Stormy
      commitAndLook pid [b]
      -- Calm Waters has 2 icons: the only card looked at, for the one character
      heading `shouldReturn` Just Cloudy
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.calmWaters]
      ((.heroExhausted) . toAttrs <$> getHeroWithCode "01012") `shouldReturn` True

    it "looks at 2 more cards while Calm Waters is active, and Rough Waters in the staging area cancels the first icon" . sailingTest $ \pid -> do
      setHeading Rainy
      calm <- genEncounterCard Locations.calmWaters
      run $ PutIntoPlayAsActiveLocation calm
      run . AddToStaging =<< genEncounterCard Locations.roughWaters
      _ <- setEncounterDeck [Enemies.scoutingShip, Locations.rollingSeas, Treacheries.thrownOffCourse, Locations.fogBank]
      b <- beravor
      run SailingTest
      commitAndLook pid [b]
      -- 3 cards, 2 icons, 1 canceled: Stormy back to Rainy
      heading `shouldReturn` Just Rainy
      length <$> getEncounterDiscard `shouldReturn` 3

    it "lets the Dream-chaser commit whoever controls it, counting as 2 characters" . sailingTest $ \pid -> do
      setHeading Sunny
      dreamChaser <- ship pid Objectives.dreamChaser
      getModifiers dreamChaser >>= (`shouldSatisfy` \ms -> CountsAsSailingCharacters 2 `elem` ms && ImmuneToPlayerCardEffects `elem` ms)
      _ <- setEncounterDeck [Enemies.scoutingShip, Locations.fogBank, Locations.rollingSeas]
      run SailingTest
      commitAndLook pid [dreamChaser]
      length <$> getEncounterDiscard `shouldReturn` 2

  describe "The Corsair deck" do
    it "takes each non-Ship enemy out of the encounter deck" . sailingTest $ \_ -> do
      _ <- setEncounterDeck [Enemies.scoutingShip, Enemies.southronSailor, Locations.fogBank, Enemies.umbarCaptain]
      run PrepareCorsairDeck
      map toCardCode <$> getEncounterDeck `shouldReturn` map toCardCode [Enemies.scoutingShip, Locations.fogBank]
      fmap sort <$> corsairDeck `shouldReturn` Just (sort $ map toCardCode [Enemies.southronSailor, Enemies.umbarCaptain])

    it "reveals its top card engaged with the player, discards to its own pile, and reshuffles it once empty" . sailingTest $ \pid -> do
      setCorsairDeck [Enemies.southronSailor]
      run $ RevealFromCorsairDeck GameSource pid
      enemiesEngagedWith pid `shouldReturn` [toCardCode Enemies.southronSailor]
      sailor <- getsGame (fromJustNote "no sailor" . fmap toTarget . headMay . toList . (.entities.enemies))
      run $ Discard sailor
      corsairDiscard `shouldReturn` [toCardCode Enemies.southronSailor]
      getEncounterDiscard `shouldReturn` []
      run $ RevealFromCorsairDeck GameSource pid
      enemiesEngagedWith pid `shouldReturn` [toCardCode Enemies.southronSailor]
      corsairDiscard `shouldReturn` []

  describe "Boarding" do
    it "reveals X Corsair cards engaged with the player as a Ship enemy engages from the staging area" . sailingTest $ \pid -> do
      setCorsairDeck [Enemies.umbarRaider, Enemies.southronSailor]
      scout <- staged Enemies.scoutingShip
      run $ EngageEnemy pid scout
      sort <$> enemiesEngagedWith pid `shouldReturn` sort (map toCardCode [Enemies.scoutingShip, Enemies.umbarRaider])
      corsairDeck `shouldReturn` Just [toCardCode Enemies.southronSailor]

    it "does nothing for a Ship enemy put into play engaged" . sailingTest $ \pid -> do
      setCorsairDeck [Enemies.umbarRaider]
      run . RevealCardEngaged pid =<< genEncounterCard Enemies.scoutingShip
      enemiesEngagedWith pid `shouldReturn` [toCardCode Enemies.scoutingShip]

    it "adds what is gained to the printed value, then its modifiers" . sailingTest $ \_ -> do
      cruiser <- staged Enemies.lightCruiser
      getBoarding cruiser `shouldReturn` 2
      run $ CreateEffect $ Effect GameSource (EnemyTarget cruiser) [GainKeyword (Boarding 1), BoardingModifier (-1)] UntilEndOfRound
      getBoarding cruiser `shouldReturn` 2

    it "has Light Cruiser's Raider boarders enter play with a resource" . sailingTest $ \pid -> do
      setCorsairDeck [Enemies.umbarRaider, Enemies.southronSailor]
      cruiser <- staged Enemies.lightCruiser
      run $ EngageEnemy pid cruiser
      resources <- getsGame (map (\e -> (toCardCode e, countTokens Resource (enemyTokens $ toAttrs e))) . toList . (.entities.enemies))
      sort resources `shouldBe` sort [(toCardCode Enemies.lightCruiser, 0), (toCardCode Enemies.umbarRaider, 1), (toCardCode Enemies.southronSailor, 0)]

  describe "Ship combat" do
    it "lets only Ship objectives defend against Ship enemies, and Ship objectives fight only Ship enemies" . sailingTest $ \pid -> do
      dreamChaser <- ship pid Objectives.dreamChaser
      b <- beravor
      scout <- staged Enemies.scoutingShip
      sailor <- staged Enemies.southronSailor
      canDefend scout dreamChaser `shouldReturn` True
      canDefend scout b `shouldReturn` False
      canDefend sailor dreamChaser `shouldReturn` False
      canFight sailor dreamChaser `shouldReturn` False
      canFight scout b `shouldReturn` True

    it "deals a Ship enemy's undefended damage to a Ship objective the player controls" . sailingTest $ \pid -> do
      dreamChaser <- ship pid Objectives.dreamChaser
      scout <- staged Enemies.scoutingShip
      run $ PlaceEnemy scout (EngagedWith pid)
      run $ UndefendedDamage pid scout
      -- the Ship objective is the only choice, not a hero
      getQuestion pid >>= (`shouldSatisfy` maybe False ((== 1) . length . questionChoices))
      chooseTarget pid dreamChaser
      getDamage dreamChaser `shouldReturn` 5

    it "bars a player engaged with Umbar Captain from attacking Ship enemies" . sailingTest $ \pid -> do
      scout <- staged Enemies.scoutingShip
      captain <- staged Enemies.umbarCaptain
      run $ PlaceEnemy scout (EngagedWith pid)
      run $ PlaceEnemy captain (EngagedWith pid)
      mayAttack pid scout `shouldReturn` []
      mayAttack pid captain >>= (`shouldSatisfy` notNull)

  describe "Preparing the fleet" do
    it "gives a lone player the Dream-chaser and the ship he chooses, Dawn Star raising his threat by 3" . sailingTest $ \pid -> do
      threat <- (.threat) <$> getPlayer pid
      run PrepareFleet
      dawnStar <- getsGame (fromJustNote "no question" . headMay . mapMaybe (\c -> case c of TargetLabel t _ -> Just t; _ -> Nothing) . maybe [] questionChoices . lookup pid . mapToList . (.question))
      chooseTarget pid dawnStar
      sort <$> getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` sort (map toCardCode [Objectives.dreamChaser, Objectives.dawnStar])
      heading `shouldReturn` Just Sunny
      (.threat) <$> getPlayer pid `shouldReturn` threat + 3

    it "has Nárelenya lower the cost of the first ally each round, and Silver Wing give each hero +1 attack" . sailingTest $ \pid -> do
      _ <- ship pid Objectives.narelenya
      _ <- ship pid Objectives.silverWing
      run $ CheckWindows [mkAfter $ PhaseBegins ResourcePhase]
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (ReduceNextCostOf (CardWithType AllyType) 1))
      (getModifiers =<< beravor) >>= (`shouldSatisfy` elem (AttackModifier 1))

    it "eliminates the player whose Dawn Star is destroyed, the ship leaving the game" . sailingTest $ \pid -> do
      dawnStar <- ship pid Objectives.dawnStar
      run $ DealDamage GameSource dawnStar 16
      (.eliminated) <$> getPlayer pid `shouldReturn` True
      getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Objectives.dawnStar) . map toCardCode)

    it "loses the game as the Dream-chaser is destroyed" . sailingTest $ \pid -> do
      dreamChaser <- ship pid Objectives.dreamChaser
      run $ DealDamage GameSource dreamChaser 18
      getsGame (.outcome) `shouldReturn` Just Lost
