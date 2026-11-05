module LordOfTheRings.DreamChaser.VoyageAcrossBelegaerSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (..), CardType (EnemyType), HasCardCode, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Heading (Heading (..))
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getLocationThreat, getStagingThreat)
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getBoarding)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, voyageAcrossBelegaer)
import LordOfTheRings.Scenario.Types (headingL, questDeckL)
import LordOfTheRings.Trait (Trait (Ship))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import TestImport

voyageTest :: (PlayerId -> GameT ()) -> IO ()
voyageTest = gameTestIn voyageAcrossBelegaer testDeck

setHeading :: Heading -> GameT ()
setHeading h = do
  g <- (scenarioL %~ overAttrs (headingL ?~ h)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

heading :: GameT (Maybe Heading)
heading = getsGame ((.heading) . toAttrs . (.scenario))

-- | The stage in play on its B side, nothing else in the quest deck.
flippedStage :: CardDef -> GameT Target
flippedStage def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  pure q

setQuestDeck :: [CardDef] -> GameT [EncounterCard]
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)
  pure cards

questDeck :: GameT [CardCode]
questDeck = getsGame (map toCardCode . (.questDeck) . toAttrs . (.scenario))

questsInPlay :: GameT [CardCode]
questsInPlay = getsGame (map toCardCode . toList . (.entities.quests))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

codesOf :: (HasField "placement" a Placement, HasCardCode a) => Placement -> [a] -> [CardCode]
codesOf p = map toCardCode . filter ((== p) . (.placement))

spec :: Spec
spec = describe "Voyage Across Belegaer" do
  it "is a scenario the game can be dealt" . voyageTest $ \_ ->
    isJust (lookupScenario "voyage-across-belegaer") `shouldBe` True

  describe "Setup" do
    it "prepares the Corsair deck and the fleet, stages Rolling Seas and Scouting Ship, and shuffles the stage 2 cards above stage 3" . voyageTest $ \pid -> do
      flipFirstQuest pid
      -- a lone player takes the Dream-chaser and chooses another ship
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> l == "$label.fleet.chooseShip"; _ -> False)
      chooseOptionMatching "choose a ship" pid (const True)
      ships <- getsGame (map toCardCode . toList . (.entities.allies))
      ships `shouldSatisfy` elem (toCardCode Objectives.dreamChaser)
      length ships `shouldBe` 2
      heading `shouldReturn` Just Sunny
      sort <$> getsGame (codesOf StagingArea . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.scoutingShip]
      getsGame (codesOf StagingArea . toList . (.entities.locations)) `shouldReturn` [toCardCode Locations.rollingSeas]
      corsairs <- getsGame (fromMaybe [] . (.corsairDeck) . toAttrs . (.scenario))
      length corsairs `shouldBe` 9
      corsairs `shouldSatisfy` all (\c -> Ship `notMember` cdCardTraits (toCardDef c))
      deck <- getEncounterDeck
      deck `shouldSatisfy` all (\c -> cdCardType (toCardDef c) /= EnemyType || Ship `member` cdCardTraits (toCardDef c))
      stages <- questDeck
      lastMay stages `shouldBe` Just (toCardCode Quests.corsairPursuit)
      sort (take 4 stages) `shouldBe` sort (map toCardCode [Quests.intoTheStorm, Quests.cursedMists, Quests.calphonsDivination, Quests.theStarsGuidance])

  describe "Advancing" do
    it "lets the players choose one of the top 2 stages while on-course, the other going to the bottom" . voyageTest $ \pid -> do
      QuestTarget departure <- flippedStage Quests.departureFromTheHavens
      [_, mists, _] <- setQuestDeck [Quests.intoTheStorm, Quests.cursedMists, Quests.corsairPursuit]
      setHeading Sunny
      run $ CompleteQuest departure
      chooseTarget pid mists
      questsInPlay `shouldReturn` [toCardCode Quests.cursedMists]
      questDeck `shouldReturn` map toCardCode [Quests.corsairPursuit, Quests.intoTheStorm]

    it "advances to the top stage while off-course" . voyageTest $ \_ -> do
      QuestTarget departure <- flippedStage Quests.departureFromTheHavens
      _ <- setQuestDeck [Quests.intoTheStorm, Quests.cursedMists, Quests.corsairPursuit]
      setHeading Cloudy
      run $ CompleteQuest departure
      questsInPlay `shouldReturn` [toCardCode Quests.intoTheStorm]
      questDeck `shouldReturn` map toCardCode [Quests.cursedMists, Quests.corsairPursuit]

  describe "The stages" do
    it "Departure from the Havens gives each Ship objective +2 willpower while on-course" . voyageTest $ \pid -> do
      _ <- flippedStage Quests.departureFromTheHavens
      run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.dreamChaser
      dreamChaser <- getsGame (toTarget . fromJustNote "no ship" . headMay . toList . (.entities.allies))
      setHeading Sunny
      getModifiers dreamChaser >>= (`shouldSatisfy` elem (WillpowerModifier 2))
      setHeading Cloudy
      getModifiers dreamChaser >>= (`shouldSatisfy` notElem (WillpowerModifier 2))

    it "Into the Storm has each player distribute 5 damage at the end of the quest phase while off-course" . voyageTest $ \pid -> do
      _ <- flippedStage Quests.intoTheStorm
      setHeading Rainy
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> "$label.dealDamageDivided" `isPrefixOf` l; _ -> False)

    it "Cursed Mists gives each non-Ship ally -2 willpower while off-course" . voyageTest $ \pid -> do
      _ <- flippedStage Quests.cursedMists
      ally <- putAllyIntoPlay pid Allies.gondorianSpearman
      setHeading Rainy
      getModifiers ally >>= (`shouldSatisfy` elem (WillpowerModifier (-2)))

    it "Calphon's Divination gives each Ship enemy Boarding 1 while off-course" . voyageTest $ \_ -> do
      _ <- flippedStage Quests.calphonsDivination
      scout <- staged Enemies.scoutingShip
      setHeading Sunny
      getBoarding scout `shouldReturn` 1
      setHeading Cloudy
      getBoarding scout `shouldReturn` 2

    it "The Star's Guidance gives each Ocean location +1 threat while off-course" . voyageTest $ \_ -> do
      _ <- flippedStage Quests.theStarsGuidance
      fog <- stagedLocation Locations.fogBank
      setHeading Cloudy
      getLocationThreat fog `shouldReturn` 3

    it "Corsair Pursuit takes progress only on-course, and is won with 10" . voyageTest $ \_ -> do
      q <- flippedStage Quests.corsairPursuit
      _ <- staged Enemies.scoutingShip
      setHeading Cloudy
      run $ PlaceTokens GameSource q Progress 6
      getsGame (fmap (.progress) . headMay . toList . (.entities.quests)) `shouldReturn` Just 0
      setHeading Sunny
      run $ PlaceTokens GameSource q Progress 10
      getsGame (.outcome) `shouldReturn` Just Won

    it "Corsair Pursuit is won once no Ship enemy is left in play" . voyageTest $ \_ -> do
      _ <- flippedStage Quests.corsairPursuit
      scout <- staged Enemies.scoutingShip
      run $ Discard GameSource (EnemyTarget scout)
      getsGame (.outcome) `shouldReturn` Just Won

  describe "Encounter cards" do
    it "Scouting Ship gets -15 engagement cost while off-course" . voyageTest $ \_ -> do
      scout <- staged Enemies.scoutingShip
      setHeading Sunny
      getEngagementCost scout `shouldReturn` 35
      setHeading Stormy
      getEngagementCost scout `shouldReturn` 20

    it "Fog Bank, active, keeps Ship enemies from engaging, attacking or being attacked" . voyageTest $ \_ -> do
      scout <- staged Enemies.scoutingShip
      fog <- stagedLocation Locations.fogBank
      run $ PlaceLocation fog ActiveLocation
      getModifiers (EnemyTarget scout) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [NoEngagementChecks, CannotAttack, CannotBeAttacked])

    it "Rolling Seas shifts the heading off-course as the players travel there" . voyageTest $ \_ -> do
      setHeading Sunny
      seas <- stagedLocation Locations.rollingSeas
      run $ TravelTo seas
      heading `shouldReturn` Just Cloudy

    it "Waterspout must be traveled to at the worst setting, and damages each Ship objective" . voyageTest $ \pid -> do
      run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.dreamChaser
      dreamChaser <- getsGame (toTarget . fromJustNote "no ship" . headMay . toList . (.entities.allies))
      spout <- stagedLocation Locations.waterspout
      setHeading Stormy
      getModifiers (LocationTarget spout) >>= (`shouldSatisfy` elem MustBeTraveledTo)
      run $ TravelTo spout
      getDamage dreamChaser `shouldReturn` 4

    it "Corsair Warship cannot be damaged and has archery 4 in the staging area" . voyageTest $ \pid -> do
      warship <- staged Enemies.corsairWarship
      getModifiers (EnemyTarget warship) >>= (`shouldSatisfy` \ms -> CannotTakeDamage `elem` ms && GainKeyword (Archery 4) `elem` ms)
      run $ PlaceEnemy warship (EngagedWith pid)
      getModifiers (EnemyTarget warship) >>= (`shouldSatisfy` notElem CannotTakeDamage)

    it "Southron Sailor has the engaged player's Ship enemies count their threat" . voyageTest $ \pid -> do
      scout <- staged Enemies.scoutingShip
      sailor <- staged Enemies.southronSailor
      run $ PlaceEnemy scout (EngagedWith pid)
      threat0 <- getStagingThreat
      run $ PlaceEnemy sailor (EngagedWith pid)
      getStagingThreat `shouldReturn` threat0 + 2

    it "Thrown Off Course shifts the heading off-course, if the first player chooses so" . voyageTest $ \pid -> do
      setHeading Sunny
      _ <- setEncounterDeck [Treacheries.thrownOffCourse]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.shiftHeadingOffCourse" pid
      heading `shouldReturn` Just Cloudy

    it "Winds of Wrath cannot be canceled at the worst setting" . voyageTest $ \_ -> do
      setHeading Stormy
      _ <- setEncounterDeck [Treacheries.windsOfWrath]
      run RevealEncounterCard
      winds <- getsGame (fromJustNote "no treachery" . fmap toTarget . headMay . toList . (.entities.treacheries))
      getModifiers winds >>= (`shouldSatisfy` elem WhenRevealedCannotBeCanceled)
