module LordOfTheRings.DreamChaser.AStormOnCobasHavenSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (..), HasCardCode, IsCard (toCard), toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Heading (Heading (..))
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (aStormOnCobasHaven, lookupScenario)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), headingL, questDeckL, setAsideL)
import LordOfTheRings.Trait (Trait (Ship))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import TestImport

cobasTest :: (PlayerId -> GameT ()) -> IO ()
cobasTest = gameTestIn aStormOnCobasHaven testDeck

setHeading :: Heading -> GameT ()
setHeading h = do
  g <- (scenarioL %~ overAttrs (headingL ?~ h)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

-- | The stage in play on its B side, the given stages in the quest deck.
flippedStage :: CardDef -> [CardDef] -> GameT Target
flippedStage def rest = do
  q <- putQuestStage def
  cards <- traverse genEncounterCard rest
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)
  putGame =<< preloadModifiers =<< getGame
  pure q

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stagedObjective :: CardDef -> GameT ObjectiveId
stagedObjective def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such objective" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

cardIdOf :: EnemyId -> GameT CardId
cardIdOf eid = getsGame ((.id) . toCard . fromJustNote "no such enemy" . lookup eid . mapToList . (.entities.enemies))

guardOf :: ObjectiveId -> GameT (Maybe CardId)
guardOf oid = getsGame (objectiveGuard . toAttrs . fromJustNote "no such objective" . lookup oid . mapToList . (.entities.objectives))

ship :: PlayerId -> CardDef -> GameT Target
ship pid def = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard def
  getsGame (toTarget . fromJustNote "no such ally" . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

codesOf :: (HasField "placement" a Placement, HasCardCode a) => Placement -> [a] -> [CardCode]
codesOf p = map toCardCode . filter ((== p) . (.placement))

questsInPlay :: GameT [CardCode]
questsInPlay = getsGame (map toCardCode . toList . (.entities.quests))

spec :: Spec
spec = describe "A Storm on Cobas Haven" do
  it "is a scenario the game can be dealt" . cobasTest $ \_ ->
    isJust (lookupScenario "a-storm-on-cobas-haven") `shouldBe` True

  describe "Setup" do
    it "prepares the fleet, sets the Warships aside, stages Raider Flagship and Scouting Ship, and guards the Dol Amroth objective chosen" . cobasTest $ \pid -> do
      flipFirstQuest pid
      chooseOptionMatching "choose a ship" pid (const True)
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> l == "$label.outmaneuverTheEnemy.objective"; _ -> False)
      chooseOptionMatching "choose a tower" pid (const True)
      staging <- getsGame (codesOf StagingArea . toList . (.entities.enemies))
      staging `shouldSatisfy` \cs -> all (`elem` cs) [toCardCode Enemies.raiderFlagship, toCardCode Enemies.scoutingShip]
      objectives <- getsGame (toList . (.entities.objectives))
      length objectives `shouldSatisfy` (>= 1)
      objectives `shouldSatisfy` any (isJust . objectiveGuard . toAttrs)
      setAside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      map toCardCode setAside `shouldBe` replicate 2 (toCardCode Objectives.dolAmrothWarship)
      corsairs <- getsGame (fromMaybe [] . (.corsairDeck) . toAttrs . (.scenario))
      corsairs `shouldSatisfy` all (\c -> Ship `notMember` cdCardTraits (toCardDef c))

  describe "Guarded objectives" do
    it "are under the players' control only once free of the encounter guarding them" . cobasTest $ \_ -> do
      tower <- stagedObjective Objectives.seaWardTower
      siege <- staged Enemies.siegeShip
      run . SetObjectiveGuard tower =<< cardIdOf siege
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (SailingTestAdditionalCards 2))
      run $ Discard (EnemyTarget siege)
      getModifiers GameTarget >>= (`shouldSatisfy` elem (SailingTestAdditionalCards 2))

    it "keep Battle in the Bay from advancing while one is guarded" . cobasTest $ \_ -> do
      QuestTarget stage <- flippedStage Quests.battleInTheBay [Quests.breakThroughTheFleet]
      tower <- stagedObjective Objectives.towerOfTheHeron
      siege <- staged Enemies.siegeShip
      run . SetObjectiveGuard tower =<< cardIdOf siege
      run $ PlaceTokens GameSource (QuestTarget stage) Progress 16
      questsInPlay `shouldReturn` [toCardCode Quests.battleInTheBay]
      run $ Discard (EnemyTarget siege)
      questsInPlay `shouldReturn` [toCardCode Quests.breakThroughTheFleet]

    it "Heavy Cruiser fetches a Dol Amroth objective and guards it; destroyed, it frees Tower of the Heron to cut the archery total" . cobasTest $ \_ -> do
      _ <- setEncounterDeck [Enemies.heavyCruiser, Objectives.towerOfTheHeron]
      run RevealEncounterCard
      resolveRevealed
      cruiser <- getsGame (fromJustNote "no cruiser" . fmap toId . headMay . toList . (.entities.enemies))
      tower <- getsGame (fromJustNote "no tower" . fmap toId . headMay . toList . (.entities.objectives))
      cid <- cardIdOf cruiser
      guardOf tower `shouldReturn` Just cid
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (ArcheryBonus (-4)))
      run $ Discard (EnemyTarget cruiser)
      getModifiers GameTarget >>= (`shouldSatisfy` elem (ArcheryBonus (-4)))

    it "Siege Ship guards an unguarded objective at the end of the quest phase" . cobasTest $ \_ -> do
      tower <- stagedObjective Objectives.towerOfTheGull
      siege <- staged Enemies.siegeShip
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      cid <- cardIdOf siege
      guardOf tower `shouldReturn` Just cid

  describe "Raider Flagship" do
    it "can be attacked only by Ship objectives" . cobasTest $ \pid -> do
      dreamChaser <- ship pid Objectives.dreamChaser
      beravor <- toTarget <$> getHeroWithCode "01012"
      flagship <- staged Enemies.raiderFlagship
      canFight flagship beravor `shouldReturn` False
      canFight flagship dreamChaser `shouldReturn` True

    it "gets -50 engagement cost at the worst heading on the first stage, and always on the last" . cobasTest $ \_ -> do
      _ <- flippedStage Quests.outmaneuverTheEnemy []
      flagship <- staged Enemies.raiderFlagship
      setHeading Rainy
      getEngagementCost flagship `shouldReturn` 50
      setHeading Stormy
      getEngagementCost flagship `shouldReturn` 0

    it "destroyed on Break Through the Fleet!, wins the game" . cobasTest $ \_ -> do
      _ <- flippedStage Quests.breakThroughTheFleet []
      flagship <- staged Enemies.raiderFlagship
      run $ Destroy GameSource (EnemyTarget flagship)
      getsGame (.outcome) `shouldReturn` Just Won

  describe "Encounter cards" do
    it "Corsair Skirmisher, destroyed, may be taken as a Ship objective by discarding allies with 3 willpower" . cobasTest $ \pid -> do
      gildor <- putAllyIntoPlay pid Allies.gildorInglorion
      skirmisher <- staged Enemies.corsairSkirmisher
      run $ PlaceEnemy skirmisher (EngagedWith pid)
      run $ Destroy GameSource (EnemyTarget skirmisher)
      chooseLabel "$label.corsairSkirmisher.discard" pid
      chooseTarget pid gildor
      taken <- getsGame (find ((== toCardCode Objectives.corsairSkirmisherShip) . toCardCode) . toList . (.entities.allies))
      fmap (.placement) taken `shouldBe` Just (InPlayArea pid)
      for_ taken \s -> getWillpower (toTarget s) `shouldReturn` 3
      getsGame (toList . (.entities.enemies)) >>= (`shouldSatisfy` null)

    it "The Beacon gives a set-aside Dol Amroth Warship to the player who triggers it" . cobasTest $ \pid -> do
      warship <- genEncounterCard Objectives.dolAmrothWarship
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [warship])
      beacon <- stagedObjective Objectives.theBeacon
      run $ UseCardAbility pid (ObjectiveSource beacon) 1 []
      getsGame (map toCardCode . toList . (.entities.allies)) `shouldReturn` [toCardCode Objectives.dolAmrothWarship]
      getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` []

    it "Ramming Speed! gains doomed 2 when no attack is made" . cobasTest $ \pid -> do
      setHeading Sunny
      threat <- (.threat) <$> getPlayer pid
      _ <- setEncounterDeck [Treacheries.rammingSpeed]
      run RevealEncounterCard
      resolveRevealed
      (.threat) <$> getPlayer pid `shouldReturn` threat + 2
