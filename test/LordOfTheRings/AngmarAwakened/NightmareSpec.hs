module LordOfTheRings.AngmarAwakened.NightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer), getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theWastesOfEriador)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)
import LordOfTheRings.Scenario.Types (GameMode (..), encounterDeckL, modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (RoundEnds))
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal cards from the encounter deck, the first on top.
reveal :: [CardDef] -> GameT ()
reveal defs = setEncounterDeck defs >> run RevealEncounterCard >> resolveRevealed

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

-- | Use every forced ability offered, one at a time, then pass the responses offered.
resolveForced :: PlayerId -> GameT ()
resolveForced pid =
  getQuestion pid >>= \case
    Just (ChooseOneAtATime (AbilityLabel {} : _)) -> chooseAbility pid >> resolveForced pid
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> resolveForced pid
    _ -> pure ()

timeShowing :: CardDef -> GameT ()
timeShowing side = run . AddToStaging =<< genEncounterCard side

timeObjective :: GameT ObjectiveId
timeObjective = getsGame (fromJustNote "no Time objective" . fmap toId . headMay . toList . (.entities.objectives))

spec :: Spec
spec = describe "Angmar Awakened Nightmare mode" do
  describe "The Wastes of Eriador" do
    it "builds the nightmare deck: removals out, the nightmare set in" . nightmare theWastesOfEriador $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.northernWarg, Enemies.whiteWarg, Treacheries.bitingWind, Enemies.packHunters, Quests.makeAStand]
        `shouldBe` [2, 2, 0, 5, 1]
      copies Nightmare.theWastesOfEriadorNightmare deck `shouldBe` 0

    it "Pack Hunters loses 30 engagement cost at Night, and finds a Warg as it engages" . nightmare theWastesOfEriador $ \pid -> do
      timeShowing Objectives.nightfall
      hunters <- staged Enemies.packHunters
      getModifiers (toTarget hunters) >>= (`shouldSatisfy` elem (EngagementCostModifier (-30)))
      [_, warg] <- setEncounterDeck [Locations.northDowns, Enemies.wolfOfAngmar]
      run $ EngageEnemy pid hunters
      resolveForced pid
      getQuestion pid >>= \case
        Just _ -> chooseTarget pid (CardIdTarget warg.id)
        Nothing -> pure ()
      resolveForced pid
      wolf <- enemyOf Enemies.wolfOfAngmar
      (.placement) <$> getEnemy wolf `shouldReturn` EngagedWith pid

    it "Shrouded Wilderness turns Day to Night as the players travel there" . nightmare theWastesOfEriador $ \pid -> do
      timeShowing Objectives.daybreak
      wild <- stagedLocation Locations.shroudedWilderness
      _ <- setEncounterDeck [Locations.northDowns]
      run $ TravelTo wild
      resolveForced pid
      isNight `shouldReturn` True
      getModifiers (toTarget wild) >>= (`shouldSatisfy` elem (ProgressLimit 0))

    it "Snowy Badlands keeps damage off the enemies in the staging area" . nightmare theWastesOfEriador $ \_ -> do
      warg <- staged Enemies.northernWarg
      _ <- stagedLocation Locations.snowyBadlands
      run $ DealDamage GameSource (toTarget warg) 2
      (.damage) <$> getEnemy warg `shouldReturn` 0

    it "Cunning Wolves adds 3 quest points to the current quest and bars engaging Wargs" . nightmare theWastesOfEriador $ \_ -> do
      q <- putQuestStage Quests.acrossTheWastes
      warg <- staged Enemies.northernWarg
      reveal [Treacheries.cunningWolves]
      getModifiers q >>= (`shouldSatisfy` elem (QuestPointModifier 3))
      getModifiers (toTarget warg) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))

    it "Torrent of Hail deals 1 damage to each questing character" . nightmare theWastesOfEriador $ \_ -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ CommitToQuest eowyn
      reveal [Treacheries.torrentOfHail]
      getCharacterDamage eowyn `shouldReturn` 1

    it "Make a Stand turns Day to Night and keeps it Night" . nightmare theWastesOfEriador $ \pid -> do
      timeShowing Objectives.daybreak
      _ <- setEncounterDeck [Locations.northDowns, Locations.northDowns]
      card <- genEncounterCard Quests.makeAStand
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (encounterDeckL %~ (card :))
      run RevealEncounterCard
      resolveRevealed
      resolveForced pid
      isNight `shouldReturn` True
      o <- timeObjective
      getModifiers (toTarget o) >>= (`shouldSatisfy` elem CannotBeFlipped)
      run $ CheckWindows [Window After RoundEnds]
      resolveForced pid
      isNight `shouldReturn` True

    it "Forced Off Track removes a progress from the main quest per Warg at the end of the round" . nightmare theWastesOfEriador $ \pid -> do
      q <- putQuestStage Quests.acrossTheWastes
      run $ PlaceTokens GameSource q Progress 5
      _ <- staged Enemies.northernWarg
      _ <- staged Enemies.whiteWarg
      run . AddToStaging =<< genEncounterCard Quests.forcedOffTrack
      run $ CheckWindows [Window After RoundEnds]
      resolveForced pid
      quests <- getsGame (toList . (.entities.quests))
      [p | p <- quests, toCardCode p == toCardCode Quests.acrossTheWastes] & map (.progress) & (`shouldBe` [3])

