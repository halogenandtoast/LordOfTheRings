module LordOfTheRings.VengeanceOfMordor.PowerOfMordorSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEliminationLevel)
import LordOfTheRings.Message.Lifted (createPowerOfMordorDeck, getPowerOfMordorDeck, lowerThreatBy, revealTopOfPowerOfMordorDeck)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Types (powerOfMordorDeckL)
import LordOfTheRings.Window (WindowType (CardsDrawn), mkAfter)
import TestImport

-- | The Power of Mordor deck holds just these cards, top first.
setPowerOfMordorDeck :: [CardDef] -> GameT ()
setPowerOfMordorDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (powerOfMordorDeckL .~ cards)

-- | Reveal its top card, as a card does, and whether there was one.
revealTop :: GameT Bool
revealTop = do
  revealed <- runQueueT revealTopOfPowerOfMordorDeck
  _ <- mainLoop
  resolveRevealed
  pure revealed

revealOnly :: CardDef -> GameT ()
revealOnly def = setPowerOfMordorDeck [def] >> void revealTop

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = describe "The Power of Mordor deck" do
  it "is built from The Power of Mordor encounter set, its five cards" . gameTest $ \_ -> do
    runQueueT createPowerOfMordorDeck
    _ <- mainLoop
    codes <- sort . map toCardCode <$> getPowerOfMordorDeck
    codes `shouldBe` ["21066", "21067", "21068", "21069", "21070"]

  it "reveals its top card, and says when it has none to reveal" . gameTest $ \_ -> do
    setPowerOfMordorDeck [Locations.theTowerOfBaradDur, Objectives.theArmiesOfMordor]
    revealTop `shouldReturn` True
    (map toCardCode <$> getPowerOfMordorDeck) `shouldReturn` [toCardCode Objectives.theArmiesOfMordor]
    getsGame (any ((== toCardCode Locations.theTowerOfBaradDur) . toCardCode) . toList . (.entities.locations)) `shouldReturn` True
    setPowerOfMordorDeck []
    revealTop `shouldReturn` False

  describe "Khamûl the Easterling" do
    it "is revealed to the staging area, and engages only the first player" . twoPlayerTest testDeck $ \one two -> do
      revealOnly Enemies.khamulTheEasterling
      eid <- enemyOf Enemies.khamulTheEasterling
      getsGame (fmap (.placement) . lookup eid . (.entities.enemies)) `shouldReturn` Just StagingArea
      run $ EngageEnemy two eid
      getsGame (fmap (.placement) . lookup eid . (.entities.enemies)) `shouldReturn` Just (EngagedWith one)
      run $ SetFirstPlayer two
      getsGame (fmap (.placement) . lookup eid . (.entities.enemies)) `shouldReturn` Just (EngagedWith two)
      mods <- getModifiers (EnemyTarget eid)
      mods `shouldSatisfy` elem ImmuneToPlayerCardEffects
      mods `shouldSatisfy` elem OnlyEngagedPlayerFights

  describe "The Tower of Barad-dûr" do
    it "lowers each player's elimination threat by 5, and cannot be traveled to" . gameTest $ \pid -> do
      revealOnly Locations.theTowerOfBaradDur
      getEliminationLevel pid `shouldReturn` 45
      lid <- getsGame (fromJustNote "no tower" . fmap toId . headMay . toList . (.entities.locations))
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  describe "Under the Watchful Eye" do
    it "has each player discard down to 5 cards, then raises the threat of a player who draws" . gameTest $ \pid -> do
      replicateM_ 7 (putCardIntoHand pid Allies.guardOfTheCitadel)
      revealOnly Objectives.underTheWatchfulEye
      replicateM_ 2 (answerQuestion pid 0)
      (length . (.hand) <$> getPlayer pid) `shouldReturn` 5
      start <- threatOf pid
      run $ CheckWindows [mkAfter $ CardsDrawn pid 2]
      threatOf pid `shouldReturn` (start + 1)

  describe "The Armies of Mordor" do
    it "stops a non-unique enemy that engages a player taking damage this round" . gameTest $ \pid -> do
      revealOnly Objectives.theArmiesOfMordor
      run . AddToStaging =<< genEncounterCard Enemies.kingSpider
      eid <- enemyOf Enemies.kingSpider
      run $ EngageEnemy pid eid
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem CannotTakeDamage)

    it "leaves a unique enemy alone" . gameTest $ \pid -> do
      revealOnly Objectives.theArmiesOfMordor
      run . AddToStaging =<< genEncounterCard Enemies.khamulTheEasterling
      eid <- enemyOf Enemies.khamulTheEasterling
      run $ EngageEnemy pid eid
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` notElem CannotTakeDamage)

  describe "Covered in Darkness" do
    it "raises each player's threat by 5, and lets player cards reduce it by only 1 a round" . gameTest $ \pid -> do
      start <- threatOf pid
      revealOnly Objectives.coveredInDarkness
      threatOf pid `shouldReturn` (start + 5)
      hid <- toId <$> getHeroWithCode "01012"
      runQueueT (lowerThreatBy (HeroSource hid) pid 3)
      _ <- mainLoop
      threatOf pid `shouldReturn` (start + 4)
      runQueueT (lowerThreatBy (HeroSource hid) pid 3)
      _ <- mainLoop
      threatOf pid `shouldReturn` (start + 4)
