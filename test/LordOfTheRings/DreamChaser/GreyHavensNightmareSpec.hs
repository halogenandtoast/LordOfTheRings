module LordOfTheRings.DreamChaser.GreyHavensNightmareSpec (spec) where

import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Heading (Heading (..))
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getLocationThreat)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (HeroMatcher (AttackingHero), selectAny)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (raidOnTheGreyHavens, theFateOfNumenor, voyageAcrossBelegaer)
import LordOfTheRings.Scenario.Types (GameMode (..), ScenarioAttrs (..), headingL, modeL, questDeckL, setAsideL, victoryDisplayL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (CardsDrawn, PhaseEnds), mkAfter, mkWhen)
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

io :: IO () -> IO ()
io = id

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

fromSet :: EncounterSet.EncounterSet -> [EncounterCard] -> Int
fromSet set = length . filter ((== Just set) . cdEncounterSet . toCardDef)

setHeading :: Heading -> GameT ()
setHeading h = do
  g <- (scenarioL %~ overAttrs (headingL ?~ h)) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

heading :: GameT (Maybe Heading)
heading = getsGame ((.heading) . toAttrs . (.scenario))

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

questDeck :: GameT [CardCode]
questDeck = map toCardCode . scenarioQuestDeck <$> scenarioAttrs

questsInPlay :: GameT [CardCode]
questsInPlay = getsGame (map toCardCode . toList . (.entities.quests))

setQuestDeck :: [CardDef] -> GameT [EncounterCard]
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)
  pure cards

-- | The stage in play, its When Revealed resolved, on its B side.
revealedStage :: PlayerId -> CardDef -> GameT Target
revealedStage pid def = do
  q <- putQuestStage def
  run $ Flip q
  resolveWhenRevealed pid
  pure q

stagedEnemy :: CardDef -> GameT EnemyId
stagedEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== card.id) . enemyCardId . toAttrs) . toList . (.entities.enemies))

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such location" . fmap toId . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

locationAttrs :: LocationId -> GameT LocationAttrs
locationAttrs lid = getsGame (fromJustNote "no such location" . fmap toAttrs . find ((== lid) . toId) . toList . (.entities.locations))

resourcesOn :: EnemyId -> GameT Int
resourcesOn eid = getsGame (maybe 0 (countTokens Resource . enemyTokens . toAttrs) . find ((== eid) . toId) . toList . (.entities.enemies))

ship :: PlayerId -> CardDef -> GameT Target
ship pid def = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard def
  getsGame (toTarget . fromJustNote "no such ally" . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

beravor :: GameT Target
beravor = toTarget <$> getHeroWithCode "01012"

reveal :: CardDef -> GameT ()
reveal def = setEncounterDeck [def] >> run RevealEncounterCard >> resolveRevealed

spec :: Spec
spec = describe "The Grey Havens Nightmare Decks" do
  describe "Voyage Across Belegaer" do
    it "builds the nightmare deck, and shuffles Corsair Confrontation and Lost at Sea in with the stage 2 cards" . nightmare voyageAcrossBelegaer $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.southronSailor, Locations.starlitSea, Locations.calmWaters, Treacheries.thrownOffCourse, Enemies.scoutingShip]
        `shouldBe` [0, 1, 0, 0, 2]
      fromSet EncounterSet.VoyageAcrossBelegaerNightmare deck `shouldBe` 16
      stages <- questDeck
      length stages `shouldBe` 7
      stages `shouldSatisfy` \s -> all (`elem` s) (map toCardCode [Quests.corsairConfrontation, Quests.lostAtSea])

    it "leaves a standard game's quest deck alone" . io . gameTestIn voyageAcrossBelegaer testDeck $ \_ -> do
      run $ DoStep 1 Setup
      length <$> questDeck `shouldReturn` 5

    it "Lost at Sea sets the heading to the worst setting, cancels the first icon, and, defeated on-course, offers the top 3 stages" . nightmare voyageAcrossBelegaer $ \pid -> do
      setHeading Sunny
      [_, _, third, _] <- setQuestDeck [Quests.intoTheStorm, Quests.cursedMists, Quests.theStarsGuidance, Quests.corsairPursuit]
      _ <- revealedStage pid Quests.lostAtSea
      heading `shouldReturn` Just Stormy
      getModifiers GameTarget >>= (`shouldSatisfy` elem CancelFirstSailingIcon)
      setHeading Sunny
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      chooseTarget pid third
      questsInPlay `shouldReturn` [toCardCode Quests.theStarsGuidance]
      questDeck `shouldReturn` map toCardCode [Quests.corsairPursuit, Quests.intoTheStorm, Quests.cursedMists]
      map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs `shouldReturn` [toCardCode Quests.lostAtSea]

    it "Corsair Confrontation adds a Corsair Warship, and is defeated at the end of the quest phase once none is in play" . nightmare voyageAcrossBelegaer $ \pid -> do
      _ <- setEncounterDeck [Enemies.corsairWarship]
      _ <- setQuestDeck [Quests.intoTheStorm, Quests.corsairPursuit]
      _ <- revealedStage pid Quests.corsairConfrontation
      chooseOptionMatching "take the warship" pid (const True)
      warship <- getsGame (fromJustNote "no warship" . fmap toId . headMay . toList . (.entities.enemies))
      run $ Discard (EnemyTarget warship)
      setHeading Cloudy
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      questsInPlay `shouldReturn` [toCardCode Quests.intoTheStorm]

    it "Corsair Confrontation may take the Corsair Warship from the victory display while the deck has one too" . nightmare voyageAcrossBelegaer $ \pid -> do
      _ <- setEncounterDeck [Enemies.corsairWarship]
      shown <- genEncounterCard Enemies.corsairWarship
      putGame . (scenarioL %~ overAttrs (\a -> a {scenarioVictoryDisplay = [EncounterCard shown]})) =<< getGame
      _ <- setQuestDeck [Quests.intoTheStorm, Quests.corsairPursuit]
      _ <- revealedStage pid Quests.corsairConfrontation
      chooseTarget pid (EncounterCard shown)
      scenarioVictoryDisplay <$> scenarioAttrs `shouldReturn` []
      length <$> getEncounterDeck `shouldReturn` 1

    it "Raging Squall damages each character committed to the Sailing test that discards it" . nightmare voyageAcrossBelegaer $ \pid -> do
      setHeading Sunny
      _ <- setEncounterDeck [Treacheries.ragingSquallNightmare]
      b <- beravor
      run SailingTest
      chooseTarget pid b
      chooseLabel "$label.sailingTest.done" pid
      chooseLabel "$label.continue" pid
      getCharacterDamage b `shouldReturn` 1
      heading `shouldReturn` Just Sunny

    it "Shattered Sea gets +2 threat and quest points for each setting off-course" . nightmare voyageAcrossBelegaer $ \_ -> do
      sea <- stagedLocation Locations.shatteredSea
      setHeading Sunny
      getLocationThreat sea `shouldReturn` 4
      setHeading Rainy
      getLocationThreat sea `shouldReturn` 8
      getModifiers (LocationTarget sea) >>= (`shouldSatisfy` elem (QuestPointModifier 4))

    it "Open Waters, in the staging area, gives each Ship enemy -5 engagement cost" . nightmare voyageAcrossBelegaer $ \_ -> do
      setHeading Sunny
      scout <- stagedEnemy Enemies.scoutingShip
      _ <- stagedLocation Locations.openWaters
      getEngagementCost scout `shouldReturn` 30

    it "Brash Corsair attacks as the engaged player exhausts a Ship objective" . nightmare voyageAcrossBelegaer $ \pid -> do
      dreamChaser <- ship pid Objectives.dreamChaser
      corsair <- stagedEnemy Enemies.brashCorsair
      run $ PlaceEnemy corsair (EngagedWith pid)
      run $ Exhaust dreamChaser
      getQuestion pid >>= (`shouldSatisfy` isJust)

    it "Dreams of Númenor may set the heading to the worst setting" . nightmare voyageAcrossBelegaer $ \pid -> do
      setHeading Sunny
      reveal Treacheries.dreamsOfNumenor
      chooseLabel "$label.dreamsOfNumenor.worst" pid
      heading `shouldReturn` Just Stormy

  describe "The Fate of Númenor" do
    it "Guardian of the Golden King removes attackers as soon as they are declared" . nightmare theFateOfNumenor $ \pid -> do
      guardian <- stagedEnemy Enemies.guardianOfTheGoldenKing
      run $ EngageEnemy pid guardian
      -- nothing at the bottom of the deck: X is 0
      g <- getGame
      putGame $ g & playersL . ix pid . deckL .~ []
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ DeclareAttackers pid guardian
      chooseTarget pid eowyn
      chooseLabel "$label.doneAttacking" pid
      selectAny AttackingHero `shouldReturn` False

    it "lets no more than 1 character attack Graxar each round" . nightmare theFateOfNumenor $ \pid -> do
      graxar <- stagedEnemy Enemies.graxar
      run $ EngageEnemy pid graxar
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ DeclareAttackers pid graxar
      chooseTarget pid eowyn
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> not ("$label.declareAttackers" `isPrefixOf` l); _ -> True)

    it "replaces the Uncharted deck with the Nightmare Deck's, Overgrown Steps staying in the encounter deck" . nightmare theFateOfNumenor $ \_ -> do
      run $ DoStep 1 Setup
      s <- scenarioAttrs
      map toCardCode (scenarioSetAside s) `shouldBe` [toCardCode Locations.shrineToMorgothNightmare]
      length s.unchartedDeck `shouldBe` 10
      s.unchartedDeck `shouldSatisfy` all ((== Just EncounterSet.TheFateOfNumenorNightmare) . cdEncounterSet . toCardDef)
      copies Locations.overgrownSteps s.encounterDeck `shouldBe` 3
      copies Locations.steepPlateau s.encounterDeck `shouldBe` 0

    it "places no progress on a non-Uncharted location in the staging area" . nightmare theFateOfNumenor $ \_ -> do
      steps <- stagedLocation Locations.overgrownSteps
      island <- stagedLocation Locations.cursedTempleNightmare
      -- as a card effect would place it
      run $ PlaceTokens ScenarioSource (LocationTarget steps) Progress 1
      (.progress) <$> locationAttrs steps `shouldReturn` 0
      getModifiers (LocationTarget island) >>= (`shouldSatisfy` notElem (ProgressLimit 0))

    it "turns Shrine to Morgoth back to Lost Island as the players would travel to it" . nightmare theFateOfNumenor $ \_ -> do
      shrine <- stagedLocation Locations.shrineToMorgothNightmare
      run $ Flip (LocationTarget shrine)
      toCardCode <$> locationAttrs shrine `shouldReturn` toCardCode Locations.shrineToMorgothNightmare
      run $ TravelTo shrine
      l <- locationAttrs shrine
      (toCardCode l, l.placement) `shouldBe` (toCardCode Locations.lostIsland, StagingArea)

    it "Overgrown Steps gives each Uncharted location in the staging area +1 threat" . nightmare theFateOfNumenor $ \_ -> do
      island <- stagedLocation Locations.forsakenCity
      _ <- stagedLocation Locations.overgrownSteps
      getLocationThreat island `shouldReturn` 3

    it "Desecrated Grounds strengthens each Undead enemy, and damages each exhausted character as it flips while active" . nightmare theFateOfNumenor $ \_ -> do
      dead <- stagedEnemy Enemies.drownedDead
      grounds <- stagedLocation Locations.desecratedGrounds
      run . Exhaust =<< beravor
      run $ PlaceLocation grounds ActiveLocation
      getModifiers (EnemyTarget dead) >>= (`shouldSatisfy` \ms -> ThreatModifier 1 `elem` ms && AttackModifier 1 `elem` ms)
      (getCharacterDamage =<< beravor) `shouldReturn` 1

    it "Calphon's Folly flips an Uncharted location without a Forced effect and switches it with the active location" . nightmare theFateOfNumenor $ \_ -> do
      steps <- stagedLocation Locations.overgrownSteps
      run $ PlaceLocation steps ActiveLocation
      temple <- stagedLocation Locations.cursedTempleNightmare
      reveal Treacheries.calphonsFolly
      t <- locationAttrs temple
      (toCardCode t, t.placement) `shouldBe` (toCardCode Locations.cursedTempleNightmare, ActiveLocation)
      (.placement) <$> locationAttrs steps `shouldReturn` StagingArea

  describe "Raid on the Grey Havens" do
    it "builds the nightmare deck with its own Sahír's Advance" . nightmare raidOnTheGreyHavens $ \_ -> do
      run $ DoStep 1 Setup
      s <- scenarioAttrs
      map (`copies` s.encounterDeck) [Enemies.corsairArsonist, Locations.elvenWaveRunner, Enemies.umbarRaider, Enemies.corsairPillager]
        `shouldBe` [0, 0, 2, 3]
      map toCardCode s.questDeck `shouldBe` [toCardCode Quests.sahirsAdvanceNightmare]

    it "keeps attachments off Captain Sahír and Na'asiyah" . nightmare raidOnTheGreyHavens $ \_ -> do
      sahir <- stagedEnemy Enemies.captainSahir
      getModifiers (EnemyTarget sahir) >>= (`shouldSatisfy` elem CannotHaveAttachments)

    it "has Captain Sahír cancel damage with Na'asiyah's resources once his own are spent" . nightmare raidOnTheGreyHavens $ \_ -> do
      _ <- putQuestStage Quests.sahirsAdvanceNightmare
      sahir <- stagedEnemy Enemies.captainSahir
      naasiyah <- stagedEnemy Enemies.naasiyah
      run $ PlaceTokens GameSource (EnemyTarget sahir) Resource 1
      run $ PlaceTokens GameSource (EnemyTarget naasiyah) Resource 2
      run $ DealDamage GameSource (EnemyTarget sahir) 2
      getDamage (EnemyTarget sahir) `shouldReturn` 0
      (,) <$> resourcesOn sahir <*> resourcesOn naasiyah `shouldReturn` (0, 1)

    it "is won once Captain Sahír and Na'asiyah are both in the victory display" . nightmare raidOnTheGreyHavens $ \pid -> do
      elite <- traverse genEncounterCard [Enemies.captainSahir, Enemies.naasiyah]
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (setAsideL .~ elite)
      _ <- revealedStage pid Quests.sahirsAdvanceNightmare
      g' <- getGame
      putGame $ g' & scenarioL %~ overAttrs (victoryDisplayL .~ map EncounterCard elite)
      run . CheckStage =<< getsGame (fromJustNote "no quest" . fmap toId . headMay . toList . (.entities.quests))
      getsGame (.outcome) `shouldReturn` Just Won

    it "Elite Marauder's excess damage goes point by point to Aflame locations of the player's choice" . nightmare raidOnTheGreyHavens $ \pid -> do
      white <- stagedLocation Locations.whiteShip
      caravel <- stagedLocation Locations.elvenCaravel
      eid <- stagedEnemy Enemies.eliteMarauder
      run $ EngageEnemy pid eid
      b <- beravor
      -- 6 attack against Beravor's 2 defense and 2 hit points left: 2 excess
      run $ DealDamage GameSource b 2
      run $ AttackDamage eid b
      chooseTarget pid white
      chooseTarget pid caravel
      map (.damage) <$> traverse locationAttrs [white, caravel] `shouldReturn` [1, 1]

    it "Corsair Pyromaniac takes a resource as a card goes underneath The Havens Burn" . nightmare raidOnTheGreyHavens $ \_ -> do
      run . AddToStaging =<< genEncounterCard Objectives.theHavensBurn
      havens <- getsGame (toTarget . fromJustNote "no Havens Burn" . headMay . toList . (.entities.objectives))
      pyro <- stagedEnemy Enemies.corsairPyromaniac
      _ <- setEncounterDeck [Locations.whiteShip]
      card <- genEncounterCard Locations.burningPiers
      run $ PlaceUnderneath havens [EncounterCard card]
      resourcesOn pyro `shouldReturn` 1
      getModifiers (EnemyTarget pyro) >>= (`shouldSatisfy` elem (AttackModifier 1))

    it "Burning Tower keeps damage from being healed, and damages a character for each damage on it" . nightmare raidOnTheGreyHavens $ \pid -> do
      tower <- stagedLocation Locations.burningTower
      b <- beravor
      getModifiers b >>= (`shouldSatisfy` elem CannotBeHealed)
      run $ DealDamage GameSource (LocationTarget tower) 1
      chooseTarget pid b
      getCharacterDamage b `shouldReturn` 1

    it "Dawn Star takes damage as the first player draws" . nightmare raidOnTheGreyHavens $ \pid -> do
      star <- stagedLocation Locations.dawnStarLocation
      run $ CheckWindows [mkAfter $ CardsDrawn pid 1]
      getDamage (LocationTarget star) `shouldReturn` 1

    it "Infiltration moves a resource from each hero to the Raider enemies" . nightmare raidOnTheGreyHavens $ \_ -> do
      heroes <- getsGame (map toTarget . toList . (.entities.heroes))
      for_ heroes \h -> run $ PlaceTokens GameSource h Resource 1
      raider <- stagedEnemy Enemies.umbarRaider
      reveal Treacheries.infiltration
      resourcesOn raider `shouldReturn` 3
