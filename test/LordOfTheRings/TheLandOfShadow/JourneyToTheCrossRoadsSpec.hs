module LordOfTheRings.TheLandOfShadow.JourneyToTheCrossRoadsSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Campaign
import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getEngagementCost, getLocationThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getArcheryTotal, getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (journeyToTheCrossRoads, lookupScenario)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

crossRoadsTest :: (PlayerId -> GameT ()) -> IO ()
crossRoadsTest = gameTestIn journeyToTheCrossRoads testDeck

-- | A campaign log with The Passage of the Marshes just won.
lordOfTheRings :: CampaignLog
lordOfTheRings =
  (newCampaignLog "the-lord-of-the-rings")
    { campaignLogHeroes = [["01012", "01007", "01002"]]
    , campaignLogCompleted =
        [ CompletedScenario sid 100
        | sid <- ["a-shadow-of-the-past", "a-knife-in-the-dark", "flight-to-the-ford", "the-ring-goes-south", "journey-in-the-dark", "breaking-of-the-fellowship", "the-uruk-hai", "helms-deep", "the-road-to-isengard", "the-passage-of-the-marshes"]
        ]
    }

campaignTest :: (PlayerId -> GameT ()) -> IO ()
campaignTest = campaignTestIn journeyToTheCrossRoads lordOfTheRings testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup with the encounter deck given and its stage 1A clicked; in campaign mode the
-- campaign card's choice is answered with the label given.
setUpChoosing :: Maybe Text -> [CardDef] -> PlayerId -> GameT ()
setUpChoosing choice deck pid = do
  run Setup
  chooseRingBearer pid (toCardCode Heroes.frodoBagginsTheLandOfShadow)
  chooseLabel "$label.keepHand" pid
  _ <- setEncounterDeck deck
  chooseOptionMatching "flip the quest" pid (const True)
  for_ choice \l -> chooseLabel l pid

setUp :: PlayerId -> GameT ()
setUp = setUpChoosing Nothing [Locations.hillsOfIthilien, Enemies.manOfHarad, Treacheries.marchingUpTheRoad]

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engagedEnemy :: PlayerId -> CardDef -> GameT EnemyId
engagedEnemy pid def = do
  eid <- stageEnemy def
  run $ EngageEnemy pid eid
  pure eid

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

locationAttrs :: LocationId -> GameT LocationAttrs
locationAttrs lid = toAttrs <$> getLocation lid

crossRoads :: GameT LocationId
crossRoads = locationOf Locations.theCrossRoadsTheLandOfShadow

underCrossRoads :: GameT [Card]
underCrossRoads = locationUnderneath <$> (locationAttrs =<< crossRoads)

blackGate :: GameT ObjectiveAttrs
blackGate = getsGame (fromJustNote "no Black Gate" . fmap toAttrs . find ((== toCardCode Objectives.theBlackGate) . toCardCode) . toList . (.entities.objectives))

underBlackGate :: GameT [CardCode]
underBlackGate = map toCardCode . objectiveUnderneath <$> blackGate

questionKey :: PlayerId -> GameT (Maybe Text)
questionKey pid = fmap (\case QuestionLabel l _ _ _ -> l; _ -> "") <$> getQuestion pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | So much progress on stage 1B.
questProgress :: Int -> GameT ()
questProgress n = do
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Progress n

spec :: Spec
spec = describe "Journey to the Cross-roads" do
  it "is a scenario the game can be dealt" . crossRoadsTest $ \_ ->
    isJust (lookupScenario "journey-to-the-cross-roads") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "builds the encounter deck from Journey to the Cross-roads, setting aside The Black Gate, The Cross-roads and Men of Harad" . crossRoadsTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 29
      all ((== Just JourneyToTheCrossRoads) . cdEncounterSet . toCardDef) deck `shouldBe` True
      map (`copies` deck) [Objectives.theBlackGate, Locations.theCrossRoadsTheLandOfShadow, Enemies.manOfHarad, Enemies.captainOfHarad, Locations.hillsOfIthilien]
        `shouldBe` [0, 0, 4, 3, 4]
      aside <- scenarioSetAside <$> scenarioAttrs
      map (`copies` aside) [Objectives.theBlackGate, Locations.theCrossRoadsTheLandOfShadow, Enemies.oliphaunt, Enemies.haradrimSoldier, Enemies.haradrimArcher]
        `shouldBe` [1, 1, 3, 3, 3]

    it "adds The Black Gate and The Cross-roads to the staging area, with the Men of Harad facedown under The Cross-roads" . crossRoadsTest $ \pid -> do
      setUp pid
      _ <- blackGate
      under <- underCrossRoads
      length under `shouldBe` 9
      all ((`elem` map toCardCode [Enemies.oliphaunt, Enemies.haradrimSoldier, Enemies.haradrimArcher]) . toCardCode) under `shouldBe` True
      cr <- crossRoads
      (.placement) <$> locationAttrs cr `shouldReturn` StagingArea
      getLocationThreat cr `shouldReturn` 0
      getModifiers (LocationTarget cr) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      scenarioSetAside <$> scenarioAttrs `shouldReturn` []
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.ambushTheMenOfHarad

    it "has each player but the first add a different non-unique location to the staging area" . twoPlayerTestIn journeyToTheCrossRoads testDeck $ \p1 p2 -> do
      run Setup
      chooseRingBearer p1 (toCardCode Heroes.frodoBagginsTheLandOfShadow)
      -- each player keeps his hand, whoever is asked first
      replicateM_ 2 $ for_ [p1, p2] \p -> do
        key <- questionKey p
        when (key == Just "$label.keepStartingHand") $ chooseLabel "$label.keepHand" p
      cards <- setEncounterDeck [Locations.hennethAnnun, Locations.ithilienWoods, Locations.hillsOfIthilien, Enemies.manOfHarad]
      chooseOptionMatching "flip the quest" p1 (const True)
      questionKey p2 `shouldReturn` Just "$label.ambushTheMenOfHarad.search"
      -- Henneth Annûn is unique, so not offered
      Just q <- getQuestion p2
      length (questionChoices q) `shouldBe` 2
      let woods = [c | c <- cards, toCardCode c == toCardCode Locations.ithilienWoods]
      for_ woods \c -> chooseTarget p2 (CardIdTarget c.id)
      woodsId <- locationOf Locations.ithilienWoods
      (.placement) <$> locationAttrs woodsId `shouldReturn` StagingArea
      length <$> getEncounterDeck `shouldReturn` 3

    it "lets each player add 1 resource to each of his heroes in campaign mode" . campaignTest $ \pid -> do
      setUpChoosing (Just "$label.journeyToTheCrossRoads.resources") [Locations.hillsOfIthilien] pid
      heroes <- getsGame (filter ((== pid) . (.owner)) . toList . (.entities.heroes))
      map (.resources) heroes `shouldSatisfy` all (== 1)
      deck <- (.deck) <$> getPlayer pid
      [() | PlayerCard c <- deck, toCardCode c == toCardCode Events.braceOfConeys] `shouldBe` []

    it "lets each player shuffle Brace of Coneys into his deck in campaign mode" . campaignTest $ \pid -> do
      setUpChoosing (Just "$label.journeyToTheCrossRoads.braceOfConeys") [Locations.hillsOfIthilien] pid
      deck <- (.deck) <$> getPlayer pid
      length [() | PlayerCard c <- deck, toCardCode c == toCardCode Events.braceOfConeys] `shouldBe` 1

  -- \* Stage 1B

  describe "Ambush the Men of Harad" do
    it "reveals the top card under The Cross-roads at the beginning of the quest phase" . crossRoadsTest $ \pid -> do
      setUp pid
      top <- fromJustNote "nothing under" . headMay <$> underCrossRoads
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      length <$> underCrossRoads `shouldReturn` 8
      eid <- enemyOf (toCardDef top)
      placementOfEnemy eid `shouldReturn` StagingArea

    it "cannot be defeated, and The Cross-roads cannot be traveled to until it has no cards under it and 20 progress is on the stage" . crossRoadsTest $ \pid -> do
      setUp pid
      cr <- crossRoads
      questProgress 25
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.ambushTheMenOfHarad
      getModifiers (LocationTarget cr) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      replicateM_ 9 $ run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      underCrossRoads `shouldReturn` []
      getModifiers (LocationTarget cr) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)

    it "wins the game when The Cross-roads is explored" . crossRoadsTest $ \pid -> do
      setUp pid
      cr <- crossRoads
      run $ PlaceLocation cr ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget cr) Progress 9
      getsGame (.outcome) `shouldReturn` Just Won

  -- \* The Black Gate

  describe "The Black Gate" do
    it "takes each Harad enemy in the staging area facedown at the end of the planning phase" . crossRoadsTest $ \pid -> do
      setUp pid
      _ <- stageEnemy Enemies.captainOfHarad
      engaged <- engagedEnemy pid Enemies.manOfHarad
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      underBlackGate `shouldReturn` [toCardCode Enemies.captainOfHarad]
      enemyCodes >>= (`shouldSatisfy` notElem (toCardCode Enemies.captainOfHarad))
      placementOfEnemy engaged `shouldReturn` EngagedWith pid

    it "loses the game with 3 more facedown cards under it than players" . crossRoadsTest $ \pid -> do
      setUp pid
      replicateM_ 3 (stageEnemy Enemies.captainOfHarad)
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      getsGame (.outcome) `shouldReturn` Nothing
      _ <- stageEnemy Enemies.captainOfHarad
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      getsGame (.outcome) `shouldReturn` Just Lost

  -- \* Locations

  describe "Henneth Annûn" do
    it "returns engaged enemies to the staging area and skips the encounter and combat phases while active" . crossRoadsTest $ \pid -> do
      setUp pid
      eid <- engagedEnemy pid Enemies.captainOfHarad
      lid <- stageLocation Locations.hennethAnnun
      run $ TravelTo lid
      placementOfEnemy eid `shouldReturn` StagingArea
      gameMods <- getModifiers GameTarget
      gameMods `shouldSatisfy` elem (SkipPhase EncounterPhase)
      gameMods `shouldSatisfy` elem (SkipPhase CombatPhase)
      q <- toTarget <$> getActiveQuest
      getModifiers q >>= (`shouldSatisfy` elem CannotTriggerAbilities)
      gate <- blackGate
      getModifiers (ObjectiveTarget gate.objectiveId) >>= (`shouldSatisfy` elem CannotTriggerAbilities)
      -- the Forced effect of stage 1B is ignored
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      length <$> underCrossRoads `shouldReturn` 9

  describe "Southward Road" do
    it "reveals the top card under The Cross-roads to travel there, and lets each player engage an additional enemy" . crossRoadsTest $ \pid -> do
      setUp pid
      lid <- stageLocation Locations.southwardRoad
      run $ TravelTo lid
      length <$> underCrossRoads `shouldReturn` 8
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem OptionallyEngagesAdditionalEnemy)

  describe "Foothills of Ephel Dúath" do
    it "cannot be traveled to without an enemy in the staging area, and engages one to travel there" . crossRoadsTest $ \pid -> do
      setUp pid
      lid <- stageLocation Locations.foothillsOfEphelDuath
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      eid <- stageEnemy Enemies.captainOfHarad
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo lid
      placementOfEnemy eid `shouldReturn` EngagedWith pid
      other <- stageEnemy Enemies.manOfHarad
      getEnemyThreat other `shouldReturn` 0

  describe "Hills of Ithilien" do
    it "deals 1 damage to a Harad enemy engaging a player while active" . crossRoadsTest $ \pid -> do
      setUp pid
      lid <- stageLocation Locations.hillsOfIthilien
      run $ PlaceLocation lid ActiveLocation
      eid <- engagedEnemy pid Enemies.captainOfHarad
      -- the Forced effects of Captain of Harad and Hills of Ithilien are ordered
      offered <- offersAbilityOf pid (LocationTarget lid)
      when offered $ chooseAbilityOf pid (LocationTarget lid)
      e <- getEnemy eid
      e.damage `shouldBe` 1

  describe "Ithilien Woods" do
    it "readies a character of the player engaging a Harad enemy while active" . crossRoadsTest $ \pid -> do
      setUp pid
      lid <- stageLocation Locations.ithilienWoods
      run $ PlaceLocation lid ActiveLocation
      hero <- fromJustNote "no hero" . headMay <$> getsGame (filter ((== pid) . (.owner)) . toList . (.entities.heroes))
      run $ Exhaust (toTarget hero)
      _ <- engagedEnemy pid Enemies.captainOfHarad
      -- the Forced effects of Captain of Harad and Ithilien Woods are ordered
      offered <- offersAbilityOf pid (LocationTarget lid)
      when offered $ chooseAbilityOf pid (LocationTarget lid)
      -- the only exhausted character is readied without asking
      h <- getHeroWithCode (toCardCode hero)
      heroExhausted (toAttrs h) `shouldBe` False

  -- \* Enemies

  describe "Captain of Harad" do
    it "gives each Harad enemy +1 attack and -10 engagement cost after it engages you" . crossRoadsTest $ \pid -> do
      setUp pid
      man <- stageEnemy Enemies.manOfHarad
      _ <- engagedEnemy pid Enemies.captainOfHarad
      getEnemyAttack man `shouldReturn` 4
      getEngagementCost man `shouldReturn` 26

  describe "Oliphaunt" do
    it "exhausts each ally of the player it engages and takes no more than 3 damage a round" . crossRoadsTest $ \pid -> do
      setUp pid
      ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      eid <- engagedEnemy pid Enemies.oliphaunt
      a <- getsGame (fromJustNote "no ally" . find ((== ally) . toTarget) . toList . (.entities.allies))
      allyExhausted (toAttrs a) `shouldBe` True
      run $ DealDamage GameSource (EnemyTarget eid) 5
      (.damage) <$> getEnemy eid `shouldReturn` 3

  describe "Haradrim Archer" do
    it "has threat and archery equal to the number of players" . crossRoadsTest $ \pid -> do
      setUp pid
      earlier <- getArcheryTotal
      eid <- stageEnemy Enemies.haradrimArcher
      getEnemyThreat eid `shouldReturn` 1
      getArcheryTotal `shouldReturn` (earlier + 1)

  describe "Man of Harad" do
    it "discards a random card from the hand of the player it engages" . crossRoadsTest $ \pid -> do
      setUp pid
      earlier <- length . (.hand) <$> getPlayer pid
      _ <- engagedEnemy pid Enemies.manOfHarad
      length . (.hand) <$> getPlayer pid `shouldReturn` (earlier - 1)

  -- \* Treacheries

  describe "The Dark Lord's Summons" do
    it "places the lowest attack Harad enemy engaged with you under The Black Gate" . crossRoadsTest $ \pid -> do
      setUp pid
      _ <- engagedEnemy pid Enemies.manOfHarad
      _ <- engagedEnemy pid Enemies.captainOfHarad
      reveal [Treacheries.theDarkLordsSummons, Locations.hillsOfIthilien]
      chooseLabel "$label.theDarkLordsSummons.blackGate" pid
      underBlackGate `shouldReturn` [toCardCode Enemies.manOfHarad]

    it "puts the top card under The Cross-roads into play engaged with you when no Harad enemy is engaged with you" . crossRoadsTest $ \pid -> do
      setUp pid
      top <- fromJustNote "nothing under" . headMay <$> underCrossRoads
      reveal [Treacheries.theDarkLordsSummons, Locations.hillsOfIthilien]
      length <$> underCrossRoads `shouldReturn` 8
      eid <- enemyOf (toCardDef top)
      placementOfEnemy eid `shouldReturn` EngagedWith pid

  describe "Fierce Southrons" do
    it "places the lowest threat Harad enemy in the staging area under The Black Gate and reveals another card" . crossRoadsTest $ \pid -> do
      setUp pid
      _ <- stageEnemy Enemies.captainOfHarad
      _ <- stageEnemy Enemies.manOfHarad
      reveal [Treacheries.fierceSouthrons, Locations.ithilienWoods]
      chooseLabel "$label.fierceSouthrons.blackGate" pid
      underBlackGate `shouldReturn` [toCardCode Enemies.manOfHarad]
      void $ locationOf Locations.ithilienWoods

    it "has each enemy in the staging area attack you otherwise" . crossRoadsTest $ \pid -> do
      setUp pid
      _ <- stageEnemy Enemies.captainOfHarad
      reveal [Treacheries.fierceSouthrons, Locations.ithilienWoods]
      chooseLabel "$label.fierceSouthrons.attack" pid
      passActions pid
      questionKey pid >>= (`shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`))

  describe "Marching Up the Road" do
    it "puts a Harad enemy searched for into play engaged with each player without an engaged enemy" . crossRoadsTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Treacheries.marchingUpTheRoad, Enemies.captainOfHarad, Locations.ithilienWoods]
      run RevealEncounterCard
      resolveRevealed
      asked <- questionKey pid
      when (asked == Just "$label.marchingUpTheRoad.searchFor") $ chooseOptionMatching "take the Captain" pid (const True)
      eid <- enemyOf Enemies.captainOfHarad
      placementOfEnemy eid `shouldReturn` EngagedWith pid

    it "places the attacking enemy under The Black Gate if its attack destroys a character" . crossRoadsTest $ \pid -> do
      setUp pid
      eid <- engagedEnemy pid Enemies.captainOfHarad
      guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Treacheries.marchingUpTheRoad]
      inPhase CombatPhase
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid guard'
      resolveShadowsInOrder pid
      passActions pid
      underBlackGate `shouldReturn` [toCardCode Enemies.captainOfHarad]

  describe "Arrows Thick in the Air" do
    it "places the enemy under The Black Gate after its attack only when that option is chosen" . crossRoadsTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
      eid <- engagedEnemy pid Enemies.forestSpider
      defender <- toTarget <$> getHeroWithCode "01012"
      _ <- setEncounterDeck [Enemies.hostOfIsengard]
      run . AttachShadowCard eid =<< genEncounterCard Treacheries.arrowsThickInTheAir
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid defender
      passActions pid
      chooseLabel "$label.arrowsThickInTheAir.blackGate" pid
      passActions pid
      getDamage defender `shouldReturn` 1
      underBlackGate `shouldReturn` [toCardCode Enemies.forestSpider]
      getsGame (Map.member eid . (.entities.enemies)) `shouldReturn` False
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.hostOfIsengard]

    it "makes the additional attack without taking the unchosen Black Gate option" . crossRoadsTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
      eid <- engagedEnemy pid Enemies.forestSpider
      defender <- toTarget <$> getHeroWithCode "01012"
      otherDefender <- toTarget <$> getHeroWithCode "01007"
      _ <- setEncounterDeck [Enemies.hostOfIsengard]
      run . AttachShadowCard eid =<< genEncounterCard Treacheries.arrowsThickInTheAir
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid defender
      passActions pid
      chooseLabel "$label.arrowsThickInTheAir.attack" pid
      passActions pid
      getDamage defender `shouldReturn` 1
      underBlackGate `shouldReturn` []
      chooseTarget pid otherDefender
      passActions pid
      resolveShadowsInOrder pid
      passActions pid
      getDamage otherDefender `shouldReturn` 2
      getsGame (Map.member eid . (.entities.enemies)) `shouldReturn` True
      getEncounterDeck `shouldReturn` []

    it "gives each Harad enemy Archery 2 until the end of the round" . crossRoadsTest $ \pid -> do
      setUp pid
      _ <- stageEnemy Enemies.captainOfHarad
      earlier <- getArcheryTotal
      reveal [Treacheries.arrowsThickInTheAir, Locations.ithilienWoods]
      getArcheryTotal `shouldReturn` (earlier + 2)

  -- \* Resolution

  describe "the campaign card's resolution" do
    it "adds the Brace of Coneys a player shuffled in to the pool and records the enemies under The Black Gate" . campaignTest $ \pid -> do
      setUpChoosing (Just "$label.journeyToTheCrossRoads.braceOfConeys") [Locations.hillsOfIthilien] pid
      replicateM_ 2 (stageEnemy Enemies.oliphaunt)
      _ <- stageEnemy Enemies.haradrimArcher
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      cr <- crossRoads
      run $ PlaceLocation cr ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget cr) Progress 9
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      l.pool `shouldSatisfy` elem (PoolCard (toCardCode Events.braceOfConeys) (Just 0) Nothing)
      Map.lookup HaradEnemies l.counts
        `shouldBe` Just (Map.fromList [(toCardCode Enemies.oliphaunt, 2), (toCardCode Enemies.haradrimArcher, 1)])

    it "earns no Brace of Coneys for a player who took the resources" . campaignTest $ \pid -> do
      setUpChoosing (Just "$label.journeyToTheCrossRoads.resources") [Locations.hillsOfIthilien] pid
      cr <- crossRoads
      run $ PlaceLocation cr ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget cr) Progress 9
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      l.pool `shouldSatisfy` notElem (PoolCard (toCardCode Events.braceOfConeys) (Just 0) Nothing)
      Map.lookup HaradEnemies l.counts `shouldBe` Nothing
