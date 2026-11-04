module LordOfTheRings.TheLandOfShadow.ShelobsLairSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Campaign
import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, shelobsLair)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

lairTest :: (PlayerId -> GameT ()) -> IO ()
lairTest = gameTestIn shelobsLair testDeck

-- | A campaign log with Journey to the Cross-roads just won.
lordOfTheRings :: CampaignLog
lordOfTheRings =
  (newCampaignLog "the-lord-of-the-rings")
    { campaignLogHeroes = [["01012", "01007", "01002"]]
    , campaignLogCompleted =
        [ CompletedScenario sid 100
        | sid <-
            [ "a-shadow-of-the-past"
            , "a-knife-in-the-dark"
            , "flight-to-the-ford"
            , "the-ring-goes-south"
            , "journey-in-the-dark"
            , "breaking-of-the-fellowship"
            , "the-uruk-hai"
            , "helms-deep"
            , "the-road-to-isengard"
            , "the-passage-of-the-marshes"
            , "journey-to-the-cross-roads"
            ]
        ]
    }

campaignTest :: (PlayerId -> GameT ()) -> IO ()
campaignTest = campaignTestIn shelobsLair lordOfTheRings testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup, the Ring-bearer chosen and stage 1A clicked.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseRingBearer pid (toCardCode Heroes.frodoBagginsTheLandOfShadow)
  chooseLabel "$label.keepHand" pid
  _ <- setEncounterDeck [Enemies.morgulOrc, Treacheries.manyOpenings, Locations.denOfNight]
  chooseOptionMatching "flip the quest" pid (const True)

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

placementOfLocation :: LocationId -> GameT Placement
placementOfLocation lid = (.placement) <$> getLocation lid

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

progressOn :: LocationId -> GameT Int
progressOn lid = (.progress) <$> getLocation lid

smeagol :: GameT (Maybe AllyAttrs)
smeagol = getsGame (fmap toAttrs . find ((== toCardCode Objectives.smeagolTheLandOfShadow) . toCardCode) . toList . (.entities.allies))

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs

shelobResources :: GameT Int
shelobResources = (.resources) <$> (getEnemy =<< enemyOf Enemies.shelob)

questionKey :: PlayerId -> GameT (Maybe Text)
questionKey pid = fmap (\case QuestionLabel l _ _ _ -> l; _ -> "") <$> getQuestion pid

frodo :: GameT Hero
frodo = getHeroWithCode (toCardCode Heroes.frodoBagginsTheLandOfShadow)

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Stage 1 left by its Forced effect, stage 2's When Revealed resolved.
toStage2 :: PlayerId -> GameT ()
toStage2 pid = do
  run . AddToVictoryDisplay . LocationTarget =<< locationOf Locations.smeagolsSecretWay
  run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
  resolveWhenRevealed pid

-- | Stage 2 defeated too, stage 3's When Revealed resolved.
toStage3 :: PlayerId -> GameT ()
toStage3 pid = do
  toStage2 pid
  -- no more than 4 progress a round goes on it
  run . CompleteQuest . (.id) =<< getActiveQuest
  resolveWhenRevealed pid

spec :: Spec
spec = describe "Shelob's Lair" do
  it "is a scenario the game can be dealt" . lairTest $ \_ ->
    isJust (lookupScenario "shelobs-lair") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "builds the encounter deck from Shelob's Lair, Gollum and Morgul Nazgûl, with The Great Spider and four cards set aside" . lairTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 27
      all ((`elem` map Just [ShelobsLair, GollumSaga, NazgulSaga]) . cdEncounterSet . toCardDef) deck `shouldBe` True
      map (`copies` deck) [Enemies.morgulOrc, Locations.deepDarkPassage, Objectives.aHeavyBurden, Enemies.shelob, Locations.cleftOfCirithUngol]
        `shouldBe` [4, 3, 0, 0, 0]
      aside <- scenarioSetAside <$> scenarioAttrs
      length aside `shouldBe` 14
      length (filter ((== Just TheGreatSpider) . cdEncounterSet . toCardDef) aside) `shouldBe` 10
      map toCardCode . scenarioQuestDeck <$> scenarioAttrs `shouldReturn` map toCardCode [Quests.theSpidersPass, Quests.gollumsBetrayal]

    it "puts Sméagol under the first player, makes Morgul Vale active and stages Sméagol's Secret Way" . lairTest $ \pid -> do
      setUp pid
      s <- fromJustNote "no Sméagol" <$> smeagol
      s.placement `shouldBe` InPlayArea pid
      (placementOfLocation =<< locationOf Locations.morgulValeShelobsLair) `shouldReturn` ActiveLocation
      (placementOfLocation =<< locationOf Locations.smeagolsSecretWay) `shouldReturn` StagingArea
      activeQuestCode `shouldReturn` toCardCode Quests.theStairsOfCirithUngol

    it "attaches A Heavy Burden to the Ring-bearer in campaign mode" . campaignTest $ \pid -> do
      setUp pid
      f <- frodo
      burdens <- getsGame (filter ((== toCardCode Objectives.aHeavyBurden) . toCardCode) . toList . (.entities.objectives))
      map (objectivePlacement . toAttrs) burdens `shouldBe` [AttachedTo (toTarget f.id)]

    it "attaches only the Campaign Pool's A Heavy Burden when the pool has one" . campaignTestIn shelobsLair (updateLog (AddToPool (PoolCard (toCardCode Objectives.aHeavyBurden) Nothing Nothing)) lordOfTheRings) testDeck $ \pid -> do
      setUp pid
      f <- frodo
      burdens <- getsGame (filter ((== toCardCode Objectives.aHeavyBurden) . toCardCode) . toList . (.entities.objectives))
      map (objectivePlacement . toAttrs) burdens `shouldBe` [AttachedTo (toTarget f.id)]

  -- \* The stages

  describe "The Stairs of Cirith Ungol" do
    it "keeps progress off Underground locations" . lairTest $ \pid -> do
      setUp pid
      den <- stageLocation Locations.denOfNight
      getModifiers (LocationTarget den) >>= (`shouldSatisfy` elem (ProgressLimit 0))

    it "advances at the end of the quest phase once Sméagol's Secret Way is in the victory display, adding Shelob and The Great Spider" . lairTest $ \pid -> do
      setUp pid
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      activeQuestCode `shouldReturn` toCardCode Quests.theStairsOfCirithUngol
      toStage2 pid
      activeQuestCode `shouldReturn` toCardCode Quests.theSpidersPass
      shelob <- enemyOf Enemies.shelob
      placementOfEnemy shelob `shouldReturn` StagingArea
      deck <- getEncounterDeck
      copies Treacheries.rageAndHunger deck `shouldBe` 3
      length . filter ((== Just TheGreatSpider) . cdEncounterSet . toCardDef) . scenarioSetAside <$> scenarioAttrs `shouldReturn` 0

    it "discards 3 random cards from each hand to discard each Nazgûl enemy in the staging area as Sméagol's Secret Way is explored" . lairTest $ \pid -> do
      setUp pid
      run . AddToStaging =<< genEncounterCard Enemies.morgulWraith
      hand <- length . (.hand) <$> getPlayer pid
      way <- locationOf Locations.smeagolsSecretWay
      run $ PlaceLocation way ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget way) Progress 2
      -- the Secret Way's own Forced effect, then the stage's Response
      chooseOptionMatching "exhaust a character" pid \case
        TargetLabel {} -> True
        _ -> False
      chooseAbility pid
      getsGame (map toCardCode . toList . (.entities.enemies)) >>= (`shouldSatisfy` notElem (toCardCode Enemies.morgulWraith))
      length . (.hand) <$> getPlayer pid `shouldReturn` (hand - 3)

  describe "The Spider's Pass" do
    it "keeps Shelob in the staging area, Sméagol from combat, and progress on it to 4 a round" . lairTest $ \pid -> do
      setUp pid
      toStage2 pid
      shelob <- enemyOf Enemies.shelob
      getModifiers (EnemyTarget shelob) >>= (`shouldSatisfy` elem CannotLeaveStagingArea)
      s <- fromJustNote "no Sméagol" <$> smeagol
      getModifiers (toTarget s.id) >>= (`shouldSatisfy` elem CannotAttackOrDefend)
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 6
      (.progress) <$> getActiveQuest `shouldReturn` 4

    it "lets each player put only 1 ally into play each round" . lairTest $ \pid -> do
      setUp pid
      toStage2 pid
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      card <- putCardIntoHand pid Allies.guardOfTheCitadel
      run $ PutAllyIntoPlay pid card.id
      allies <- getsGame (filter ((== toCardCode Allies.guardOfTheCitadel) . toCardCode) . toList . (.entities.allies))
      length allies `shouldBe` 1

  describe "Gollum's Betrayal" do
    it "stages Cleft of Cirith Ungol, turns Sméagol to Gollum, heals Shelob and places 1 resource on her for each player" . lairTest $ \pid -> do
      setUp pid
      toStage2 pid
      shelob <- enemyOf Enemies.shelob
      run $ DealDamage GameSource (EnemyTarget shelob) 3
      run . CompleteQuest . (.id) =<< getActiveQuest
      resolveWhenRevealed pid
      activeQuestCode `shouldReturn` toCardCode Quests.gollumsBetrayal
      (placementOfLocation =<< locationOf Locations.cleftOfCirithUngol) `shouldReturn` StagingArea
      smeagol >>= (`shouldSatisfy` isNothing)
      gollum <- enemyOf Enemies.gollumTheLandOfShadow
      placementOfEnemy gollum `shouldReturn` EngagedWith pid
      getDamage (EnemyTarget shelob) `shouldReturn` 0
      shelobResources `shouldReturn` 1
      getEnemyThreat shelob `shouldReturn` 5

    it "cancels damage dealt to Shelob for 1 of her resources" . lairTest $ \pid -> do
      setUp pid
      toStage3 pid
      shelob <- enemyOf Enemies.shelob
      run $ DealDamage GameSource (EnemyTarget shelob) 4
      getDamage (EnemyTarget shelob) `shouldReturn` 0
      shelobResources `shouldReturn` 0
      run $ DealDamage GameSource (EnemyTarget shelob) 4
      getDamage (EnemyTarget shelob) `shouldReturn` 4

    it "adds a defeated Gollum to the victory display without turning him over" . lairTest $ \pid -> do
      setUp pid
      toStage3 pid
      gollum <- enemyOf Enemies.gollumTheLandOfShadow
      run $ DealDamage GameSource (EnemyTarget gollum) 5
      smeagol >>= (`shouldSatisfy` isNothing)
      victoryCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.gollumTheLandOfShadow))

    it "is won once Shelob and Gollum are in the victory display, earning A Heavy Burden" . campaignTest $ \pid -> do
      setUp pid
      toStage3 pid
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 8
      getsGame (.outcome) `shouldReturn` Nothing
      run . AddToVictoryDisplay . EnemyTarget =<< enemyOf Enemies.gollumTheLandOfShadow
      getsGame (.outcome) `shouldReturn` Nothing
      run . AddToVictoryDisplay . EnemyTarget =<< enemyOf Enemies.shelob
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      filter ((== toCardCode Objectives.aHeavyBurden) . (.code)) l.pool `shouldBe` [PoolCard (toCardCode Objectives.aHeavyBurden) Nothing Nothing]

  -- \* The encounter cards

  describe "Shelob" do
    it "gets +1 threat for each resource on her and makes the first player choose at the start of the quest phase" . lairTest $ \pid -> do
      setUp pid
      toStage2 pid
      shelob <- enemyOf Enemies.shelob
      getEnemyThreat shelob `shouldReturn` 4
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      chooseLabel "$label.shelob.resource" pid
      shelobResources `shouldReturn` 1
      getEnemyThreat shelob `shouldReturn` 5

    it "loses Indestructible, gets +2 attack and engages the first player once Cleft of Cirith Ungol is explored" . lairTest $ \pid -> do
      setUp pid
      toStage3 pid
      shelob <- enemyOf Enemies.shelob
      getModifiers (EnemyTarget shelob) >>= (`shouldSatisfy` notElem (LoseKeyword Indestructible))
      cleft <- locationOf Locations.cleftOfCirithUngol
      run $ PlaceLocation cleft ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget cleft) Progress 4
      victoryCodes >>= (`shouldSatisfy` elem (toCardCode Locations.cleftOfCirithUngol))
      mods <- getModifiers (EnemyTarget shelob)
      mods `shouldSatisfy` elem (LoseKeyword Indestructible)
      mods `shouldSatisfy` elem (AttackModifier 2)
      placementOfEnemy shelob `shouldReturn` EngagedWith pid

  describe "Cleft of Cirith Ungol and Den of Night" do
    it "cannot be traveled to without Shelob in play" . lairTest $ \pid -> do
      setUp pid
      den <- stageLocation Locations.denOfNight
      getModifiers (LocationTarget den) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      toStage2 pid
      getModifiers (LocationTarget den) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo den
      shelobResources `shouldReturn` 1

  describe "Deep Dark Passage" do
    it "reduces the progress placed on each location in the staging area by 1" . lairTest $ \pid -> do
      setUp pid
      way <- locationOf Locations.smeagolsSecretWay
      _ <- stageLocation Locations.deepDarkPassage
      run $ PlaceTokens GameSource (LocationTarget way) Progress 1
      progressOn way `shouldReturn` 0

  describe "The Under-way" do
    it "gives each Orc enemy -5 engagement cost and a shadow card as it engages" . lairTest $ \pid -> do
      setUp pid
      _ <- stageLocation Locations.theUnderWay
      run . AddToStaging =<< genEncounterCard Enemies.morgulOrc
      orc <- enemyOf Enemies.morgulOrc
      getModifiers (EnemyTarget orc) >>= (`shouldSatisfy` elem (EngagementCostModifier (-5)))
      _ <- setEncounterDeck [Treacheries.manyOpenings]
      run $ EngageEnemy pid orc
      length . (.shadowCards) <$> getEnemy orc `shouldReturn` 1

  describe "Orc of Cirith Ungol" do
    it "discards 1 random card from each player's hand, as the revealer chooses" . lairTest $ \pid -> do
      setUp pid
      hand <- length . (.hand) <$> getPlayer pid
      reveal [Enemies.orcOfCirithUngol, Locations.denOfNight]
      chooseLabel "$label.orcOfCirithUngol.discard" pid
      length . (.hand) <$> getPlayer pid `shouldReturn` (hand - 1)

  describe "Lurking Malice" do
    it "places 1 resource on Shelob, as the revealer chooses" . lairTest $ \pid -> do
      setUp pid
      toStage2 pid
      reveal [Treacheries.lurkingMalice, Locations.denOfNight, Locations.denOfNight]
      chooseLabel "$label.lurkingMalice.resource" pid
      shelobResources `shouldReturn` 1

  describe "Rage and Hunger" do
    it "gives Shelob +2 attack and has her attack each player" . lairTest $ \pid -> do
      setUp pid
      toStage2 pid
      reveal [Treacheries.rageAndHunger, Locations.denOfNight, Locations.denOfNight]
      shelob <- enemyOf Enemies.shelob
      getModifiers (EnemyTarget shelob) >>= (`shouldSatisfy` elem (AttackModifier 2))
      passActions pid
      questionKey pid >>= (`shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`))

  describe "Shelob's Poison" do
    it "attaches to a hero of the revealer, exhausting and blanking it" . lairTest $ \pid -> do
      setUp pid
      f <- frodo
      reveal [Treacheries.shelobsPoison, Locations.denOfNight]
      chooseTarget pid f.id
      getModifiers (toTarget f.id) >>= (`shouldSatisfy` \ms -> Blank `elem` ms && CannotReady `elem` ms)
      heroExhausted . toAttrs <$> frodo `shouldReturn` True

  describe "Many Openings" do
    it "gives each location in the staging area +1 threat" . lairTest $ \pid -> do
      setUp pid
      way <- locationOf Locations.smeagolsSecretWay
      reveal [Treacheries.manyOpenings, Locations.denOfNight]
      getModifiers (LocationTarget way) >>= (`shouldSatisfy` elem (ThreatModifier 1))
      length <$> getEncounterDeck `shouldReturn` 1

    it "reveals another card with fewer locations in the staging area than players" . lairTest $ \pid -> do
      setUp pid
      run . Discard . LocationTarget =<< locationOf Locations.smeagolsSecretWay
      reveal [Treacheries.manyOpenings, Locations.denOfNight]
      getEncounterDeck `shouldReturn` []
      (placementOfLocation =<< locationOf Locations.denOfNight) `shouldReturn` StagingArea
