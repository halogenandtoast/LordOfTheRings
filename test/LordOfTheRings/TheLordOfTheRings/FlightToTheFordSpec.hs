module LordOfTheRings.TheLordOfTheRings.FlightToTheFordSpec (spec) where

import LordOfTheRings.Campaign (CampaignLog (..), newCampaignLog)
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Fellowship (theOneRingExhausted)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, EncounterPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (flightToTheFord, lookupScenario)
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (getRingBearersLife)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), burdenDeckL, encounterDeckL, modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (..), mkAfter)
import TestImport

fordTest :: (PlayerId -> GameT ()) -> IO ()
fordTest = gameTestIn flightToTheFord testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

campaignTest :: (PlayerId -> GameT ()) -> IO ()
campaignTest = campaignTestIn flightToTheFord lotr testDeck

-- | Setup, The Black Riders' Frodo Baggins taken should the game offer several
-- Ring-bearers, the starting hand kept and stage 1A clicked.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  getQuestion pid >>= \case
    Just (QuestionLabel l _ _ _) | "$label.saga.chooseFellowshipHero" `isPrefixOf` l -> void $ answerQuestion pid 0
    _ -> pure ()
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

frodo :: GameT Target
frodo = hero (toCardCode Heroes.frodoBagginsTheBlackRiders)

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

enemiesIn :: GameT [CardCode]
enemiesIn = getsGame (map toCardCode . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationAttrs :: LocationId -> GameT LocationAttrs
locationAttrs lid = getsGame (toAttrs . fromJustNote "no such location" . lookup lid . (.entities.locations))

treacheriesOn :: Target -> GameT [CardCode]
treacheriesOn host = getsGame (map toCardCode . filter ((== AttachedTo host) . (.placement)) . toList . (.entities.treacheries))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = do
  g <- (phaseL .~ p) <$> getGame
  putGame g
  putGame =<< preloadModifiers g

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

-- | Reveal the first of the cards, the rest beneath it in the encounter deck.
reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

setBurdenDeck :: [CardDef] -> GameT ()
setBurdenDeck defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (burdenDeckL .~ cards)) =<< getGame

burdenDeckCodes :: GameT [CardCode]
burdenDeckCodes = map toCardCode . (.scenarioBurdenDeck) <$> scenarioAttrs

setLife :: Int -> GameT ()
setLife n = run $ ScenarioSpecific "flightToTheFord.setLife" (toJSON n)

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

burdens :: [CardDef]
burdens = [Treacheries.weightOfTheRing, Treacheries.fearOfDiscovery, Treacheries.eatenAlive, Treacheries.panickedFlightToTheFord, Treacheries.overcomeByTerror]

spec :: Spec
spec = describe "Flight to the Ford" do
  it "is a scenario the game can be dealt" . fordTest $ \_ ->
    isJust (lookupScenario "flight-to-the-ford") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "builds the Burden deck, wounds the Ring-bearer, sets his life at 15 and stages The Last Bridge and a Fell Rider" . fordTest $ \pid -> do
      setUp pid
      attrs <- scenarioAttrs
      sort (map toCardCode attrs.scenarioBurdenDeck) `shouldBe` sort (map toCardCode burdens)
      sort (map toCardCode attrs.scenarioSetAside) `shouldBe` sort (map toCardCode [Locations.fordOfBruinen, Enemies.theWitchKing])
      all ((`elem` map Just [FlightToTheFord, Hunted, TheBlackRiders, TheNazgul]) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      map toCardCode attrs.questDeck `shouldBe` [toCardCode Quests.raceToRivendell]
      activeQuestCode `shouldReturn` toCardCode Quests.pursuedByTheEnemy
      f <- frodo
      wound <- getsGame (find ((== toCardCode Objectives.anEvilWound) . toCardCode) . toList . (.entities.objectives))
      fmap (.placement) wound `shouldBe` Just (AttachedTo f)
      getRingBearersLife `shouldReturn` 15
      bridge <- locationAttrs =<< locationOf Locations.theLastBridge
      bridge.placement `shouldBe` StagingArea
      rider <- enemyOf Enemies.fellRider
      placementOfEnemy rider `shouldReturn` StagingArea
      copies Enemies.fellRider attrs.encounterDeck `shouldBe` 3

    it "builds the encounter deck in normal and easy mode" . fordTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      let counted = [Enemies.fellRider, Locations.theLastBridge, Locations.ettenmoors, Locations.theOldRoad, Treacheries.strickenDumb, Treacheries.theNineAreAbroad, Enemies.ringwraith]
      map (`copies` deck) counted `shouldBe` [4, 1, 3, 3, 3, 2, 4]
      copies Treacheries.weightOfTheRing deck `shouldBe` 0

    it "keeps fewer copies in easy mode" . gameTestIn (overAttrs (modeL .~ EasyMode) flightToTheFord) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Treacheries.theNineAreAbroad, Enemies.ringwraith] `shouldBe` [1, 2]

  -- \* The Ring-bearer's life

  describe "An Evil Wound" do
    it "turns damage to the Ring-bearer into lost life, and he cannot be healed" . fordTest $ \pid -> do
      setUp pid
      f <- frodo
      getModifiers f >>= (`shouldSatisfy` elem CannotBeHealed)
      run $ DealDamage GameSource f 2
      useForcedAbilities pid
      getCharacterDamage f `shouldReturn` 0
      getRingBearersLife `shouldReturn` 13

    it "loses the game as the life reaches 0" . fordTest $ \pid -> do
      setUp pid
      setLife 2
      f <- frodo
      run $ DealDamage GameSource f 3
      useForcedAbilities pid
      getsGame (.outcome) `shouldReturn` Just Lost

  -- \* The stages

  describe "Pursued by the Enemy" do
    it "cannot be defeated until The Last Bridge is in the victory display, and reduces the life as the round ends" . fordTest $ \pid -> do
      setUp pid
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 15
      activeQuestCode `shouldReturn` toCardCode Quests.pursuedByTheEnemy
      run $ CheckWindows [mkAfter RoundEnds]
      useForcedAbilities pid
      getRingBearersLife `shouldReturn` 14
      bridge <- locationOf Locations.theLastBridge
      run $ PlaceLocation bridge ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget bridge) Progress 4
      resolveRevealed
      activeQuestCode `shouldReturn` toCardCode Quests.raceToRivendell

  describe "Race to Rivendell" do
    it "adds Ford of Bruinen and The Witch-king, gets +15 quest points while a Nazgûl is in play, and wins once defeated" . fordTest $ \pid -> do
      setUp pid
      _ <- putQuestStage Quests.raceToRivendell
      q <- getActiveQuest
      run $ Flip (toTarget q)
      _ <- locationOf Locations.fordOfBruinen
      wk <- enemyOf Enemies.theWitchKing
      getModifiers (toTarget q) >>= (`shouldSatisfy` elem (QuestPointModifier 15))
      run $ Discard (EnemyTarget wk)
      for_ [Enemies.fellRider] \_ -> enemyOf Enemies.fellRider >>= run . Discard . EnemyTarget
      getModifiers (toTarget q) >>= (`shouldSatisfy` notElem (QuestPointModifier 15))
      run $ PlaceTokens GameSource (toTarget q) Progress 15
      getsGame (.outcome) `shouldReturn` Just Won

  -- \* The campaign card

  describe "the campaign card" do
    it "shuffles the top burden into the encounter deck to cancel an enemy's attack" . campaignTest $ \pid -> do
      setUp pid
      setBurdenDeck [Treacheries.panickedFlightToTheFord, Treacheries.eatenAlive]
      _ <- setEncounterDeck [Treacheries.strickenDumb]
      rider <- engaged pid Enemies.ringwraith
      run $ EnemyAttack pid rider
      chooseAbility pid
      burdenDeckCodes `shouldReturn` [toCardCode Treacheries.eatenAlive]
      deck <- getEncounterDeck
      copies Treacheries.panickedFlightToTheFord deck `shouldBe` 1
      -- the attack is canceled: no defender is asked for
      getQuestion pid >>= \case
        Just (QuestionLabel l _ _ _) -> ("$label.declareDefender" `isPrefixOf` l) `shouldBe` False
        _ -> pure ()

    it "is not offered outside campaign mode" . fordTest $ \pid -> do
      setUp pid
      rider <- engaged pid Enemies.ringwraith
      run $ EnemyAttack pid rider
      offersAbilityOf pid ScenarioTarget `shouldReturn` False

    it "adds each burden in play, in the encounter deck and in the discard pile to the Campaign Pool as the players win" . campaignTest $ \pid -> do
      setUp pid
      setBurdenDeck [Treacheries.overcomeByTerror]
      f <- frodo
      weight <- genEncounterCard Treacheries.weightOfTheRing
      run $ RevealCard weight
      resolveRevealed
      treacheriesOn f `shouldReturn` [toCardCode Treacheries.weightOfTheRing]
      fear <- genEncounterCard Treacheries.fearOfDiscovery
      run $ AddToEncounterDiscard [fear]
      eaten <- genEncounterCard Treacheries.eatenAlive
      putGame . (scenarioL %~ overAttrs (encounterDeckL %~ (eaten :))) =<< getGame
      _ <- putQuestStage Quests.raceToRivendell
      q <- getActiveQuest
      run $ Flip (toTarget q)
      for_ [Enemies.theWitchKing, Enemies.fellRider] \d -> enemyOf d >>= run . Discard . EnemyTarget
      run $ PlaceTokens GameSource (toTarget q) Progress 15
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      sort (map (.code) l.pool) `shouldBe` sort (map toCardCode [Treacheries.weightOfTheRing, Treacheries.fearOfDiscovery, Treacheries.eatenAlive])

  -- \* Locations

  describe "The Last Bridge" do
    it "has X threat, reveals the top burden to travel there, and cannot be traveled to with the Burden deck empty" . fordTest $ \pid -> do
      setUp pid
      bridge <- locationOf Locations.theLastBridge
      getModifiers (LocationTarget bridge) >>= (`shouldSatisfy` elem (ThreatModifier 1))
      setBurdenDeck [Treacheries.panickedFlightToTheFord]
      p <- threatOf pid
      _ <- setEncounterDeck [Treacheries.theEnemyIsUponUs, Treacheries.theEnemyIsUponUs, Treacheries.theEnemyIsUponUs]
      run $ TravelTo bridge
      resolveRevealed
      threatOf pid `shouldReturn` (p + 3)
      burdenDeckCodes `shouldReturn` []
      run $ PlaceLocation bridge StagingArea
      getModifiers (LocationTarget bridge) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  describe "Ford of Bruinen" do
    it "discards each engaged enemy after it leaves play explored" . fordTest $ \pid -> do
      setUp pid
      rider <- enemyOf Enemies.fellRider
      run $ EngageEnemy pid rider
      ford <- stageLocation Locations.fordOfBruinen
      run $ PlaceLocation ford ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget ford) Progress 6
      chooseAbilityOf pid (LocationTarget ford)
      enemiesIn `shouldReturn` []

  describe "The Troll's Camp" do
    it "gives each hero +1 willpower, attack and defense while active" . fordTest $ \_ -> do
      camp <- stageLocation Locations.theTrollsCamp
      run $ PlaceLocation camp ActiveLocation
      theodred <- hero "01002"
      getModifiers theodred >>= (`shouldSatisfy` \ms -> all (`elem` ms) [WillpowerModifier 1, AttackModifier 1, DefenseModifier 1])

  describe "Ettenmoors" do
    it "deals Nazgûl no shadow cards in the combat phase while active, and reduces the life as progress is placed on it" . fordTest $ \pid -> do
      setUp pid
      moors <- stageLocation Locations.ettenmoors
      run $ PlaceLocation moors ActiveLocation
      rider <- enemyOf Enemies.fellRider
      inPhase CombatPhase
      getModifiers (EnemyTarget rider) >>= (`shouldSatisfy` elem (ShadowCardsDealt 0))
      inPhase QuestPhase
      getModifiers (EnemyTarget rider) >>= (`shouldSatisfy` notElem (ShadowCardsDealt 0))
      run $ PlaceTokens GameSource (LocationTarget moors) Progress 1
      useForcedAbilities pid
      getRingBearersLife `shouldReturn` 14

  describe "The Old Road" do
    it "in campaign mode places the top burden on the encounter deck to place 4 progress on itself" . campaignTest $ \pid -> do
      setUp pid
      setBurdenDeck [Treacheries.eatenAlive, Treacheries.panickedFlightToTheFord]
      road <- stageLocation Locations.theOldRoad
      run $ ActionWindow pid
      chooseAbilityOf pid (LocationTarget road)
      -- its 4 quest points filled, it is explored
      getsGame (any ((== toCardCode Locations.theOldRoad) . toCardCode) . toList . (.entities.locations)) `shouldReturn` False
      map toCardCode . take 1 <$> getEncounterDeck `shouldReturn` [toCardCode Treacheries.eatenAlive]
      burdenDeckCodes `shouldReturn` [toCardCode Treacheries.panickedFlightToTheFord]

    it "offers nothing outside campaign mode" . fordTest $ \pid -> do
      setUp pid
      road <- stageLocation Locations.theOldRoad
      run $ ActionWindow pid
      offersAbilityOf pid (LocationTarget road) `shouldReturn` False

  -- \* Enemies

  describe "Fell Rider" do
    it "has the first player reduce the life or engage it as the encounter phase begins" . fordTest $ \pid -> do
      setUp pid
      rider <- enemyOf Enemies.fellRider
      run $ CheckWindows [Window After (PhaseBegins EncounterPhase)]
      useForcedAbilities pid
      chooseLabel "$label.fellRider.reduceLife" pid
      getRingBearersLife `shouldReturn` 14
      run $ CheckWindows [Window After (PhaseBegins EncounterPhase)]
      useForcedAbilities pid
      chooseLabel "$label.fellRider.engage" pid
      placementOfEnemy rider `shouldReturn` EngagedWith pid

  -- \* Treacheries

  describe "Stricken Dumb" do
    it "shuffles the top burden into the encounter deck and reveals another, or raises threat and ends the quest phase" . fordTest $ \pid -> do
      setUp pid
      setBurdenDeck [Treacheries.panickedFlightToTheFord]
      reveal [Treacheries.strickenDumb, Treacheries.theEnemyIsUponUs, Treacheries.theEnemyIsUponUs]
      chooseLabel "$label.strickenDumb.burden" pid
      resolveRevealed
      burdenDeckCodes `shouldReturn` []
      p <- threatOf pid
      -- the Burden deck empty, only the threat is left
      inPhase QuestPhase
      reveal [Treacheries.strickenDumb]
      threatOf pid `shouldReturn` (p + 3)

  describe "Pain Assailed Him" do
    it "reduces the life by 1 and the Ring-bearer's stats to 0 until the end of the round" . fordTest $ \pid -> do
      setUp pid
      reveal [Treacheries.painAssailedHim]
      getRingBearersLife `shouldReturn` 14
      f <- frodo
      getModifiers f >>= (`shouldSatisfy` \ms -> all (`elem` ms) [WillpowerReducedToZero, DoesNotCountAttack, DoesNotCountDefense])

  describe "The Nine are Abroad" do
    it "blanks each character's text box until the end of the round, gaining surge with a Nazgûl in the staging area" . fordTest $ \pid -> do
      setUp pid
      reveal [Treacheries.theNineAreAbroad, Treacheries.painAssailedHim]
      theodred <- hero "01002"
      getModifiers theodred >>= (`shouldSatisfy` elem Blank)
      -- the Fell Rider in the staging area: surge reveals Pain Assailed Him
      getRingBearersLife `shouldReturn` 14

  describe "The Enemy is Upon Us!" do
    it "has each engaged enemy attack, gaining surge when none does" . fordTest $ \pid -> do
      setUp pid
      reveal [Treacheries.theEnemyIsUponUs, Treacheries.painAssailedHim]
      getRingBearersLife `shouldReturn` 14

  -- \* Burdens

  describe "Weight of the Ring" do
    it "attaches to the Ring-bearer and, as the quest phase begins, exhausts The One Ring or reveals another card" . fordTest $ \pid -> do
      setUp pid
      reveal [Treacheries.weightOfTheRing, Treacheries.theEnemyIsUponUs, Treacheries.theEnemyIsUponUs]
      f <- frodo
      treacheriesOn f `shouldReturn` [toCardCode Treacheries.weightOfTheRing]
      run $ CheckWindows [Window After (PhaseBegins QuestPhase)]
      useForcedAbilities pid
      chooseLabel "$label.weightOfTheRing.exhaust" pid
      theOneRingExhausted `shouldReturn` True

  describe "Fear of Discovery" do
    it "exhausts the Ring-bearer or reveals another card" . fordTest $ \pid -> do
      setUp pid
      reveal [Treacheries.fearOfDiscovery, Treacheries.theEnemyIsUponUs, Treacheries.theEnemyIsUponUs]
      chooseLabel "$label.fearOfDiscovery.exhaust" pid
      resolveRevealed
      heroExhausted . toAttrs <$> getHeroWithCode (toCardCode Heroes.frodoBagginsTheBlackRiders) `shouldReturn` True

  describe "Eaten Alive!" do
    it "deals X damage divided among the first player's heroes" . fordTest $ \pid -> do
      setUp pid
      theodred <- hero "01002"
      reveal [Treacheries.eatenAlive, Treacheries.theEnemyIsUponUs, Treacheries.theEnemyIsUponUs]
      chooseTarget pid theodred
      resolveRevealed
      getCharacterDamage theodred `shouldReturn` 1

  describe "Overcome by Terror" do
    it "exhausts The One Ring, or discards 2 cards at random once it is exhausted" . fordTest $ \pid -> do
      setUp pid
      reveal [Treacheries.overcomeByTerror, Treacheries.overcomeByTerror, Treacheries.theEnemyIsUponUs, Treacheries.theEnemyIsUponUs]
      resolveRevealed
      theOneRingExhausted `shouldReturn` True
      length . (.hand) <$> getPlayer pid `shouldReturn` 4

  describe "Panicked" do
    it "raises the first player's threat by 3 and passes the first player token" . twoPlayerTestIn flightToTheFord testDeck $ \one two -> do
      p <- threatOf one
      reveal [Treacheries.panickedFlightToTheFord, Treacheries.theEnemyIsUponUs, Treacheries.theEnemyIsUponUs]
      resolveRevealed
      threatOf one `shouldReturn` (p + 3)
      getLead `shouldReturn` two
