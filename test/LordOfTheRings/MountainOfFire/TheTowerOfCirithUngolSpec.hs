module LordOfTheRings.MountainOfFire.TheTowerOfCirithUngolSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (CampaignLog (..), CompletedScenario (..), PoolCard (..), newCampaignLog)
import LordOfTheRings.Card (cdCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Helpers.Staging (getEngagementCost, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theTowerOfCirithUngol)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Sphere (Sphere (Fellowship))
import LordOfTheRings.Trait (Trait (RingBearer))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

towerTest :: (PlayerId -> GameT ()) -> IO ()
towerTest = gameTestIn theTowerOfCirithUngol testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

beravor :: CardCode
beravor = "01012"

frodo :: CardCode
frodo = cdCardCode Heroes.frodoBagginsTheMountainOfFire

-- | Setup, its stage 1A clicked, Last of the Company and The One Ring given to Beravor.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseRingBearer pid frodo
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)
  chooseTarget pid =<< hero beravor

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

heroAttrs :: CardCode -> GameT HeroAttrs
heroAttrs code = toAttrs <$> getHeroWithCode code

-- | The card each objective in play is attached to, by code.
objectivesOn :: Target -> GameT [CardCode]
objectivesOn t = getsGame (map toCardCode . filter ((== AttachedTo t) . (.placement)) . toList . (.entities.objectives))


locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

questProgress :: Int -> GameT ()
questProgress n = do
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Progress n

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

offersTarget :: PlayerId -> Target -> GameT Bool
offersTarget pid t = maybe False (any \case TargetLabel t' _ -> t' == t; _ -> False) . fmap questionChoices <$> getQuestion pid

offersLabel :: PlayerId -> Text -> GameT Bool
offersLabel pid l = maybe False (any \case Label l' _ -> l' == l; _ -> False) . fmap questionChoices <$> getQuestion pid

-- | Defeat The Two Watchers' enemy side, after the players traveled there.
defeatTheWatchers :: GameT ()
defeatTheWatchers = do
  run . TravelTo =<< locationOf Locations.theTwoWatchers
  eid <- enemyOf Enemies.theTwoWatchersEnemy
  run $ Destroy GameSource (EnemyTarget eid)

spec :: Spec
spec = describe "The Tower of Cirith Ungol" do
  it "is a scenario the game can be dealt" . towerTest $ \_ ->
    isJust (lookupScenario "the-tower-of-cirith-ungol") `shouldBe` True

  describe "setup" do
    it "builds the encounter deck from its three sets, its set-aside cards out of it" . towerTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.echoingPassage, Locations.torchLitHall, Enemies.gorbag, Enemies.woundedUruk, Enemies.orcOfMordor, Enemies.snagaArcher, Enemies.urukSoldierOrcsOfMordor, Treacheries.drawnToTheRing, Treacheries.deepShadows, Treacheries.evilVigilance, Treacheries.orcishHowls]
        `shouldBe` [3, 3, 1, 3, 3, 3, 3, 3, 2, 3, 3]
      length deck `shouldBe` 36
      sort . map toCardCode . scenarioSetAside <$> scenarioAttrs
        `shouldReturn` sort (map toCardCode [Enemies.shagrat, Locations.topmostChamber, Locations.theTwoWatchers, Objectives.lastOfTheCompany])

    it "places the Ring-bearer under Topmost Chamber and gives Last of the Company and The One Ring to a hero of the first player" . towerTest $ \pid -> do
      setUp pid
      chamber <- locationOf Locations.topmostChamber
      _ <- locationOf Locations.theTwoWatchers
      chamberCard <- (.cardId) . toAttrs <$> getLocation chamber
      ringBearer <- heroAttrs frodo
      ringBearer.heroGuardedBy `shouldBe` Just chamberCard
      b <- hero beravor
      sort <$> objectivesOn b `shouldReturn` sort (map toCardCode [Objectives.lastOfTheCompany, Objectives.theOneRingTheMountainOfFire])
      objectivesOn (toTarget ringBearer.heroId) `shouldReturn` []
      getModifiers b >>= (`shouldSatisfy` \ms -> GainTrait RingBearer `elem` ms && GainResourceIcon Fellowship `elem` ms)
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotLoseFirstPlayerToken)
      map toCardCode . scenarioSetAside <$> scenarioAttrs `shouldReturn` [toCardCode Enemies.shagrat]
      activeQuestCode `shouldReturn` toCardCode Quests.passTheTwoWatchers

  describe "Pass the Two Watchers" do
    it "gives non-unique locations -1 threat and keeps them in the staging area; The Two Watchers has twice the stage as threat" . towerTest $ \pid -> do
      setUp pid
      passage <- stageLocation Locations.windingStairs
      getModifiers (LocationTarget passage) >>= (`shouldSatisfy` elem CannotLeaveStagingArea)
      getLocationThreat passage `shouldReturn` 1
      watchers <- locationOf Locations.theTwoWatchers
      getLocationThreat watchers `shouldReturn` 2
      getModifiers (LocationTarget watchers) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

    it "turns The Two Watchers to its enemy side in the staging area as the players travel there, engaged with each player" . towerTest $ \pid -> do
      setUp pid
      run . TravelTo =<< locationOf Locations.theTwoWatchers
      eid <- enemyOf Enemies.theTwoWatchersEnemy
      placementOfEnemy eid `shouldReturn` StagingArea
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [EngagedWithEachPlayer AnyPlayer, ImmuneToPlayerCardEffects, AttackDamageToThreatInstead])
      getsGame (map toCardCode . toList . (.entities.locations)) >>= (`shouldSatisfy` notElem (toCardCode Locations.theTwoWatchers))

    it "raises the defending player's threat instead of dealing The Two Watchers' damage" . towerTest $ \pid -> do
      setUp pid
      run . TravelTo =<< locationOf Locations.theTwoWatchers
      eid <- enemyOf Enemies.theTwoWatchersEnemy
      b <- hero beravor
      threat <- threatOf pid
      run $ AttackDamage eid b
      getDamage b `shouldReturn` 0
      -- 5 attack against Beravor's 2 defense
      threatOf pid `shouldReturn` (threat + 3)

    it "is defeated once The Two Watchers' enemy side is, and stage 2A adds an enemy per player" . towerTest $ \pid -> do
      setUp pid
      -- the deck is shuffled first: whatever its order, the one enemy is staged
      _ <- setEncounterDeck [Locations.torchLitHall, Enemies.woundedUruk]
      defeatTheWatchers
      victoryCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.theTwoWatchersEnemy))
      activeQuestCode `shouldReturn` toCardCode Quests.fightingInTheTower
      resolveWhenRevealed pid
      enemyCodes `shouldReturn` [toCardCode Enemies.woundedUruk]

  describe "Fighting in the Tower" do
    it "keeps Topmost Chamber out of reach until 18 progress, adds Shagrat as the players travel there, frees the Ring-bearer and advances once it is explored" . towerTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.torchLitHall]
      defeatTheWatchers
      resolveWhenRevealed pid
      chamber <- locationOf Locations.topmostChamber
      getModifiers (LocationTarget chamber) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      questProgress 18
      activeQuestCode `shouldReturn` toCardCode Quests.fightingInTheTower
      getModifiers (LocationTarget chamber) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo chamber
      shagrat <- enemyOf Enemies.shagrat
      placementOfEnemy shagrat `shouldReturn` StagingArea
      _ <- setEncounterDeck [Locations.torchLitHall]
      run $ PlaceTokens GameSource (LocationTarget chamber) Progress 1
      victoryCodes >>= (`shouldSatisfy` elem (toCardCode Locations.topmostChamber))
      ringBearer <- heroAttrs frodo
      ringBearer.heroGuardedBy `shouldBe` Nothing
      ringBearer.heroOwner `shouldBe` pid
      activeQuestCode `shouldReturn` toCardCode Quests.escapeFromCirithUngol

    it "discards an enemy at the beginning of the quest phase, and its copy in the staging area with it" . towerTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.torchLitHall]
      defeatTheWatchers
      resolveWhenRevealed pid
      uruk <- stage Enemies.woundedUruk
      _ <- setEncounterDeck [Locations.echoingPassage, Enemies.woundedUruk, Enemies.gorbag]
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      getsGame (lookup uruk . (.entities.enemies)) >>= (`shouldSatisfy` isNothing)
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.gorbag]
      _ <- setEncounterDeck [Enemies.gorbag]
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.gorbag))

  describe "Escape from Cirith Ungol" do
    it "sends Last of the Company to the victory display, The One Ring to the exhausted Ring-bearer and The Two Watchers back to the staging area, then is won once the Watchers are defeated" . towerTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.torchLitHall]
      defeatTheWatchers
      resolveWhenRevealed pid
      questProgress 18
      chamber <- locationOf Locations.topmostChamber
      run $ TravelTo chamber
      run $ PlaceTokens GameSource (LocationTarget chamber) Progress 1
      resolveWhenRevealed pid
      victoryCodes >>= (`shouldSatisfy` elem (toCardCode Objectives.lastOfTheCompany))
      ringBearer <- heroAttrs frodo
      objectivesOn (toTarget ringBearer.heroId) `shouldReturn` [toCardCode Objectives.theOneRingTheMountainOfFire]
      ringBearer.heroExhausted `shouldBe` True
      watchers <- locationOf Locations.theTwoWatchers
      getLocationThreat watchers `shouldReturn` 6
      getModifiers (LocationTarget watchers) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      questProgress 12
      activeQuestCode `shouldReturn` toCardCode Quests.escapeFromCirithUngol
      getModifiers (LocationTarget watchers) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      run $ TravelTo watchers
      eid <- enemyOf Enemies.theTwoWatchersEnemy
      getEnemyThreat eid `shouldReturn` 5
      run $ Destroy GameSource (EnemyTarget eid)
      getsGame (.outcome) `shouldReturn` Just Won

  describe "the encounter cards" do
    it "has Orcish Howls damage each staged enemy and give it +1 threat per damage, surging with none" . towerTest $ \pid -> do
      setUp pid
      uruk <- stage Enemies.woundedUruk
      reveal [Treacheries.orcishHowls, Locations.echoingPassage]
      getEnemyThreat uruk `shouldReturn` (2 + 3)
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.echoingPassage]

    it "has Orcish Howls surge with no enemy in the staging area" . towerTest $ \pid -> do
      setUp pid
      reveal [Treacheries.orcishHowls, Locations.echoingPassage]
      getEncounterDeck `shouldReturn` []

    it "has Drawn to the Ring exhaust The One Ring to reveal a card, or engage the next enemy" . towerTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Treacheries.drawnToTheRing, Locations.echoingPassage, Enemies.gorbag]
      run RevealEncounterCard
      resolveRevealed
      offersLabel pid "$label.drawnToTheRing.exhaustTheOneRing" `shouldReturn` True
      chooseLabel "$label.drawnToTheRing.discardUntilEnemy" pid
      gorbag <- enemyOf Enemies.gorbag
      placementOfEnemy gorbag `shouldReturn` EngagedWith pid

    it "lets Deep Shadows discard a hand of 3 or more, else reveals" . towerTest $ \pid -> do
      setUp pid
      length . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` (>= 3))
      _ <- setEncounterDeck [Treacheries.deepShadows, Locations.echoingPassage]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.deepShadows.discardHand" pid
      length . (.hand) <$> getPlayer pid `shouldReturn` 0
      putGame . (playersL . ix pid . handL .~ []) =<< getGame
      _ <- setEncounterDeck [Treacheries.deepShadows, Locations.echoingPassage]
      run RevealEncounterCard
      resolveRevealed
      getEncounterDeck `shouldReturn` []

    it "raises the threat of Gorbag's, Uruk Soldier's and Wounded Uruk's engaged players as their cards read" . towerTest $ \pid -> do
      setUp pid
      soldier <- stage Enemies.urukSoldierOrcsOfMordor
      threat <- threatOf pid
      run $ EngageEnemy pid soldier
      threatOf pid `shouldReturn` (threat + 2)
      uruk <- stage Enemies.woundedUruk
      (.damage) <$> getEnemyAttrs uruk `shouldReturn` 2

    it "gives Shagrat -X engagement cost as progress is placed on the quest" . towerTest $ \pid -> do
      setUp pid
      shagrat <- stage Enemies.shagrat
      getModifiers (EnemyTarget shagrat) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [NotFoughtByAllies, CannotTakeNonCombatDamage])
      questProgress 3
      getEngagementCost shagrat `shouldReturn` 43

    it "gives Echoing Passage +1 threat for each enemy in the staging area" . towerTest $ \pid -> do
      setUp pid
      passage <- stageLocation Locations.echoingPassage
      _ <- stage Enemies.gorbag
      -- printed 1, +1 for Gorbag, -1 for stage 1B
      getLocationThreat passage `shouldReturn` 1

    it "has Evil Vigilance raise threat by 1 for each questing character, as chosen" . towerTest $ \pid -> do
      setUp pid
      b <- hero beravor
      run $ CommitToQuest b
      threat <- threatOf pid
      reveal [Treacheries.evilVigilance]
      offersTarget pid b `shouldReturn` True
      chooseOptionMatching "raise threat" pid \case Label l _ -> "$label.evilVigilance.threat" `isPrefixOf` l; _ -> False
      threatOf pid `shouldReturn` (threat + 1)

    it "bars travel to Winding Stairs while a player has no ready character" . towerTest $ \pid -> do
      setUp pid
      stairs <- stageLocation Locations.windingStairs
      getModifiers (LocationTarget stairs) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      for_ ["01012", "01007", "01002"] \c -> run . Exhaust =<< hero c
      getModifiers (LocationTarget stairs) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  describe "A Heavy Burden" do
    it "raises each player's threat by 1 or exhausts the Ring-bearer at the end of the planning phase" . campaignTestIn theTowerOfCirithUngol ((newCampaignLog "the-lord-of-the-rings") {campaignLogPool = [PoolCard (cdCardCode Objectives.aHeavyBurden) Nothing Nothing]}) testDeck $ \pid -> do
      setUp pid
      threat <- threatOf pid
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      chooseLabel "$label.aHeavyBurden.raiseThreat" pid
      threatOf pid `shouldReturn` (threat + 1)
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      b <- hero beravor
      chooseTarget pid b
      (.heroExhausted) <$> heroAttrs beravor `shouldReturn` True

  describe "campaign mode" do
    let pool = [PoolCard (cdCardCode Objectives.aHeavyBurden) Nothing Nothing, PoolCard (cdCardCode Attachments.bladeOfGondolin) (Just 0) Nothing]
        campaignLog = (newCampaignLog "the-lord-of-the-rings") {campaignLogPool = pool}
        campaignTest = campaignTestIn theTowerOfCirithUngol campaignLog testDeck
        withBlade pid = do
          card <- genPlayerCard Attachments.bladeOfGondolin (Just pid)
          putGame . (playersL . ix pid . deckL %~ (PlayerCard card :)) =<< getGame
        setUpCampaign pid = do
          withBlade pid
          run Setup
          chooseRingBearer pid frodo
          -- the only Item boon of the seat: set aside without a question
          chooseLabel "$label.keepHand" pid
          chooseOptionMatching "flip the quest" pid (const True)
          chooseTarget pid =<< hero beravor

    it "sets the Item boon aside and attaches A Heavy Burden to the hero with Last of the Company" . campaignTest $ \pid -> do
      setUpCampaign pid
      deck <- (.deck) <$> getPlayer pid
      any ((== toCardCode Attachments.bladeOfGondolin) . toCardCode) deck `shouldBe` False
      b <- hero beravor
      objectivesOn b >>= (`shouldSatisfy` elem (toCardCode Objectives.aHeavyBurden))
      scenarioCampaignCardFlipped <$> scenarioAttrs `shouldReturn` True

    it "attaches A Heavy Burden to the Ring-bearer and the boons to Shagrat at stage 3A, and loses them should Shagrat be in play as the scenario is won" . campaignTest $ \pid -> do
      setUpCampaign pid
      _ <- setEncounterDeck [Locations.torchLitHall]
      defeatTheWatchers
      resolveWhenRevealed pid
      questProgress 18
      chamber <- locationOf Locations.topmostChamber
      run $ TravelTo chamber
      run $ PlaceTokens GameSource (LocationTarget chamber) Progress 1
      ringBearer <- heroAttrs frodo
      objectivesOn (toTarget ringBearer.heroId) >>= (`shouldSatisfy` elem (toCardCode Objectives.aHeavyBurden))
      shagrat <- enemyOf Enemies.shagrat
      shagratCard <- (.cardId) <$> getEnemyAttrs shagrat
      guarded <- getsGame (map (toCardCode . snd) . concat . lookup shagratCard . scenarioGuardedPlayerCards . toAttrs . (.scenario))
      guarded `shouldBe` [toCardCode Attachments.bladeOfGondolin]
      resolveWhenRevealed pid
      run $ GameOver Won
      l <- getCampaignLog
      map (.completedScenario) l.completed `shouldBe` ["the-tower-of-cirith-ungol"]
      map poolCardCode l.pool `shouldBe` [cdCardCode Objectives.aHeavyBurden]
 where
  getEnemyAttrs eid = toAttrs <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))
