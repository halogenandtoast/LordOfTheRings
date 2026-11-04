module LordOfTheRings.OnTheDoorstep.TheLonelyMountainSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign (newCampaignLog)
import LordOfTheRings.Card (cdCardCode, cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (TheLonelyMountain, Wilderland))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Entities (locationsL)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationUnderneath))
import LordOfTheRings.Matcher.Types (PlayerMatcher (FirstPlayer))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario)
import LordOfTheRings.Scenario qualified as Scenarios
import LordOfTheRings.Scenario.Scenarios.TheLonelyMountain.Helpers (enemyProgress)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

tlmTest :: (PlayerId -> GameT ()) -> IO ()
tlmTest = gameTestIn Scenarios.theLonelyMountain testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup: the first player takes Bilbo Baggins, stage 1A sets up and turns over.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

questCodes :: GameT [CardCode]
questCodes = getsGame (map toCardCode . toList . (.entities.quests))

questDeck :: GameT [CardCode]
questDeck = getsGame (map toCardCode . scenarioQuestDeck . toAttrs . (.scenario))

setAsideCodes :: GameT [CardCode]
setAsideCodes = map toCardCode . scenarioSetAside <$> scenarioAttrs

progressOn :: EnemyId -> GameT Int
progressOn = enemyProgress

underTheMountain :: GameT [Card]
underTheMountain = do
  l <- locationOf Locations.theLonelyMountain
  getsGame (maybe [] (locationUnderneath . toAttrs) . Map.lookup l . (.entities.locations))

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

-- | The player's hand and deck replaced with copies of these cards.
setHandAndDeck :: PlayerId -> [CardDef] -> [CardDef] -> GameT ()
setHandAndDeck pid hand deck = do
  h <- traverse (\d -> PlayerCard <$> genPlayerCard d (Just pid)) hand
  d <- traverse (\c -> PlayerCard <$> genPlayerCard c (Just pid)) deck
  g <- getGame
  putGame $ g & playersL . ix pid . handL .~ h & playersL . ix pid . deckL .~ d

handCodes :: PlayerId -> GameT [CardCode]
handCodes pid = map toCardCode . (.hand) <$> getPlayer pid

-- | The players move on from stage 1 to stage 2B, as its Forced effect does.
toStageTwo :: GameT Target
toStageTwo = do
  qs <- getsGame (map toId . toList . (.entities.quests))
  for_ qs (run . LeaveQuestStage)
  run AdvanceQuest
  getsGame (toTarget . fromJustNote "no stage" . headMay . toList . (.entities.quests))

-- | A stage put into play on its B side, as the players reach it.
reachStage :: CardDef -> GameT Target
reachStage def = do
  run . PutQuestIntoPlay =<< genEncounterCard def
  q <- getsGame (toTarget . fromJustNote "no such stage" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))
  run $ Flip q
  resolveRevealed
  pure q

bilboResources :: Int -> GameT ()
bilboResources n = do
  b <- getHeroWithCode "132001"
  run $ PlaceTokens GameSource (toTarget b) Resource n

questSuccessfully :: GameT ()
questSuccessfully = do
  inPhase QuestPhase
  run $ CheckWindows [mkAfter QuestedSuccessfully]

spec :: Spec
spec = describe "The Lonely Mountain" do
  it "is a scenario the game can be dealt" . tlmTest $ \_ ->
    isJust (lookupScenario "the-lonely-mountain") `shouldBe` True

  describe "setup" do
    it "builds the encounter deck from The Lonely Mountain and Wilderland, the four searched cards set aside" . tlmTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- (.encounterDeck) <$> scenarioAttrs
      all ((`elem` map Just [TheLonelyMountain, Wilderland]) . cdEncounterSet . toCardDef) deck `shouldBe` True
      -- 25 cards of The Lonely Mountain (29 less the 4 set aside) and 10 of Wilderland
      length deck `shouldBe` 35
      setAsideCodes >>= (`shouldSatisfy` \cs -> sort cs == sort (map cdCardCode [Enemies.smaugTheGolden, Enemies.smaugTheMagnificent, Locations.theLonelyMountain, Objectives.aBarePatch]))

    it "gives the first player Bilbo Baggins, stages Smaug the Golden and The Lonely Mountain, the treasures under it" . tlmTest $ \pid -> do
      setUp pid
      b <- getHeroWithCode "132001"
      b.owner `shouldBe` pid
      enemyCodes `shouldReturn` [toCardCode Enemies.smaugTheGolden]
      locationCodes `shouldReturn` [toCardCode Locations.theLonelyMountain]
      setAsideCodes >>= (`shouldSatisfy` \cs -> sort cs == sort (map cdCardCode [Enemies.smaugTheMagnificent, Objectives.aBarePatch]))
      under <- underTheMountain
      sort (map toCardCode under) `shouldBe` sort (map cdCardCode [Attachments.theArkenstoneTreasure, Attachments.mithrilShirtTreasure, Attachments.throrsBattleAxe, Attachments.throrsHuntingBow, Attachments.throrsGoldenCup])
      questCodes `shouldReturn` [toCardCode Quests.aWarmWelcome]
      questDeck `shouldReturn` map cdCardCode [Quests.theBurglarsTurnTheLonelyMountain, Quests.insideInformation, Quests.thievesFireMurder]

  describe "stage 1" do
    it "skips the quest phase and advances to stage 2 at the end of the round" . tlmTest $ \pid -> do
      setUp pid
      getModifiers GameTarget >>= (`shouldSatisfy` elem SkipQuestPhase)
      run $ CheckWindows [mkAfter RoundEnds]
      questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]

  describe "The Lonely Mountain" do
    it "gets -2 threat for each card under it, immune to player cards and staying in the staging area" . tlmTest $ \pid -> do
      setUp pid
      l <- locationOf Locations.theLonelyMountain
      getLocationThreat l `shouldReturn` 2
      getModifiers (LocationTarget l) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, CannotLeaveStagingArea])

    it "has its threat halved, rounding up, while Secret Entrance is the active location" . tlmTest $ \pid -> do
      setUp pid
      l <- locationOf Locations.theLonelyMountain
      -- 12 - 10 = 2, halved to 1
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.secretEntrance
      getLocationThreat l `shouldReturn` 1
      g <- getGame
      putGame =<< preloadModifiers (g & entitiesL . locationsL . ix l %~ overAttrs (\a -> a {locationUnderneath = drop 2 (locationUnderneath a)}))
      -- three treasures left: 12 - 6 = 6, halved to 3
      getLocationThreat l `shouldReturn` 3

  describe "Smaug the Golden" do
    it "has threat X, the non-Burglar characters committed, and cannot be engaged" . tlmTest $ \pid -> do
      setUp pid
      sg <- enemyOf Enemies.smaugTheGolden
      getEnemyThreat sg `shouldReturn` 0
      b <- getHeroWithCode "132001"
      run $ CommitToQuest (toTarget b)
      getEnemyThreat sg `shouldReturn` 0
      beravor <- getHeroWithCode "01012"
      run $ CommitToQuest (toTarget beravor)
      getEnemyThreat sg `shouldReturn` 1
      getModifiers (EnemyTarget sg) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, NoEngagementChecks])
      run $ EngageEnemy pid sg
      (.placement) <$> getsGame (fromJustNote "sg" . Map.lookup sg . (.entities.enemies)) `shouldReturn` StagingArea

    it "advances the players to stage 4A with his 6th progress" . tlmTest $ \pid -> do
      setUp pid
      _ <- toStageTwo
      sg <- enemyOf Enemies.smaugTheGolden
      run $ PlaceTokens GameSource (EnemyTarget sg) Progress 5
      questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]
      run $ PlaceTokens GameSource (EnemyTarget sg) Progress 1
      resolveRevealed
      questCodes `shouldReturn` [toCardCode Quests.thievesFireMurder]
      questDeck `shouldReturn` [toCardCode Quests.insideInformation]
      enemyCodes `shouldReturn` [toCardCode Enemies.smaugTheMagnificent]
      setAsideCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.smaugTheGolden))
      _ <- pure pid
      pure ()

  describe "burgle attempts" do
    it "takes the top treasure into the first player's hand at stage 2B once he discards the matching card" . tlmTest $ \pid -> do
      setUp pid
      _ <- toStageTwo
      sg <- enemyOf Enemies.smaugTheGolden
      top <- headMay <$> underTheMountain
      -- Croaking Crows: 1 card matching the revealed card's sphere and type
      setHandAndDeck pid [Allies.guardOfTheCitadel, Events.everVigilant] [Allies.snowbournScout]
      _ <- setEncounterDeck [Locations.hiddenPath, Enemies.croakingCrows, Locations.greatHall]
      questSuccessfully
      progressOn sg `shouldReturn` 1
      chooseLabel "$label.theLonelyMountain.burgle.discard" pid
      handCodes pid >>= (`shouldSatisfy` \cs -> toCardCode Allies.guardOfTheCitadel `notElem` cs && fmap toCardCode top `elem` map Just cs)
      under <- underTheMountain
      length under `shouldBe` 4
      -- the discarded cards: Hidden Path and Croaking Crows
      sort . map toCardCode <$> getEncounterDiscard `shouldReturn` sort (map cdCardCode [Enemies.croakingCrows, Locations.hiddenPath])
      chooseLabel "$label.theBurglarsTurnTheLonelyMountain.advanceToStage3" pid
      resolveRevealed
      questCodes `shouldReturn` [toCardCode Quests.insideInformation]

    it "has Smaug the Golden attack the first player after a failed attempt at stage 2B" . tlmTest $ \pid -> do
      setUp pid
      _ <- toStageTwo
      sg <- enemyOf Enemies.smaugTheGolden
      setHandAndDeck pid [Events.everVigilant] [Allies.snowbournScout]
      _ <- setEncounterDeck [Locations.greatHall, Locations.hiddenPath]
      questSuccessfully
      -- Great Hall wants 3 allies: no discard is offered
      q <- getQuestion pid
      q `shouldSatisfy` maybe False (\x -> not $ any (\case Label l _ -> l == "$label.theLonelyMountain.burgle.discard"; _ -> False) (questionChoices x))
      chooseLabel "$label.theLonelyMountain.burgle.fail" pid
      shadows <- getsGame (maybe [] ((.shadowCards) . toAttrs) . Map.lookup sg . (.entities.enemies))
      length shadows `shouldBe` 1
      under <- underTheMountain
      length under `shouldBe` 5

    it "fails at once when the first player has no cards left in his deck" . tlmTest $ \pid -> do
      setUp pid
      _ <- toStageTwo
      sg <- enemyOf Enemies.smaugTheGolden
      setHandAndDeck pid [Allies.guardOfTheCitadel] []
      _ <- setEncounterDeck [Enemies.croakingCrows, Locations.hiddenPath]
      questSuccessfully
      shadows <- getsGame (maybe [] ((.shadowCards) . toAttrs) . Map.lookup sg . (.entities.enemies))
      length shadows `shouldBe` 1

    it "lets the first player spend 2 Baggins resources to take the revealed card and reveal the next" . tlmTest $ \pid -> do
      setUp pid
      _ <- toStageTwo
      bilboResources 2
      b <- getHeroWithCode "132001"
      had <- getHeroResources b.id
      -- Desolation of Smaug: 2 cards matching the revealed card's cost
      setHandAndDeck pid [Allies.snowbournScout] [Allies.guardOfTheCitadel, Events.everVigilant]
      _ <- setEncounterDeck [Locations.desolationOfSmaug, Locations.hiddenPath]
      questSuccessfully
      -- whichever is revealed first, taking it into hand leaves the other revealed
      chooseLabel "$label.theLonelyMountain.burgle.takeRevealed" pid
      getHeroResources b.id `shouldReturn` (had - 2)
      length <$> handCodes pid `shouldReturn` 2
      deck <- (.deck) <$> getPlayer pid
      length deck `shouldBe` 1

  describe "stage 3" do
    it "sets Smaug the Golden aside, and brings Smaug the Magnificent, engaged with the first player, and A Bare Patch" . tlmTest $ \pid -> do
      setUp pid
      _ <- reachStage Quests.insideInformation
      enemyCodes `shouldReturn` [toCardCode Enemies.smaugTheMagnificent]
      sm <- enemyOf Enemies.smaugTheMagnificent
      getModifiers (EnemyTarget sm) >>= (`shouldSatisfy` elem (EngagedWithEachPlayer FirstPlayer))
      objectives <- getsGame (map toCardCode . toList . (.entities.objectives))
      objectives `shouldBe` [toCardCode Objectives.aBarePatch]

    it "has A Bare Patch's burgle attach it to Smaug the Magnificent, -3 defense" . tlmTest $ \pid -> do
      setUp pid
      _ <- reachStage Quests.insideInformation
      sm <- enemyOf Enemies.smaugTheMagnificent
      getEnemyDefense sm `shouldReturn` 8
      setHandAndDeck pid [Allies.guardOfTheCitadel] [Allies.snowbournScout]
      _ <- setEncounterDeck [Enemies.croakingCrows, Locations.hiddenPath]
      questSuccessfully
      patch <- getsGame (fromJustNote "patch" . headMay . toList . (.entities.objectives))
      chooseAbilityOf pid (toTarget patch)
      chooseLabel "$label.theLonelyMountain.burgle.discard" pid
      getEnemyDefense sm `shouldReturn` 5

    it "removes Smaug the Magnificent once its progress passes his remaining hit points, then is won" . tlmTest $ \pid -> do
      setUp pid
      q <- reachStage Quests.insideInformation
      sm <- enemyOf Enemies.smaugTheMagnificent
      run $ PlaceTokens GameSource (EnemyTarget sm) Damage 15
      run $ PlaceTokens GameSource q Progress 5
      enemyCodes `shouldReturn` [toCardCode Enemies.smaugTheMagnificent]
      run $ PlaceTokens GameSource q Progress 1
      enemyCodes `shouldReturn` []
      getsGame (.outcome) `shouldReturn` Nothing
      run $ PlaceTokens GameSource q Progress 14
      getsGame (.outcome) `shouldReturn` Just Won
      _ <- pure pid
      pure ()

    it "has Smaug the Magnificent attack again when dealt a shadow card with a burgle effect" . tlmTest $ \pid -> do
      setUp pid
      _ <- reachStage Quests.insideInformation
      sm <- enemyOf Enemies.smaugTheMagnificent
      _ <- setEncounterDeck [Locations.greatHall, Locations.hiddenPath]
      run $ DealShadowCard sm
      inPhase CombatPhase
      run $ EnemyAttacks pid
      passActions pid
      -- undefended
      chooseLabel "$label.takeAttackUndefended" pid
      passActions pid
      -- his second attack is dealt Hidden Path
      shadows <- getsGame (maybe [] ((.shadowCards) . toAttrs) . Map.lookup sm . (.entities.enemies))
      map toCardCode shadows `shouldSatisfy` elem (toCardCode Locations.hiddenPath)

  describe "stage 4" do
    it "resets the quest deck to stage 2B with its 7th progress, Smaug the Golden back in the staging area" . tlmTest $ \pid -> do
      setUp pid
      _ <- toStageTwo
      sg <- enemyOf Enemies.smaugTheGolden
      run $ PlaceTokens GameSource (EnemyTarget sg) Progress 6
      resolveRevealed
      q <- getsGame (toTarget . fromJustNote "stage 4" . headMay . toList . (.entities.quests))
      sm <- enemyOf Enemies.smaugTheMagnificent
      run $ PlaceTokens GameSource (EnemyTarget sm) Damage 3
      run $ PlaceTokens GameSource q Progress 7
      questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]
      questDeck `shouldReturn` map cdCardCode [Quests.insideInformation, Quests.thievesFireMurder]
      enemyCodes `shouldReturn` [toCardCode Enemies.smaugTheGolden]
      sg' <- enemyOf Enemies.smaugTheGolden
      progressOn sg' `shouldReturn` 0
      _ <- pure pid
      pure ()

  describe "the encounter cards" do
    it "has Croaking Crows put 1 progress on Smaug the Golden as a location is revealed" . tlmTest $ \pid -> do
      setUp pid
      sg <- enemyOf Enemies.smaugTheGolden
      _ <- stage Enemies.croakingCrows
      _ <- setEncounterDeck [Locations.hiddenPath]
      run RevealEncounterCard
      resolveRevealed
      progressOn sg `shouldReturn` 1
      _ <- pure pid
      pure ()

    it "has Great Hall, active, put 1 progress on Smaug the Golden at the end of the round" . tlmTest $ \pid -> do
      setUp pid
      sg <- enemyOf Enemies.smaugTheGolden
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.greatHall
      hall <- locationOf Locations.greatHall
      run $ CheckWindows [mkAfter RoundEnds]
      -- stage 1B's Forced effect answers the end of the round too: Great Hall's first
      chooseAbilityOf pid (LocationTarget hall)
      progressOn sg `shouldReturn` 1
      _ <- pure pid
      pure ()

    it "lets Western Slopes, explored, remove 1 progress from Smaug the Golden" . tlmTest $ \pid -> do
      setUp pid
      sg <- enemyOf Enemies.smaugTheGolden
      run $ PlaceTokens GameSource (EnemyTarget sg) Progress 2
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.westernSlopes
      slopes <- locationOf Locations.westernSlopes
      run $ PlaceTokens GameSource (LocationTarget slopes) Progress 5
      chooseAbilityOf pid (LocationTarget slopes)
      chooseLabel "$label.westernSlopes.removeProgress" pid
      progressOn sg `shouldReturn` 1

    it "returns Secret Entrance to the staging area when the revealed card is not the type named" . tlmTest $ \pid -> do
      setUp pid
      run . AddToStaging =<< genEncounterCard Locations.secretEntrance
      se <- locationOf Locations.secretEntrance
      _ <- setEncounterDeck [Locations.hiddenPath]
      run $ TravelTo se
      b <- getHeroWithCode "01012"
      chooseTarget pid (toTarget b)
      chooseLabel "$label.secretEntrance.enemy" pid
      resolveRevealed
      (.placement) <$> getLocation se `shouldReturn` StagingArea

    it "keeps Secret Entrance active when the revealed card is the type named" . tlmTest $ \pid -> do
      setUp pid
      run . AddToStaging =<< genEncounterCard Locations.secretEntrance
      se <- locationOf Locations.secretEntrance
      _ <- setEncounterDeck [Locations.hiddenPath]
      run $ TravelTo se
      b <- getHeroWithCode "01012"
      chooseTarget pid (toTarget b)
      chooseLabel "$label.secretEntrance.location" pid
      resolveRevealed
      (.placement) <$> getLocation se `shouldReturn` ActiveLocation

    it "gives the first treachery revealed each round surge while Desolation of Smaug is active" . tlmTest $ \pid -> do
      setUp pid
      run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.desolationOfSmaug
      _ <- setEncounterDeck [Treacheries.knownToAnOunce, Locations.hiddenPath, Treacheries.knownToAnOunce, Locations.greatHall]
      run RevealEncounterCard
      resolveRevealed
      locationCodes >>= (`shouldSatisfy` elem (toCardCode Locations.hiddenPath))
      run RevealEncounterCard
      resolveRevealed
      locationCodes >>= (`shouldSatisfy` notElem (toCardCode Locations.greatHall))
      _ <- pure pid
      pure ()

    it "has Pretending to Sleep advance to stage 4A unless the players spend X Baggins resources" . tlmTest $ \pid -> do
      setUp pid
      _ <- toStageTwo
      sg <- enemyOf Enemies.smaugTheGolden
      run $ PlaceTokens GameSource (EnemyTarget sg) Progress 1
      bilboResources 2
      _ <- setEncounterDeck [Treacheries.pretendingToSleep, Locations.hiddenPath, Locations.hiddenPath]
      run RevealEncounterCard
      resolveRevealed
      progressOn sg `shouldReturn` 2
      chooseLabel "$label.groupSpend" pid
      b <- getHeroWithCode "132001"
      replicateM_ 2 $ chooseTarget pid (toTarget b)
      questCodes `shouldReturn` [toCardCode Quests.theBurglarsTurnTheLonelyMountain]
      _ <- setEncounterDeck [Treacheries.pretendingToSleep, Locations.hiddenPath, Locations.hiddenPath]
      run RevealEncounterCard
      resolveRevealed
      -- 3 progress now, and Bilbo has none left: the players advance
      questCodes `shouldReturn` [toCardCode Quests.thievesFireMurder]

    it "has Known to an Ounce give each Dragon +1 threat for each event discarded" . tlmTest $ \pid -> do
      setUp pid
      sg <- enemyOf Enemies.smaugTheGolden
      setHandAndDeck pid [Events.everVigilant, Events.sneakAttack] []
      inPhase QuestPhase
      b <- getHeroWithCode "01012"
      run $ CommitToQuest (toTarget b)
      reveal1 Treacheries.knownToAnOunce
      handCodes pid `shouldReturn` []
      getEnemyThreat sg `shouldReturn` 3

    it "has Belching Fire's Dragons attack the player with the most willpower committed" . tlmTest $ \pid -> do
      setUp pid
      sg <- enemyOf Enemies.smaugTheGolden
      inPhase QuestPhase
      reveal1 Treacheries.belchingFire
      -- the attack, no shadow card dealt: the player is asked to defend
      shadows <- getsGame (maybe [] ((.shadowCards) . toAttrs) . Map.lookup sg . (.entities.enemies))
      shadows `shouldBe` []
      getQuestion pid >>= (`shouldSatisfy` isJust)

    it "attaches Dragon-Spell to the hero with the most resources, who takes damage as he spends them" . tlmTest $ \pid -> do
      setUp pid
      beravor <- getHeroWithCode "01012"
      run $ PlaceTokens GameSource (toTarget beravor) Resource 3
      resources <- getHeroResources beravor.id
      willpower <- getWillpower (toTarget beravor)
      reveal1 Treacheries.dragonSpell
      getWillpower (toTarget beravor) `shouldReturn` max 0 (willpower - resources)
      run $ SpendResourcesFrom pid [(toTarget beravor, 2)]
      getDamage (toTarget beravor) `shouldReturn` 2
      getWillpower (toTarget beravor) `shouldReturn` max 0 (willpower - (resources - 2))

  describe "campaign mode" do
    it "records each treasure taken from under The Lonely Mountain for the player who took it" . tlmTest $ \pid -> do
      setUp pid
      _ <- toStageTwo
      top <- fromJustNote "treasure" . headMay <$> underTheMountain
      setHandAndDeck pid [Allies.guardOfTheCitadel] [Allies.snowbournScout]
      _ <- setEncounterDeck [Enemies.croakingCrows, Locations.hiddenPath]
      questSuccessfully
      chooseLabel "$label.theLonelyMountain.burgle.discard" pid
      chooseLabel "$label.theBurglarsTurnTheLonelyMountain.stay" pid
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (\a -> a {scenarioCampaign = Just (newCampaignLog "the-hobbit")})
      run ResolveCampaign
      pool <- (.pool) <$> getCampaignLog
      [(p.code, p.seat) | p <- pool] `shouldBe` [(toCardCode top, Just 0)]
 where
  reveal1 def = do
    _ <- setEncounterDeck [def, Locations.hiddenPath]
    run RevealEncounterCard
    resolveRevealed
