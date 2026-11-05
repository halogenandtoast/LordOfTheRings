module LordOfTheRings.FlameOfTheWest.TheSiegeOfGondorSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getLocationThreat, getStagingThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Keyword (Keyword (Archery))
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theSiegeOfGondor)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

siegeTest :: (PlayerId -> GameT ()) -> IO ()
siegeTest = gameTestIn theSiegeOfGondor testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | Setup, its stage 1A clicked and the player's search for a Ship answered with the first
-- one offered.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  chooseOptionMatching "search for a Ship" pid (const True)

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

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

-- | The location made the active location.
activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  card <- genEncounterCard def
  run $ PutIntoPlayAsActiveLocation card
  getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

objectiveCodes :: GameT [CardCode]
objectiveCodes = getsGame (map toCardCode . toList . (.entities.objectives))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The Corsair Fleet in the staging area, with this many resources on it.
fleetWith :: Int -> GameT ObjectiveId
fleetWith n = do
  run . AddToStaging =<< genEncounterCard Objectives.theCorsairFleet
  oid <- getsGame (fromJustNote "no fleet" . fmap toId . find ((== toCardCode Objectives.theCorsairFleet) . toCardCode) . toList . (.entities.objectives))
  when (n > 0) $ run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource n
  pure oid

fleetResources :: ObjectiveId -> GameT Int
fleetResources oid = getsGame (maybe 0 (.resources) . lookup oid . (.entities.objectives))

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

shadowAttack :: PlayerId -> EnemyId -> CardDef -> GameT ()
shadowAttack pid eid shadow = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

shipsInVictoryDisplay :: Int -> GameT ()
shipsInVictoryDisplay n = replicateM_ n do
  galley <- stageLocation Locations.umbarWarGalley
  run $ AddToVictoryDisplay (LocationTarget galley)

isQuestion :: Text -> Maybe (Question msg) -> Bool
isQuestion key = \case
  Just (QuestionLabel l _ _ _) -> key `isPrefixOf` l
  _ -> False

spec :: Spec
spec = describe "The Siege of Gondor" do
  it "is a scenario the game can be dealt" . siegeTest $ \_ ->
    isJust (lookupScenario "the-siege-of-gondor") `shouldBe` True

  describe "setup" do
    it "gives the first player Aragorn, adds The Corsair Fleet, Great Corsair Ship and a searched Ship to the staging area" . siegeTest $ \pid -> do
      setUp pid
      a <- getHeroWithCode (toCardCode Heroes.aragornTheFlameOfTheWest)
      a.owner `shouldBe` pid
      objectiveCodes `shouldReturn` [toCardCode Objectives.theCorsairFleet]
      codes <- locationCodes
      length codes `shouldBe` 2
      codes `shouldSatisfy` elem (toCardCode Locations.greatCorsairShip)
      codes `shouldSatisfy` any (`elem` map toCardCode [Locations.umbarWarGalley, Locations.corsairDromon])
      deck <- getEncounterDeck
      all ((== Just TheSiegeOfGondor) . cdEncounterSet . toCardDef) deck `shouldBe` True
      (toCardCode <$> getActiveQuest) `shouldReturn` toCardCode Quests.attackOnPelargir

    it "builds the encounter deck in normal mode" . siegeTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.greatCorsairShip, Locations.umbarWarGalley, Locations.corsairDromon, Locations.portOfPelargir, Locations.pelargirQuays, Enemies.corsairCaptain, Enemies.corsairOfUmbar, Enemies.umbarPirateCrew, Enemies.haradrimPirate, Treacheries.fellFolkOfUmbar, Treacheries.blackSails, Treacheries.raidingTheCoasts, Treacheries.drivenToTheBrink]
        `shouldBe` [0, 3, 3, 3, 2, 2, 3, 2, 3, 2, 3, 2, 2]

    it "builds the easy mode encounter deck" . gameTestIn (overAttrs (modeL .~ EasyMode) theSiegeOfGondor) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.corsairCaptain, Enemies.umbarPirateCrew, Treacheries.raidingTheCoasts, Treacheries.drivenToTheBrink]
        `shouldBe` [1, 1, 1, 1]

  describe "Attack on Pelargir" do
    it "makes staged Ships immune to player cards, cannot be defeated while Great Corsair Ship is in play, and wins once defeated" . siegeTest $ \_ -> do
      q <- putQuestStage Quests.attackOnPelargir
      run $ Flip q
      galley <- stageLocation Locations.umbarWarGalley
      quays <- stageLocation Locations.pelargirQuays
      getModifiers (LocationTarget galley) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getModifiers (LocationTarget quays) >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)
      ship <- stageLocation Locations.greatCorsairShip
      run $ PlaceTokens GameSource q Progress 20
      getsGame (.outcome) `shouldReturn` Nothing
      run $ AddToVictoryDisplay (LocationTarget ship)
      run $ PlaceTokens GameSource q Progress 1
      getsGame (.outcome) `shouldReturn` Just Won

  describe "The Corsair Fleet" do
    it "gains a resource as the staging step begins, and each player assigns that much damage or reveals a card" . siegeTest $ \pid -> do
      oid <- fleetWith 1
      run $ CheckWindows [mkAfter StagingStepBegins]
      fleetResources oid `shouldReturn` 2
      chooseLabel "$label.theCorsairFleet.damage n=i:2" pid
      beravor <- hero "01012"
      replicateM_ 2 (chooseTarget pid beravor)
      getCharacterDamage beravor `shouldReturn` 2
      _ <- setEncounterDeck [Locations.portOfPelargir]
      run $ CheckWindows [mkAfter StagingStepBegins]
      chooseLabel "$label.theCorsairFleet.reveal" pid
      resolveRevealed
      locationCodes `shouldReturn` [toCardCode Locations.portOfPelargir]

  describe "locations" do
    it "gives Great Corsair Ship the Fleet's resources as threat, and bars travel there until 3 Ships are in the victory display" . siegeTest $ \_ -> do
      _ <- fleetWith 3
      ship <- stageLocation Locations.greatCorsairShip
      getLocationThreat ship `shouldReturn` 3
      getModifiers (LocationTarget ship) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      shipsInVictoryDisplay 3
      getModifiers (LocationTarget ship) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)

    it "has each player search for a Corsair as the players travel to Great Corsair Ship, a Captain if able" . siegeTest $ \pid -> do
      shipsInVictoryDisplay 3
      ship <- stageLocation Locations.greatCorsairShip
      _ <- setEncounterDeck [Enemies.corsairOfUmbar, Enemies.corsairCaptain, Enemies.haradrimPirate]
      run $ TravelTo ship
      -- the lone player must take the Captain
      captain <- fromJustNote "no captain" . find ((== toCardCode Enemies.corsairCaptain) . toCardCode) <$> getEncounterDeck
      getQuestion pid >>= \q -> [t | Just q' <- [q], TargetLabel t _ <- questionChoices q'] `shouldBe` [CardIdTarget captain.id]
      chooseTarget pid (CardIdTarget captain.id)
      enemyCodes `shouldReturn` [toCardCode Enemies.corsairCaptain]

    it "lets a player of two take any Corsair, the last a Captain when none was taken" . twoPlayerTestIn theSiegeOfGondor testDeck $ \p1 p2 -> do
      shipsInVictoryDisplay 3
      ship <- stageLocation Locations.greatCorsairShip
      cards <- setEncounterDeck [Enemies.corsairOfUmbar, Enemies.corsairCaptain]
      run $ TravelTo ship
      case cards of
        [corsair, captain] -> do
          chooseTarget p1 (CardIdTarget corsair.id)
          getQuestion p2 >>= \q -> [t | Just q' <- [q], TargetLabel t _ <- questionChoices q'] `shouldBe` [CardIdTarget captain.id]
        _ -> error "two cards"

    it "has Umbar War Galley speed up Corsairs while active, and add a Corsair from the deck as the players travel there" . siegeTest $ \_ -> do
      pirate <- stage Enemies.haradrimPirate
      galley <- stageLocation Locations.umbarWarGalley
      _ <- setEncounterDeck [Treacheries.blackSails, Enemies.corsairOfUmbar]
      run $ TravelTo galley
      getEngagementCost pirate `shouldReturn` 24
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.corsairOfUmbar))

    it "gives each Corsair archery 1 while Corsair Dromon is active" . siegeTest $ \_ -> do
      pirate <- stage Enemies.haradrimPirate
      _ <- activeLocation Locations.corsairDromon
      getModifiers (EnemyTarget pirate) >>= (`shouldSatisfy` elem (GainKeyword (Archery 1)))

    it "heals no more than 1 damage from each character each round while Port of Pelargir is staged" . siegeTest $ \_ -> do
      _ <- stageLocation Locations.portOfPelargir
      beravor <- hero "01012"
      run $ PlaceTokens GameSource beravor Damage 3
      run $ HealDamage GameSource beravor 2
      getCharacterDamage beravor `shouldReturn` 2
      run $ HealDamage GameSource beravor 2
      getCharacterDamage beravor `shouldReturn` 2

    it "has Port of Pelargir's shadow keep a damaged character from readying this round" . siegeTest $ \pid -> do
      captain <- engaged pid Enemies.corsairCaptain
      shadowAttack pid captain Locations.portOfPelargir
      beravor <- hero "01012"
      chooseTarget pid beravor
      passActions pid
      getModifiers beravor >>= (`shouldSatisfy` elem CannotReady)

    it "has Pelargir Quays gain damage each round, then be discarded and deal 4 damage" . siegeTest $ \pid -> do
      _ <- stageLocation Locations.pelargirQuays
      replicateM_ 3 $ run $ CheckWindows [mkAfter RoundEnds]
      locationCodes `shouldReturn` [toCardCode Locations.pelargirQuays]
      run $ CheckWindows [mkAfter RoundEnds]
      locationCodes `shouldReturn` []
      getQuestion pid >>= (`shouldSatisfy` isQuestion "$label.dealDamageDivided")

  describe "enemies" do
    it "has Corsair Captain cost 30 less and attack as it engages while a Ship is active" . siegeTest $ \pid -> do
      captain <- stage Enemies.corsairCaptain
      getModifiers (EnemyTarget captain) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))
      getEngagementCost captain `shouldReturn` 49
      _ <- activeLocation Locations.corsairDromon
      getEngagementCost captain `shouldReturn` 19
      run $ EngageEnemy pid captain
      passActions pid
      getQuestion pid >>= (`shouldSatisfy` isQuestion "$label.declareDefender")

    it "gives Corsair of Umbar +2 attack and defense while a Ship is active" . siegeTest $ \_ -> do
      corsair <- stage Enemies.corsairOfUmbar
      _ <- activeLocation Locations.umbarWarGalley
      getModifiers (EnemyTarget corsair) >>= (`shouldSatisfy` \ms -> AttackModifier 2 `elem` ms && DefenseModifier 2 `elem` ms)

    it "has Umbar Pirate Crew refuse attachments and exhaust a character as it engages" . siegeTest $ \pid -> do
      crew <- stage Enemies.umbarPirateCrew
      getModifiers (EnemyTarget crew) >>= (`shouldSatisfy` elem CannotHaveAttachments)
      run $ EngageEnemy pid crew
      beravor <- hero "01012"
      chooseTarget pid beravor
      heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

    it "has Haradrim Pirate deal 2 damage as it engages while a Ship is active" . siegeTest $ \pid -> do
      _ <- activeLocation Locations.umbarWarGalley
      _ <- engaged pid Enemies.haradrimPirate
      beravor <- hero "01012"
      chooseTarget pid beravor
      getCharacterDamage beravor `shouldReturn` 2

  describe "treacheries" do
    it "has Fell Folk of Umbar return engaged Corsairs or engage one from the deck" . siegeTest $ \pid -> do
      reveal [Treacheries.fellFolkOfUmbar, Treacheries.blackSails, Enemies.haradrimPirate, Locations.portOfPelargir]
      -- its Forced effect as it engages, before it is engaged
      chooseOptionMatching "pirate damage" pid (const True)
      pirate <- enemyOf Enemies.haradrimPirate
      placementOfEnemy pirate `shouldReturn` EngagedWith pid
      reveal [Treacheries.fellFolkOfUmbar]
      chooseLabel "$label.fellFolkOfUmbar.return" pid
      placementOfEnemy pirate `shouldReturn` StagingArea

    it "has Black Sails raise staged Ships' threat, and search for a Ship with no non-unique one staged" . siegeTest $ \pid -> do
      _ <- fleetWith 2
      ship <- stageLocation Locations.greatCorsairShip
      _ <- setEncounterDeck [Treacheries.blackSails, Locations.corsairDromon]
      run RevealEncounterCard
      resolveRevealed
      getLocationThreat ship `shouldReturn` 3
      chooseOptionMatching "the Ship" pid (const True)
      locationCodes >>= (`shouldSatisfy` elem (toCardCode Locations.corsairDromon))

    it "has Raiding the Coasts add 3 threat this phase with no attachment to discard" . siegeTest $ \_ -> do
      threat <- getStagingThreat
      reveal [Treacheries.raidingTheCoasts]
      getStagingThreat `shouldReturn` (threat + 3)

    it "has Raiding the Coasts discard the highest cost attachment if chosen" . siegeTest $ \pid -> do
      beravor <- hero "01012"
      _ <- putAttachmentOn pid Attachments.celebriansStone beravor
      reveal [Treacheries.raidingTheCoasts]
      chooseLabel "$label.raidingTheCoasts.discard" pid
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

    it "has Driven to the Brink deal the Fleet's resources among questing characters" . siegeTest $ \pid -> do
      _ <- fleetWith 2
      beravor <- hero "01012"
      run $ CommitToQuest beravor
      reveal [Treacheries.drivenToTheBrink]
      chooseLabel "$label.drivenToTheBrink.damage" pid
      replicateM_ 2 (chooseTarget pid beravor)
      getCharacterDamage beravor `shouldReturn` 2

  describe "campaign mode" do
    it "puts an earned Army of the Dead into play under the first player's control" . campaignTestIn theSiegeOfGondor lotr {campaignLogPool = [PoolCard (toCardCode Objectives.armyOfTheDeadBoon) Nothing Nothing]} testDeck $ \pid -> do
      setUp pid
      armies <- getsGame (filter ((== toCardCode Objectives.armyOfTheDeadBoon) . toCardCode) . toList . (.entities.allies))
      map (.placement) armies `shouldBe` [InPlayArea pid]
      deck <- getEncounterDeck
      copies Objectives.armyOfTheDeadBoon deck `shouldBe` 0

    it "records the Fleet's resources and removes Army of the Dead from the pool" . campaignTestIn theSiegeOfGondor lotr {campaignLogPool = [PoolCard (toCardCode Objectives.armyOfTheDeadBoon) Nothing Nothing]} testDeck $ \_ -> do
      _ <- fleetWith 4
      run $ GameOver Won
      l <- getCampaignLog
      l.pool `shouldBe` []
      recordedCount CorsairFleetResources (toCardCode Objectives.theCorsairFleet) l `shouldBe` 4

