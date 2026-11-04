module LordOfTheRings.VengeanceOfMordor.TheRiverRunningSpec (spec) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (cdCardTraits, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (ObjectiveAttrs (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theRiverRunning)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, questDeckL, setAsideL)
import LordOfTheRings.Trait (Trait (Easterling))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

riverTest :: (PlayerId -> GameT ()) -> IO ()
riverTest = gameTestIn theRiverRunning testDeck

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = getsGame (maybe Unplaced (.placement) . lookup eid . (.entities.enemies))

-- | The codes and placements of the enemies in play.
enemiesInPlay :: GameT [(CardCode, Placement)]
enemiesInPlay = getsGame (map (\e -> (toCardCode e, (toAttrs e).placement)) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

locationPlacements :: GameT [(CardCode, Placement)]
locationPlacements = getsGame (map (\l -> (toCardCode l, (toAttrs l).placement)) . toList . (.entities.locations))

-- | Easterling Pursuit, added to the staging area with as many resources on it.
pursuitWith :: Int -> GameT ObjectiveId
pursuitWith n = do
  run . AddToStaging =<< genEncounterCard Objectives.easterlingPursuit
  o <- getsGame (fromJustNote "no Easterling Pursuit" . fmap toId . find ((== toCardCode Objectives.easterlingPursuit) . toCardCode) . toList . (.entities.objectives))
  when (n > 0) $ run $ PlaceTokens GameSource (ObjectiveTarget o) Resource n
  pure o

pursuitAttrs :: GameT (Maybe ObjectiveAttrs)
pursuitAttrs = getsGame (fmap toAttrs . find ((== toCardCode Objectives.easterlingPursuit) . toCardCode) . toList . (.entities.objectives))

pursuitResources :: GameT Int
pursuitResources = maybe 0 (countTokens Resource . (.objectiveTokens)) <$> pursuitAttrs

questOf :: CardDef -> GameT Quest
questOf def = getsGame (fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

questCodes :: GameT [CardCode]
questCodes = getsGame (map toCardCode . toList . (.entities.quests))

-- | The stage put into play and turned over, its When Revealed resolved if it has one.
reachStage :: PlayerId -> CardDef -> GameT Target
reachStage pid def = do
  q <- putQuestStage def
  run $ Flip q
  asked <- isJust <$> getQuestion pid
  when asked $ resolveWhenRevealed pid
  pure q

setAsideCards :: [CardDef] -> GameT ()
setAsideCards defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (setAsideL .~ cards)) =<< getGame

discarded :: CardDef -> GameT EncounterCard
discarded def = do
  card <- genEncounterCard def
  run $ AddToEncounterDiscard [card]
  pure card

discardCodes :: GameT [CardCode]
discardCodes = map toCardCode <$> getEncounterDiscard

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

{- | The enemy attacks the player, the shadow card dealt to it first, and the defender
declares against it.
-}
shadowAttack :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
shadowAttack pid eid shadow defender = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid
  chooseTarget pid defender
  passActions pid

-- | Setup through stage 1A, the player adding the first Easterling enemy offered.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  chooseOptionMatching "add an Easterling enemy" pid (const True)

-- | Every copy of the card in the encounter deck and in play, after setup.
copiesOf :: CardDef -> GameT Int
copiesOf def = do
  deck <- map toCardCode <$> getEncounterDeck
  enemies <- map fst <$> enemiesInPlay
  locations <- map fst <$> locationPlacements
  pure $ length $ filter (== toCardCode def) (deck <> enemies <> locations)

spec :: Spec
spec = describe "The River Running" do
  it "is a scenario the game can be dealt" . riverTest $ \_ ->
    isJust (lookupScenario "the-river-running") `shouldBe` True

  describe "setup and stage 1" do
    it "sets Warriors of the East aside, stages Easterling Pursuit, a River Running and an Easterling enemy" . riverTest $ \pid -> do
      setUp pid
      a <- toAttrs <$> getsGame (.scenario)
      map toCardCode a.scenarioSetAside `shouldBe` [toCardCode Quests.warriorsOfTheEast]
      deck <- map toCardCode <$> getEncounterDeck
      deck `shouldSatisfy` notElem (toCardCode Quests.warriorsOfTheEast)
      deck `shouldSatisfy` notElem (toCardCode Objectives.easterlingPursuit)
      length (filter (== toCardCode Locations.riverRunning) deck) `shouldBe` 1
      fmap (.placement) <$> pursuitAttrs `shouldReturn` Just StagingArea
      locationPlacements `shouldReturn` [(toCardCode Locations.riverRunning, StagingArea)]
      staged <- getsGame (toList . (.entities.enemies))
      map (\e -> ((toAttrs e).placement, Easterling `member` cdCardTraits (toCardDef e))) staged `shouldBe` [(StagingArea, True)]
      questCodes `shouldReturn` [toCardCode Quests.chasedByEasterlings]
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

    it "has each player add an Easterling enemy unlike the others" . twoPlayerTestIn theRiverRunning testDeck $ \p1 p2 -> do
      run Setup
      replicateM_ 2 do
        asked <- filterM (fmap isJust . getQuestion) [p1, p2]
        for_ (take 1 asked) $ chooseLabel "$label.keepHand"
      lead <- getLead
      let other = if lead == p1 then p2 else p1
      chooseOptionMatching "flip the quest" lead (const True)
      chooseOptionMatching "add an Easterling enemy" lead (const True)
      firstCodes <- map fst <$> enemiesInPlay
      deck <- getEncounterDeck
      -- the second player is not offered the first player's title
      Just q <- getQuestion other
      let offered = [t | TargetLabel t _ <- questionChoices q]
          sameTitle = [toTarget c | c <- deck, toCardCode c `elem` firstCodes]
      offered `shouldSatisfy` all (`notElem` sameTitle)
      chooseOptionMatching "add an Easterling enemy" other (const True)
      codes <- map fst <$> enemiesInPlay
      length (nub codes) `shouldBe` 2

    it "keeps the easy-mode counts of The River Running's cards" . gameTestIn (overAttrs (modeL .~ EasyMode) theRiverRunning) testDeck $ \pid -> do
      setUp pid
      copiesOf Enemies.easterlingOutrider `shouldReturn` 1
      copiesOf Locations.exposedRiverbank `shouldReturn` 1
      copiesOf Locations.riverRunning `shouldReturn` 2
      deck <- map toCardCode <$> getEncounterDeck
      length (filter (== toCardCode Treacheries.enemiesCloseBehind) deck) `shouldBe` 2

    it "gets +5 quest points per player at stage 1B" . riverTest $ \pid -> do
      next <- genEncounterCard Quests.theCrossingOfAraw
      putGame . (scenarioL %~ overAttrs (questDeckL .~ [next])) =<< getGame
      q <- reachStage pid Quests.chasedByEasterlings
      run $ PlaceTokens GameSource q Progress 9
      questCodes `shouldReturn` [toCardCode Quests.chasedByEasterlings]
      run $ PlaceTokens GameSource q Progress 1
      questCodes `shouldReturn` [toCardCode Quests.theCrossingOfAraw]

    it "adds 1 threat to the staging area for each resource on Easterling Pursuit at stage 1B" . riverTest $ \pid -> do
      _ <- reachStage pid Quests.chasedByEasterlings
      o <- pursuitWith 0
      threat0 <- getStagingThreat
      run $ PlaceTokens GameSource (ObjectiveTarget o) Resource 2
      getStagingThreat `shouldReturn` threat0 + 2

  describe "stage 2" do
    it "adds Warriors of the East and an enemy for each player from the shuffled discard pile to the staging area" . riverTest $ \pid -> do
      setAsideCards [Quests.warriorsOfTheEast]
      _ <- setEncounterDeck [Locations.rollingPlains, Enemies.easterlingRaider, Enemies.warriorOfRhun]
      _ <- discarded Locations.rockyOutcrop
      _ <- reachStage pid Quests.theCrossingOfAraw
      sort <$> questCodes `shouldReturn` sort [toCardCode Quests.theCrossingOfAraw, toCardCode Quests.warriorsOfTheEast]
      enemiesInPlay >>= (`shouldSatisfy` \es -> length es == 1 && all ((== StagingArea) . snd) es)
      discardCodes >>= (`shouldSatisfy` all (`elem` [toCardCode Locations.rollingPlains, toCardCode Locations.rockyOutcrop]))
      a <- toAttrs <$> getsGame (.scenario)
      a.scenarioSetAside `shouldBe` []

    it "gives each enemy in the staging area -10 engagement cost at stage 2B" . riverTest $ \pid -> do
      raider <- stage Enemies.easterlingRaider
      _ <- reachStage pid Quests.theCrossingOfAraw
      getEngagementCost raider `shouldReturn` 18

    it "places no more than 5 progress on stage 2B each round while a player is engaged with an enemy" . riverTest $ \pid -> do
      raider <- engaged pid Enemies.easterlingRaider
      q <- reachStage pid Quests.theCrossingOfAraw
      run $ PlaceTokens GameSource q Progress 8
      (.progress) . toAttrs <$> questOf Quests.theCrossingOfAraw `shouldReturn` 5
      run $ Destroy GameSource (EnemyTarget raider)
      run $ PlaceTokens GameSource q Progress 8
      (.progress) . toAttrs <$> questOf Quests.theCrossingOfAraw `shouldReturn` 13

    it "is won as stage 2 is defeated" . riverTest $ \pid -> do
      q <- reachStage pid Quests.theCrossingOfAraw
      run $ PlaceTokens GameSource q Progress 15
      getsGame (.outcome) `shouldReturn` Just Won

  describe "Easterling Pursuit" do
    it "gains a resource at the end of the round" . riverTest $ \_ -> do
      _ <- pursuitWith 0
      run $ CheckWindows [mkAfter RoundEnds]
      pursuitResources `shouldReturn` 1

    it "at 3 resources removes them, shuffles the discard pile in and stages an enemy for each player" . riverTest $ \_ -> do
      _ <- pursuitWith 2
      _ <- setEncounterDeck [Locations.rollingPlains, Enemies.warriorOfRhun, Enemies.easterlingRaider]
      _ <- discarded Locations.rockyOutcrop
      run $ CheckWindows [mkAfter RoundEnds]
      pursuitResources `shouldReturn` 0
      enemiesInPlay >>= (`shouldSatisfy` \es -> length es == 1 && all ((== StagingArea) . snd) es)
      discardCodes >>= (`shouldSatisfy` all (`elem` [toCardCode Locations.rollingPlains, toCardCode Locations.rockyOutcrop]))

  describe "encounter cards" do
    it "has Easterling Outrider surge" . riverTest $ \_ -> do
      reveal [Enemies.easterlingOutrider, Locations.rollingPlains]
      map fst <$> locationPlacements `shouldReturn` [toCardCode Locations.rollingPlains]

    it "lets the engaged player place a resource on Easterling Pursuit as Easterling Outrider engages" . riverTest $ \pid -> do
      _ <- pursuitWith 0
      _ <- engaged pid Enemies.easterlingOutrider
      chooseLabel "$label.easterlingOutrider.pursuit" pid
      pursuitResources `shouldReturn` 1

    it "or has Easterling Outrider make an immediate attack" . riverTest $ \pid -> do
      _ <- pursuitWith 0
      _ <- setEncounterDeck []
      _ <- engaged pid Enemies.easterlingOutrider
      chooseLabel "$label.easterlingOutrider.attack" pid
      passActions pid
      gloin <- hero "01007"
      chooseTarget pid gloin
      passActions pid
      pursuitResources `shouldReturn` 0
      -- its 2 attack against Glóin's 1 defense
      getDamage gloin `shouldReturn` 1

    it "discards an attachment the defending player controls with Easterling Outrider's shadow" . riverTest $ \pid -> do
      raider <- engaged pid Enemies.easterlingRaider
      beravor <- hero "01012"
      _ <- putAttachmentOn pid Attachments.citadelPlate beravor
      plate <- getsGame (map toTarget . toList . (.entities.attachments))
      shadowAttack pid raider Enemies.easterlingOutrider beravor
      traverse_ (chooseTarget pid) plate
      getsGame (length . (.entities.attachments)) `shouldReturn` 0

    it "bars optional engagement while River Running is staged, and has the first player engage the highest engagement cost enemy to travel there" . riverTest $ \pid -> do
      river <- stageLocation Locations.riverRunning
      getModifiers GameTarget >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))
      raider <- stage Enemies.easterlingRaider
      warrior <- stage Enemies.warriorOfRhun
      run $ TravelTo river
      placementOfEnemy warrior `shouldReturn` EngagedWith pid
      placementOfEnemy raider `shouldReturn` StagingArea
      getModifiers GameTarget >>= (`shouldSatisfy` notElem (CannotBeOptionallyEngagedBy AnyPlayer))

    it "places a resource on Easterling Pursuit as a when revealed effect is canceled while Exposed Riverbank is staged, and to travel there" . riverTest $ \_ -> do
      _ <- pursuitWith 0
      bank <- stageLocation Locations.exposedRiverbank
      plains <- stageLocation Locations.rollingPlains
      run $ CheckWindows [mkAfter $ WhenRevealedCanceled (LocationTarget plains)]
      pursuitResources `shouldReturn` 1
      run $ TravelTo bank
      pursuitResources `shouldReturn` 2
      run $ CheckWindows [mkAfter $ WhenRevealedCanceled (LocationTarget plains)]
      pursuitResources `shouldReturn` 2

    it "places a resource on Easterling Pursuit with Enemies Close Behind, then raises threat by 1 for each" . riverTest $ \pid -> do
      _ <- pursuitWith 1
      threat0 <- threatOf pid
      reveal [Treacheries.enemiesCloseBehind]
      pursuitResources `shouldReturn` 2
      threatOf pid `shouldReturn` threat0 + 2

    it "gives the attacking enemy +1 attack with Enemies Close Behind's shadow, and a resource on Easterling Pursuit as it destroys a character" . riverTest $ \pid -> do
      _ <- pursuitWith 0
      raider <- engaged pid Enemies.easterlingRaider
      beravor <- hero "01012"
      run $ PlaceTokens GameSource beravor Damage 2
      shadowAttack pid raider Treacheries.enemiesCloseBehind beravor
      -- 2 attack, +1, against Beravor's 2 defense: 1 damage, and she has 1 hit point left
      getDamage beravor `shouldReturn` 3
      pursuitResources `shouldReturn` 0
      run $ Ready beravor
      shadowAttack pid raider Treacheries.enemiesCloseBehind beravor
      pursuitResources `shouldReturn` 1
