module LordOfTheRings.VengeanceOfMordor.TheCityOfUlfastSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (enemiesL, questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers, preloadModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getEngagementCost, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (..), PlayerMatcher (..), getMainQuestResources, select, selectAny)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theCityOfUlfast)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

-- | A game of the scenario, the player at the test deck's 27 starting threat.
ulfastTest :: (PlayerId -> GameT ()) -> IO ()
ulfastTest body = gameTestIn theCityOfUlfast testDeck \pid -> do
  putGame . (playersL . ix pid . threatL .~ 27) =<< getGame
  body pid

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup through stage 1A, the player's location the first the search offers.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  getQuestion pid >>= \case
    Just _ -> chooseOptionMatching "search for a location" pid (const True)
    Nothing -> pure ()

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engagedWith :: PlayerId -> CardDef -> GameT EnemyId
engagedWith pid def = do
  eid <- stage def
  run $ PlaceEnemy eid (EngagedWith pid)
  pure eid

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = getsGame (maybe Unplaced (.placement) . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

stagedLocationCodes :: GameT [CardCode]
stagedLocationCodes = getsGame (map toCardCode . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))

questOf :: CardDef -> GameT Target
questOf def = getsGame (toTarget . fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

-- | A stage put into play on its B side.
stageB :: CardDef -> GameT Target
stageB def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  putGame =<< preloadModifiers g
  pure q

withDamage :: EnemyId -> Int -> GameT ()
withDamage eid n = do
  g <- getGame
  let g' = g & entitiesL . enemiesL . ix eid %~ overAttrs (\a -> a {enemyTokens = addTokens Damage n a.enemyTokens})
  putGame g'
  putGame =<< preloadModifiers g'

damageOf :: EnemyId -> GameT Int
damageOf eid = getsGame (maybe 0 (countTokens Damage . enemyTokens . toAttrs) . lookup eid . (.entities.enemies))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

discarded :: CardDef -> GameT EncounterCard
discarded def = do
  card <- genEncounterCard def
  run $ AddToEncounterDiscard [card]
  pure card

treacheriesOn :: EnemyId -> GameT [CardCode]
treacheriesOn eid = getsGame (map toCardCode . filter ((== AttachedTo (EnemyTarget eid)) . (.placement)) . toList . (.entities.treacheries))

treacheryOf :: CardDef -> GameT Target
treacheryOf def = getsGame (toTarget . fromJustNote "no such treachery" . find ((== toCardCode def) . toCardCode) . toList . (.entities.treacheries))

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

heroInPlay :: CardCode -> GameT Bool
heroInPlay code = getsGame (any ((== code) . toCardCode) . toList . (.entities.heroes))

-- | The player defends the attack under way with the character.
defendWith :: PlayerId -> Target -> GameT ()
defendWith pid defender = do
  passActions pid
  chooseTarget pid defender
  passActions pid

-- | The enemy attacks the player in the combat phase, the shadow card dealt to it first.
shadowAttack :: PlayerId -> EnemyId -> CardDef -> Target -> GameT ()
shadowAttack pid eid shadow defender = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  defendWith pid defender

engagedWithGuard :: PlayerId -> EnemyId -> GameT Bool
engagedWithGuard pid guard' = elem guard' <$> select (EnemyEngagedWith $ PlayerWithId pid)

-- | Choose the label with the key, its variables after it.
chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

setOf :: EncounterCard -> Maybe EncounterSet
setOf = cdEncounterSet . toCardDef

spec :: Spec
spec = describe "The City of Ulfast" do
  it "is a scenario the game can be dealt" . ulfastTest $ \_ ->
    isJust (lookupScenario "the-city-of-ulfast") `shouldBe` True

  describe "setup" do
    it "adds City Guard and a location to the staging area, builds the quest deck, and turns stage 1 over" . ulfastTest $ \pid -> do
      setUp pid
      enemyCodes `shouldReturn` [toCardCode Enemies.cityGuard]
      length <$> stagedLocationCodes `shouldReturn` 1
      a <- scenarioAttrs
      a.scenarioSetAside `shouldBe` []
      map toCardCode a.questDeck `shouldBe` map toCardCode [Quests.tryingToBlendIn, Quests.drawingAttention, Quests.ulchorsTrap]
      all ((`elem` [Just TheCityOfUlfast, Just ServantsOfSauron, Just UnderGuard]) . setOf) a.encounterDeck `shouldBe` True
      any ((== toCardCode Enemies.cityGuard) . toCardCode) a.encounterDeck `shouldBe` False
      -- 27 of The City of Ulfast, 8 of Servants of Sauron, 8 of Under Guard; City Guard and
      -- the location are out
      length a.encounterDeck `shouldBe` 41
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.searchingForUlchor
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

    it "has each player add a location with a title unlike those already staged" . twoPlayerTestIn theCityOfUlfast testDeck $ \p1 p2 -> do
      run Setup
      -- each keeps his hand, then the first player performs the setup and each searches in
      -- turn, whoever is asked first
      replicateM_ 5 do
        asked <- filterM (fmap isJust . getQuestion) [p1, p2]
        for_ (take 1 asked) \p -> chooseOptionMatching "answer" p (const True)
      getQuestion p1 `shouldReturn` Nothing
      getQuestion p2 `shouldReturn` Nothing
      enemyCodes `shouldReturn` [toCardCode Enemies.cityGuard]
      codes <- stagedLocationCodes
      length codes `shouldBe` 2
      length (ordNub codes) `shouldBe` 2

    it "puts fewer copies of some cards in the encounter deck in easy mode" . gameTestIn (overAttrs (modeL .~ EasyMode) theCityOfUlfast) testDeck $ \pid -> do
      setUp pid
      a <- scenarioAttrs
      staged <- stagedLocationCodes
      let codes = map toCardCode a.encounterDeck <> staged
          copies def = length (filter (== toCardCode def) codes)
      map copies [Enemies.ulfastSentry, Enemies.theKingsElite, Locations.guardedArchway, Locations.streetsOfUlfast, Treacheries.weveBeenSeen]
        `shouldBe` [2, 1, 2, 2, 1]
      map copies [Enemies.ulchorsAgent, Locations.theKingsLawn, Locations.crowdedMarket, Treacheries.extraVigil, Treacheries.patrollingTheStreets, Quests.catAndMouse]
        `shouldBe` [2, 2, 2, 2, 3, 1]

  describe "the quest" do
    it "lowers City Guard's engagement cost by 5 for each resource on stage 1B, places 1 there at the end of the round, and caps its damage at 5" . ulfastTest $ \_ -> do
      guard' <- stage Enemies.cityGuard
      q <- stageB Quests.searchingForUlchor
      getEngagementCost guard' `shouldReturn` 45
      run $ CheckWindows [mkAfter RoundEnds]
      getMainQuestResources `shouldReturn` 1
      getEngagementCost guard' `shouldReturn` 40
      run $ PlaceTokens GameSource q Resource 2
      getEngagementCost guard' `shouldReturn` 30
      run $ DealDamage GameSource (EnemyTarget guard') 8
      damageOf guard' `shouldReturn` 5
      run $ DealDamage GameSource (EnemyTarget guard') 3
      damageOf guard' `shouldReturn` 5

    it "has City Guard engaged with a player once his threat reaches its engagement cost" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      q <- stageB Quests.searchingForUlchor
      -- 27 threat
      run $ PlaceTokens GameSource q Resource 3
      engagedWithGuard pid guard' `shouldReturn` False
      run $ PlaceTokens GameSource q Resource 1
      engagedWithGuard pid guard' `shouldReturn` True
      placementOfEnemy guard' `shouldReturn` StagingArea

    it "has each player raise his threat by 5 or reveal an encounter card as stage 2 is revealed, and caps City Guard's damage at 10" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      card <- genEncounterCard Quests.tryingToBlendIn
      _ <- setEncounterDeck [Locations.theKingsLawn]
      run $ AdvanceToQuest card
      resolveRevealed
      chooseLabel "$label.tryingToBlendIn.reveal" pid
      stagedLocationCodes `shouldReturn` [toCardCode Locations.theKingsLawn]
      threatOf pid `shouldReturn` 27
      run $ DealDamage GameSource (EnemyTarget guard') 12
      damageOf guard' `shouldReturn` 10

    it "raises the threat by 5 when chosen on stage 2" . ulfastTest $ \pid -> do
      card <- genEncounterCard Quests.tryingToBlendIn
      run $ AdvanceToQuest card
      resolveRevealed
      chooseLabel "$label.tryingToBlendIn.threat" pid
      threatOf pid `shouldReturn` 32

    it "has each player raise his threat by 5 or take City Guard's attack as stage 3 is revealed" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      card <- genEncounterCard Quests.drawingAttention
      _ <- setEncounterDeck [Locations.theKingsLawn]
      run $ AdvanceToQuest card
      resolveRevealed
      chooseLabel "$label.drawingAttention.attack" pid
      beravor <- hero "01012"
      defendWith pid beravor
      -- 9 attack against 2 defense and 4 hit points
      heroInPlay "01012" `shouldReturn` False
      run $ DealDamage GameSource (EnemyTarget guard') 20
      damageOf guard' `shouldReturn` 15

    it "raises the threat by 5 when chosen on stage 3" . ulfastTest $ \pid -> do
      _ <- stage Enemies.cityGuard
      card <- genEncounterCard Quests.drawingAttention
      run $ AdvanceToQuest card
      resolveRevealed
      chooseLabel "$label.drawingAttention.threat" pid
      threatOf pid `shouldReturn` 32

    it "adds enemies from the shuffled encounter discard pile to the staging area as stage 4 is revealed" . ulfastTest $ \_ -> do
      _ <- setEncounterDeck []
      _ <- discarded Enemies.ulfastSentry
      _ <- discarded Treacheries.patrollingTheStreets
      card <- genEncounterCard Quests.ulchorsTrap
      run $ AdvanceToQuest card
      resolveRevealed
      enemyCodes `shouldReturn` [toCardCode Enemies.ulfastSentry]

    it "cannot be defeated on stage 4 while City Guard has hit points remaining, and wins the game once it has none" . ulfastTest $ \_ -> do
      guard' <- stage Enemies.cityGuard
      q <- stageB Quests.ulchorsTrap
      run $ PlaceTokens GameSource q Progress 20
      getsGame (.outcome) `shouldReturn` Nothing
      run $ DealDamage GameSource (EnemyTarget guard') 25
      damageOf guard' `shouldReturn` 20
      -- Indestructible: it stays in play
      enemyCodes `shouldReturn` [toCardCode Enemies.cityGuard]
      getsGame (.outcome) `shouldReturn` Just Won

  describe "City Guard" do
    it "is immune to player card effects, cannot leave the staging area, and has threat 1 more than the resources on the main quest" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      q <- stageB Quests.searchingForUlchor
      getModifiers guard' >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getEnemyThreat guard' `shouldReturn` 1
      run $ PlaceTokens GameSource q Resource 2
      getEnemyThreat guard' `shouldReturn` 3
      run $ EngageEnemy pid guard'
      placementOfEnemy guard' `shouldReturn` StagingArea

    it "attacks the player it is engaged with in the combat phase" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      q <- stageB Quests.searchingForUlchor
      run $ PlaceTokens GameSource q Resource 4
      _ <- setEncounterDeck [Locations.theKingsLawn]
      inPhase CombatPhase
      run $ EnemyAttacks pid
      beravor <- hero "01012"
      defendWith pid beravor
      heroInPlay "01012" `shouldReturn` False
      placementOfEnemy guard' `shouldReturn` StagingArea

  describe "Ulfast Sentry" do
    it "has attack 1 more than the resources on the main quest, and places 1 resource there after it engages you, when chosen" . ulfastTest $ \pid -> do
      _ <- stageB Quests.searchingForUlchor
      sentry <- stage Enemies.ulfastSentry
      getEnemyAttack sentry `shouldReturn` 1
      run $ EngageEnemy pid sentry
      chooseLabel "$label.ulfastSentry.resource" pid
      getMainQuestResources `shouldReturn` 1
      getEnemyAttack sentry `shouldReturn` 2

    it "makes an attack after it engages you, when chosen" . ulfastTest $ \pid -> do
      _ <- stageB Quests.searchingForUlchor
      sentry <- stage Enemies.ulfastSentry
      _ <- setEncounterDeck [Locations.theKingsLawn]
      run $ EngageEnemy pid sentry
      chooseLabel "$label.ulfastSentry.attack" pid
      eowyn <- hero "01007"
      defendWith pid eowyn
      -- 1 attack against 1 defense
      getDamage eowyn `shouldReturn` 0
      getMainQuestResources `shouldReturn` 0

  describe "The King's Elite" do
    it "cannot have player card attachments, and places 1 resource on the main quest when it attacks" . ulfastTest $ \pid -> do
      _ <- stageB Quests.searchingForUlchor
      elite <- engagedWith pid Enemies.theKingsElite
      getModifiers elite >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)
      _ <- setEncounterDeck []
      inPhase CombatPhase
      run $ EnemyAttacks pid
      getMainQuestResources `shouldReturn` 1

    it "has the attacking enemy make an additional attack as a shadow" . ulfastTest $ \pid -> do
      agent <- engagedWith pid Enemies.ulchorsAgent
      eowyn <- hero "01007"
      theodred <- hero "01002"
      shadowAttack pid agent Enemies.theKingsElite eowyn
      defendWith pid theodred
      -- 3 attack against 1 defense
      getDamage theodred `shouldReturn` 2

  describe "Ulchor's Agent" do
    it "raises each player's threat by 3 as it is revealed" . ulfastTest $ \pid -> do
      reveal [Enemies.ulchorsAgent]
      threatOf pid `shouldReturn` 30

    it "gives the attacker +1 attack as a shadow, and raises each player's threat by 1 if the attack destroys a character" . ulfastTest $ \pid -> do
      agent <- engagedWith pid Enemies.ulchorsAgent
      eowyn <- hero "01007"
      -- 3 + 1 attack against 1 defense and 3 hit points
      shadowAttack pid agent Enemies.ulchorsAgent eowyn
      heroInPlay "01007" `shouldReturn` False
      threatOf pid `shouldReturn` 28

  describe "The King's Lawn" do
    it "raises each player's threat by 1 more in the refresh phase while in the staging area, and places a resource on the main quest as the players travel there" . ulfastTest $ \pid -> do
      _ <- stageB Quests.searchingForUlchor
      lawn <- stageLocation Locations.theKingsLawn
      run $ CheckWindows [mkAfter (PhaseBegins RefreshPhase)]
      threatOf pid `shouldReturn` 28
      run $ TravelTo lawn
      getMainQuestResources `shouldReturn` 1

  describe "Guarded Archway" do
    it "keeps a player engaged with City Guard from reducing his threat while in the staging area, and has City Guard attack the first player as the players travel there" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      q <- stageB Quests.searchingForUlchor
      archway <- stageLocation Locations.guardedArchway
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem CannotReduceThreat)
      run $ PlaceTokens GameSource q Resource 4
      -- read from City Guard's engagement, itself a modifier, once the modifiers are
      -- gathered again after the next message
      run $ CheckWindows []
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotReduceThreat)
      _ <- setEncounterDeck [Locations.theKingsLawn]
      run $ TravelTo archway
      beravor <- hero "01012"
      defendWith pid beravor
      heroInPlay "01012" `shouldReturn` False
      placementOfEnemy guard' `shouldReturn` StagingArea

  describe "Crowded Market" do
    it "gives City Guard +20 engagement cost while active, and adds an enemy from the shuffled discard pile to the staging area as the players travel there" . ulfastTest $ \_ -> do
      guard' <- stage Enemies.cityGuard
      market <- stageLocation Locations.crowdedMarket
      _ <- setEncounterDeck []
      _ <- discarded Enemies.theKingsElite
      _ <- discarded Locations.theKingsLawn
      run $ TravelTo market
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.theKingsElite))
      getEngagementCost guard' `shouldReturn` 65

  describe "Streets of Ulfast" do
    it "has threat 1 more than the resources on the main quest, and raises each player's threat by as much as the players travel there" . ulfastTest $ \pid -> do
      q <- stageB Quests.searchingForUlchor
      streets <- stageLocation Locations.streetsOfUlfast
      run $ PlaceTokens GameSource q Resource 2
      getLocationThreat streets `shouldReturn` 3
      run $ TravelTo streets
      threatOf pid `shouldReturn` 30

    it "returns the attacking enemy to the staging area after the attack as a shadow" . ulfastTest $ \pid -> do
      agent <- engagedWith pid Enemies.ulchorsAgent
      theodred <- hero "01002"
      shadowAttack pid agent Locations.streetsOfUlfast theodred
      placementOfEnemy agent `shouldReturn` StagingArea

  describe "Extra Vigil" do
    it "attaches to City Guard for -10 engagement cost, and is discarded for 1 threat after the player attacks and damages City Guard" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      reveal [Treacheries.extraVigil]
      treacheriesOn guard' `shouldReturn` [toCardCode Treacheries.extraVigil]
      getEngagementCost guard' `shouldReturn` 35
      vigil <- treacheryOf Treacheries.extraVigil
      heroes <- traverse hero ["01012", "01007", "01002"]
      inPhase CombatPhase
      -- 2 + 1 + 2 attack against 4 defense
      for_ heroes (run . AttackWith)
      run $ ResolveAttack pid guard'
      damageOf guard' `shouldReturn` 1
      chooseAbilityOf pid vigil
      threatOf pid `shouldReturn` 28
      treacheriesOn guard' `shouldReturn` []
      getEngagementCost guard' `shouldReturn` 45

    it "offers nothing when the attack deals City Guard no damage past its cap" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      _ <- stageB Quests.searchingForUlchor
      withDamage guard' 5
      reveal [Treacheries.extraVigil]
      vigil <- treacheryOf Treacheries.extraVigil
      heroes <- traverse hero ["01012", "01007", "01002"]
      inPhase CombatPhase
      for_ heroes (run . AttackWith)
      run $ ResolveAttack pid guard'
      damageOf guard' `shouldReturn` 5
      offersAbilityOf pid vigil `shouldReturn` False

    it "attaches to City Guard as a shadow when it is the attacker" . ulfastTest $ \pid -> do
      guard' <- stage Enemies.cityGuard
      q <- stageB Quests.searchingForUlchor
      run $ PlaceTokens GameSource q Resource 4
      theodred <- hero "01002"
      shadowAttack pid guard' Treacheries.extraVigil theodred
      treacheriesOn guard' `shouldReturn` [toCardCode Treacheries.extraVigil]

  describe "We've Been Seen" do
    it "raises a player's threat by 1 more than the allies he controls, when chosen" . ulfastTest $ \pid -> do
      _ <- stage Enemies.cityGuard
      _ <- putAllyIntoPlay pid Allies.wanderingTook
      reveal [Treacheries.weveBeenSeen]
      chooseLabelStarting "$label.weveBeenSeen.threat" pid
      threatOf pid `shouldReturn` 29

    it "has City Guard attack the player, when chosen" . ulfastTest $ \pid -> do
      _ <- stage Enemies.cityGuard
      reveal [Treacheries.weveBeenSeen]
      _ <- setEncounterDeck [Locations.theKingsLawn]
      chooseLabel "$label.weveBeenSeen.attack" pid
      beravor <- hero "01012"
      defendWith pid beravor
      heroInPlay "01012" `shouldReturn` False

    it "gives the attacker +1 attack and sends its excess damage to the player's other characters as a shadow" . ulfastTest $ \pid -> do
      agent <- engagedWith pid Enemies.ulchorsAgent
      took <- putAllyIntoPlay pid Allies.wanderingTook
      passActions pid
      beravor <- hero "01012"
      -- 3 + 1 attack against 1 defense and 2 hit points: 1 damage past them
      shadowAttack pid agent Treacheries.weveBeenSeen took
      chooseTarget pid beravor
      passActions pid
      getDamage beravor `shouldReturn` 1

  describe "Patrolling the Streets" do
    it "places a resource on the main quest, then raises each player's threat by 1 more than the resources there, when chosen" . ulfastTest $ \pid -> do
      _ <- stageB Quests.searchingForUlchor
      reveal [Treacheries.patrollingTheStreets]
      getMainQuestResources `shouldReturn` 1
      chooseLabelStarting "$label.patrollingTheStreets.threat" pid
      threatOf pid `shouldReturn` 29

    it "keeps progress off the current quest this phase, when chosen" . ulfastTest $ \pid -> do
      q <- stageB Quests.searchingForUlchor
      reveal [Treacheries.patrollingTheStreets]
      chooseLabel "$label.patrollingTheStreets.noProgress" pid
      run $ PlaceTokens GameSource q Progress 3
      (.progress) <$> getActiveQuest `shouldReturn` 0
      threatOf pid `shouldReturn` 27

    it "gives the attacker +1 attack for each resource on the main quest as a shadow" . ulfastTest $ \pid -> do
      q <- stageB Quests.searchingForUlchor
      run $ PlaceTokens GameSource q Resource 2
      agent <- engagedWith pid Enemies.ulchorsAgent
      theodred <- hero "01002"
      _ <- setEncounterDeck [Treacheries.patrollingTheStreets]
      run $ DealShadowCard agent
      inPhase CombatPhase
      run $ EnemyAttacks pid
      defendWith pid theodred
      -- 3 + 2 attack against 1 defense and 4 hit points
      heroInPlay "01002" `shouldReturn` False

  describe "Cat and Mouse" do
    it "gives each enemy in the staging area -10 engagement cost, and lets each player discard a non-unique enemy engaged with him as it is defeated" . ulfastTest $ \pid -> do
      _ <- stageB Quests.searchingForUlchor
      reveal [Quests.catAndMouse]
      sentry <- stage Enemies.ulfastSentry
      getEngagementCost sentry `shouldReturn` 15
      elite <- engagedWith pid Enemies.theKingsElite
      getEngagementCost elite `shouldReturn` 30
      side <- questOf Quests.catAndMouse
      run $ PlaceTokens GameSource side Progress 10
      chooseTarget pid elite
      enemyCodes `shouldReturn` [toCardCode Enemies.ulfastSentry]
      selectAny (EnemyWithId elite) `shouldReturn` False
