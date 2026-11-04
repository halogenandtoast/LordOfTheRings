module LordOfTheRings.FlameOfTheWest.ThePassingOfTheGreyCompanySpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Campaign
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getLocationThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Keyword (Keyword (Phantom))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (AnyCard), getResourcesOfSphere)
import LordOfTheRings.Message.Lifted (lowerThreat)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Saga (removedBurdenSets)
import LordOfTheRings.Scenario (lookupScenario, thePassingOfTheGreyCompany)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, setAsideL, victoryDisplayL)
import LordOfTheRings.Sphere (Sphere (Fellowship))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

greyTest :: (PlayerId -> GameT ()) -> IO ()
greyTest = gameTestIn thePassingOfTheGreyCompany testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

-- | Setup, its stage 1A clicked and stage 1B's When Revealed resolved, passing.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  resolveWhenRevealed pid
  chooseLabel "$label.pass" pid

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

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | A card effect reduces the player's threat.
lower :: PlayerId -> Int -> GameT ()
lower pid n = do
  runQueueT (lowerThreat pid n)
  void mainLoop

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

setThreat :: PlayerId -> Int -> GameT ()
setThreat pid n = do
  t <- threatOf pid
  run $ RaiseThreat pid (n - t)

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

-- | The copies of Overcome by Fear on the player's threat dial.
fearsOn :: PlayerId -> GameT [ObjectiveId]
fearsOn pid =
  getsGame (map toId . filter (\o -> toCardCode o == toCardCode Objectives.overcomeByFear && o.placement == AttachedTo (PlayerTarget pid)) . toList . (.entities.objectives))

-- | A copy of Overcome by Fear set aside, as one detached is.
setAsideFear :: GameT ()
setAsideFear = do
  card <- genEncounterCard Objectives.overcomeByFear
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL %~ (card :))

setAsideCodes :: GameT [CardCode]
setAsideCodes = map toCardCode . scenarioSetAside <$> scenarioAttrs

-- | Overcome by Fear on the player's dial, from the set-aside copies.
attachFear :: PlayerId -> GameT ObjectiveId
attachFear pid = do
  card <- genEncounterCard Objectives.overcomeByFear
  run $ PutObjectiveIntoPlayAttached card (PlayerTarget pid)
  fromJustNote "no fear" . headMay <$> fearsOn pid

-- | The enemy attacks the player, the shadow card dealt to it first.
shadowAttack :: PlayerId -> EnemyId -> CardDef -> GameT ()
shadowAttack pid eid shadow = do
  _ <- setEncounterDeck [shadow]
  run $ DealShadowCard eid
  inPhase CombatPhase
  run $ EnemyAttacks pid
  passActions pid

-- | Whether the open question offers a label starting with the key.
offersLabel :: Text -> PlayerId -> GameT Bool
offersLabel key pid = maybe False (any \case Label l _ -> key `isPrefixOf` l; _ -> False) . fmap questionChoices <$> getQuestion pid

aragorn :: GameT Hero
aragorn = getHeroWithCode (toCardCode Heroes.aragornTheFlameOfTheWest)

spec :: Spec
spec = describe "The Passing of the Grey Company" do
  it "is a scenario the game can be dealt" . greyTest $ \_ ->
    isJust (lookupScenario "the-passing-of-the-grey-company") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "gives the first player Aragorn, Overcome by Fear on his dial, and sets The Stone of Erech and Army of the Dead aside" . greyTest $ \pid -> do
      setUp pid
      a <- aragorn
      a.owner `shouldBe` pid
      length <$> fearsOn pid `shouldReturn` 1
      sort <$> setAsideCodes `shouldReturn` sort (map toCardCode [Locations.theStoneOfErech, Enemies.armyOfTheDead])
      attrs <- scenarioAttrs
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.thePathsOfTheDead, Quests.theDeadAreFollowing]
      all ((== Just ThePassingOfTheGreyCompany) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      activeQuestCode `shouldReturn` toCardCode Quests.theForbiddenDoor
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

    it "attaches a copy to each player's dial" . twoPlayerTestIn thePassingOfTheGreyCompany testDeck $ \p1 p2 -> do
      run Setup
      replicateM_ 2 do
        asked <- filterM (fmap isJust . getQuestion) [p1, p2]
        for_ (take 1 asked) $ chooseLabel "$label.keepHand"
      lead <- getLead
      chooseOptionMatching "flip the quest" lead (const True)
      length <$> fearsOn p1 `shouldReturn` 1
      length <$> fearsOn p2 `shouldReturn` 1

    it "has each player who adds 1 resource to each of his heroes raise his threat by 3" . greyTest $ \pid -> do
      flipFirstQuest pid
      threat <- threatOf pid
      beravor <- getHeroWithCode "01012"
      resources <- getHeroResources beravor.id
      resolveWhenRevealed pid
      chooseLabel "$label.theForbiddenDoor.resources" pid
      threatOf pid `shouldReturn` (threat + 3)
      getHeroResources beravor.id `shouldReturn` (resources + 1)

    it "builds the encounter deck in normal mode" . greyTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.deadManOfDunharrow, Enemies.faithlessDead, Enemies.ghostsOfMen, Enemies.shadowHost, Locations.darkDoor, Locations.trystingPlace, Locations.deadlyRoad, Locations.hauntedPath, Treacheries.bloodRunsChill, Treacheries.theWayIsShut, Treacheries.whispersInTheDark, Treacheries.noTurningBack]
        `shouldBe` [4, 3, 2, 2, 2, 3, 3, 3, 3, 2, 3, 2]

    it "builds the easy mode encounter deck" . gameTestIn (overAttrs (modeL .~ EasyMode) thePassingOfTheGreyCompany) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.faithlessDead, Enemies.shadowHost, Locations.darkDoor, Locations.trystingPlace, Treacheries.bloodRunsChill, Treacheries.noTurningBack]
        `shouldBe` [2, 1, 1, 2, 1, 1]

  -- \* The Forbidden Door

  describe "The Forbidden Door" do
    it "discards each hand at the end of the planning phase and advances to The Paths of the Dead, each player searching out a different location" . greyTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.darkDoor, Locations.hauntedPath]
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      length . (.hand) <$> getPlayer pid `shouldReturn` 0
      activeQuestCode `shouldReturn` toCardCode Quests.thePathsOfTheDead
      resolveWhenRevealed pid
      chooseTarget pid . CardIdTarget . (.id) . fromJustNote "no card" . headMay =<< getEncounterDeck
      length <$> locationCodes `shouldReturn` 1

  -- \* The Paths of the Dead

  describe "The Paths of the Dead" do
    it "has the highest threat as its quest points, and is defeated by that much progress" . greyTest $ \pid -> do
      q <- putQuestStage Quests.thePathsOfTheDead
      run $ Flip q
      resolveWhenRevealed pid
      setThreat pid 30
      run $ PlaceTokens GameSource q Progress 29
      activeQuestCode `shouldReturn` toCardCode Quests.thePathsOfTheDead
      -- defeated, with no stage left in the quest deck to advance to
      run $ PlaceTokens GameSource q Progress 1
      getsGame (.outcome) `shouldReturn` Just Won

    it "keeps each player's threat from dropping below his initial threat" . greyTest $ \pid -> do
      q <- putQuestStage Quests.thePathsOfTheDead
      run $ Flip q
      resolveWhenRevealed pid
      start <- (.startingThreat) <$> getPlayer pid
      setThreat pid (start + 2)
      lower pid 5
      threatOf pid `shouldReturn` start

    it "has each player raise his threat by 1 or play no cards as the planning phase begins" . greyTest $ \pid -> do
      q <- putQuestStage Quests.thePathsOfTheDead
      run $ Flip q
      resolveWhenRevealed pid
      threat <- threatOf pid
      run $ CheckWindows [mkAfter $ PhaseBegins PlanningPhase]
      chooseLabel "$label.thePathsOfTheDead.raiseThreat" pid
      threatOf pid `shouldReturn` (threat + 1)
      run $ CheckWindows [mkAfter $ PhaseBegins PlanningPhase]
      chooseLabel "$label.thePathsOfTheDead.cannotPlay" pid
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem (CannotPlay AnyCard))

  -- \* The Dead are Following

  describe "The Dead are Following" do
    it "adds The Stone of Erech and Army of the Dead, and lets a player detach Overcome by Fear for 3 threat" . greyTest $ \pid -> do
      setUp pid
      threat <- threatOf pid
      -- stage 2, its search answered, then stage 3
      run . CompleteQuest . toId =<< getActiveQuest
      resolveWhenRevealed pid
      chooseOptionMatching "search" pid (const True)
      locations <- length <$> locationCodes
      run . CompleteQuest . toId =<< getActiveQuest
      resolveWhenRevealed pid
      chooseLabel "$label.theDeadAreFollowing.detach" pid
      threatOf pid `shouldReturn` (threat + 3)
      fearsOn pid `shouldReturn` []
      setAsideCodes `shouldReturn` [toCardCode Objectives.overcomeByFear]
      length <$> locationCodes `shouldReturn` (locations + 1)
      locationCodes >>= (`shouldSatisfy` elem (toCardCode Locations.theStoneOfErech))
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.armyOfTheDead))

    it "takes Phantom and 20 engagement cost from Oathbreakers while The Stone of Erech is active, and the players win as it is explored" . greyTest $ \pid -> do
      q <- putQuestStage Quests.theDeadAreFollowing
      run $ Flip q
      resolveWhenRevealed pid
      host <- stage Enemies.shadowHost
      stone <- stageLocation Locations.theStoneOfErech
      getModifiers (LocationTarget stone) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      run $ PlaceTokens GameSource q Progress 7
      -- it is not defeated by its progress
      activeQuestCode `shouldReturn` toCardCode Quests.theDeadAreFollowing
      getModifiers (LocationTarget stone) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      _ <- setEncounterDeck [Enemies.deadManOfDunharrow]
      run $ TravelTo stone
      (.progress) . toAttrs <$> getActiveQuest `shouldReturn` 2
      -- the Stone's Forced effect: the player adds an Oathbreaker from the encounter deck
      chooseTarget pid . CardIdTarget . (.id) . fromJustNote "no card" . headMay =<< getEncounterDeck
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.deadManOfDunharrow))
      getModifiers (EnemyTarget host) >>= (`shouldSatisfy` elem (LoseKeyword Phantom))
      getEngagementCost host `shouldReturn` 25
      run $ PlaceTokens GameSource (LocationTarget stone) Progress 5
      getsGame (.outcome) `shouldReturn` Just Won

  -- \* Phantom and Overcome by Fear

  describe "Phantom" do
    it "raises the defending player's threat by the damage instead of dealing it" . greyTest $ \pid -> do
      host <- engaged pid Enemies.shadowHost
      beravor <- hero "01012"
      threat <- threatOf pid
      shadowAttack pid host Locations.darkDoor
      chooseTarget pid beravor
      passActions pid
      -- 4 attack against Beravor's 2 defense
      getCharacterDamage beravor `shouldReturn` 0
      threatOf pid `shouldReturn` (threat + 2)

  describe "Overcome by Fear" do
    it "keeps the threat from dropping, raises it at the end of the round, and may be detached for a Fellowship resource" . greyTest $ \pid -> do
      setUp pid
      threat <- threatOf pid
      lower pid 2
      threatOf pid `shouldReturn` threat
      a <- aragorn
      run $ PlaceTokens GameSource (HeroTarget a.id) Resource 1
      run $ CheckWindows [mkAfter RoundEnds]
      threatOf pid `shouldReturn` (threat + 1)
      chooseLabel "$label.overcomeByFear.spend" pid
      fearsOn pid `shouldReturn` []
      setAsideCodes >>= (`shouldSatisfy` elem (toCardCode Objectives.overcomeByFear))
      getHeroResources a.id `shouldReturn` 0
      lower pid 2
      threatOf pid `shouldReturn` (threat - 1)

  -- \* The encounter cards

  describe "encounter cards" do
    it "has Dead Man of Dunharrow attach a set aside Overcome by Fear or raise threat by 2 as it engages, only the latter with none set aside" . greyTest $ \pid -> do
      threat <- threatOf pid
      _ <- engaged pid Enemies.deadManOfDunharrow
      threatOf pid `shouldReturn` (threat + 2)
      setAsideFear
      _ <- engaged pid Enemies.deadManOfDunharrow
      chooseLabel "$label.overcomeByFear.attach" pid
      length <$> fearsOn pid `shouldReturn` 1
      setAsideCodes `shouldReturn` []

    it "has Ghosts of Men attack the revealer unless he takes Overcome by Fear" . greyTest $ \pid -> do
      setAsideFear
      reveal [Enemies.ghostsOfMen]
      offersLabel "$label.overcomeByFear.attach" pid `shouldReturn` True
      chooseLabel "$label.ghostsOfMen.other" pid
      passActions pid
      getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel l _ _ _) -> "$label.declareDefender" `isPrefixOf` l; _ -> False)

    it "has Faithless Dead raise each player's threat by 2 or reveal another card" . greyTest $ \pid -> do
      threat <- threatOf pid
      reveal [Enemies.faithlessDead]
      chooseLabel "$label.faithlessDead.raiseThreat" pid
      threatOf pid `shouldReturn` (threat + 2)
      reveal [Enemies.faithlessDead, Locations.darkDoor]
      chooseLabel "$label.faithlessDead.reveal" pid
      resolveRevealed
      locationCodes `shouldReturn` [toCardCode Locations.darkDoor]

    it "gives Army of the Dead twice the players in threat, and has its attack take Overcome by Fear or an ally" . greyTest $ \pid -> do
      army <- engaged pid Enemies.armyOfTheDead
      getModifiers (EnemyTarget army) >>= (`shouldSatisfy` \ms -> ImmuneToPlayerCardEffects `elem` ms && ThreatModifier 2 `elem` ms)
      guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Locations.darkDoor]
      run $ DealShadowCard army
      inPhase CombatPhase
      run $ EnemyAttacks pid
      -- with no copy set aside, the ally goes
      getsGame (lookup (case guard' of AllyTarget a -> a; _ -> error "ally") . (.entities.allies)) >>= (`shouldSatisfy` isNothing)

    it "keeps progress off staged locations while Dark Door is staged" . greyTest $ \_ -> do
      door <- stageLocation Locations.darkDoor
      path <- stageLocation Locations.hauntedPath
      getModifiers (LocationTarget path) >>= (`shouldSatisfy` elem (ProgressLimit 0))
      getModifiers (LocationTarget door) >>= (`shouldSatisfy` elem (ProgressLimit 0))

    it "keeps staged enemies from damage while Trysting Place is staged, and engages the first player with an Oathbreaker to travel there" . greyTest $ \pid -> do
      host <- stage Enemies.shadowHost
      place <- stageLocation Locations.trystingPlace
      getModifiers (EnemyTarget host) >>= (`shouldSatisfy` elem CannotTakeDamage)
      _ <- setEncounterDeck [Enemies.deadManOfDunharrow, Locations.darkDoor]
      run $ TravelTo place
      chooseTarget pid . CardIdTarget . (.id) . fromJustNote "no card" . headMay =<< getEncounterDeck
      dead <- enemyOf Enemies.deadManOfDunharrow
      placementOfEnemy dead `shouldReturn` EngagedWith pid

    it "has Deadly Road raise each threat as a When Revealed effect is canceled, and cost a Fellowship resource to travel to" . greyTest $ \pid -> do
      road <- stageLocation Locations.deadlyRoad
      getModifiers (LocationTarget road) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      threat <- threatOf pid
      run $ CheckWindows [mkAfter $ WhenRevealedCanceled pid (LocationTarget road)]
      threatOf pid `shouldReturn` (threat + 1)
      fellowship <- getResourcesOfSphere pid Fellowship
      fellowship `shouldBe` 0

    it "gives Haunted Path 1 threat for each player at 35 threat or more, and raises threat to travel there" . greyTest $ \pid -> do
      path <- stageLocation Locations.hauntedPath
      getLocationThreat path `shouldReturn` 1
      setThreat pid 35
      getLocationThreat path `shouldReturn` 2
      run $ TravelTo path
      threatOf pid `shouldReturn` 36

    it "has Blood Runs Chill damage exhausted characters at 35 threat" . greyTest $ \pid -> do
      beravor <- hero "01012"
      run $ Exhaust beravor
      reveal [Treacheries.bloodRunsChill]
      getCharacterDamage beravor `shouldReturn` 0
      setThreat pid 35
      reveal [Treacheries.bloodRunsChill]
      getCharacterDamage beravor `shouldReturn` 1

    it "has The Way is Shut keep progress off the current quest this round" . greyTest $ \pid -> do
      q <- putQuestStage Quests.thePathsOfTheDead
      reveal [Treacheries.theWayIsShut, Locations.darkDoor]
      getModifiers q >>= (`shouldSatisfy` elem (ProgressLimit 0))
      _ <- pure pid
      pure ()

    it "has Whispers in the Dark weaken the revealer's characters unless he takes Overcome by Fear" . greyTest $ \pid -> do
      setAsideFear
      reveal [Treacheries.whispersInTheDark]
      chooseLabel "$label.whispersInTheDark.other" pid
      beravor <- hero "01012"
      getModifiers beravor >>= (`shouldSatisfy` \ms -> all (`elem` ms) [WillpowerModifier (-1), AttackModifier (-1), DefenseModifier (-1)])
      fearsOn pid `shouldReturn` []

    it "has No Turning Back attach Overcome by Fear with no ally to discard" . greyTest $ \pid -> do
      setAsideFear
      reveal [Treacheries.noTurningBack]
      length <$> fearsOn pid `shouldReturn` 1

  -- \* Campaign mode

  describe "campaign mode" do
    it "removes the burdens of five sets in each of The Flame of the West's scenarios" . greyTest $ \_ -> do
      for_ ["the-passing-of-the-grey-company", "the-siege-of-gondor", "the-battle-of-the-pelennor-fields"] \sid ->
        removedBurdenSets sid `shouldBe` [AShadowOfThePast, FlightToTheFord, TheRingGoesSouth, ThePassageOfTheMarshes, ShelobsLair]

    it "puts Overcome by Fear recorded for a seat onto that player's dial" . campaignTestIn thePassingOfTheGreyCompany lotr {campaignLogPool = [PoolCard (toCardCode Objectives.overcomeByFear) (Just 0) Nothing]} testDeck $ \pid -> do
      setUp pid
      length <$> fearsOn pid `shouldReturn` 1

    it "records Overcome by Fear for the players with it on their dial, and earns Army of the Dead from the victory display" . campaignTestIn thePassingOfTheGreyCompany lotr testDeck $ \pid -> do
      _ <- attachFear pid
      army <- genEncounterCard Enemies.armyOfTheDead
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard army])
      run $ GameOver Won
      l <- getCampaignLog
      l.pool `shouldBe` [PoolCard (toCardCode Objectives.overcomeByFear) (Just 0) Nothing, PoolCard (toCardCode Objectives.armyOfTheDeadBoon) Nothing Nothing]

    it "earns nothing with no Overcome by Fear attached and Army of the Dead not defeated" . campaignTestIn thePassingOfTheGreyCompany lotr testDeck $ \_ -> do
      run $ GameOver Won
      l <- getCampaignLog
      l.pool `shouldBe` []
