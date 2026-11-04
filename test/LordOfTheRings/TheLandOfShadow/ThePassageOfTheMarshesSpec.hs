module LordOfTheRings.TheLandOfShadow.ThePassageOfTheMarshesSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Campaign
import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getLocationThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Fellowship (getSagaOneRing)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, thePassageOfTheMarshes)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

marshesTest :: (PlayerId -> GameT ()) -> IO ()
marshesTest = gameTestIn thePassageOfTheMarshes testDeck

-- | A campaign log with The Road to Isengard just won.
lordOfTheRings :: CampaignLog
lordOfTheRings =
  (newCampaignLog "the-lord-of-the-rings")
    { campaignLogHeroes = [["01012", "01007", "01002"]]
    , campaignLogCompleted =
        [ CompletedScenario sid 100
        | sid <- ["a-shadow-of-the-past", "a-knife-in-the-dark", "flight-to-the-ford", "the-ring-goes-south", "journey-in-the-dark", "breaking-of-the-fellowship", "the-uruk-hai", "helms-deep", "the-road-to-isengard"]
        ]
    }

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | Setup with the encounter deck given, its stage 1A clicked and the first location of
-- the deck chosen for its search.
setUpWith :: [CardDef] -> PlayerId -> GameT ()
setUpWith deck pid = do
  run Setup
  chooseRingBearer pid (toCardCode Heroes.frodoBagginsTheLandOfShadow)
  chooseLabel "$label.keepHand" pid
  cards <- setEncounterDeck deck
  chooseOptionMatching "flip the quest" pid (const True)
  -- a lone location is taken without asking
  asked <- isJust <$> getQuestion pid
  when asked $ for_ (take 1 [c | c <- cards, toCardDef c `elem` locationDefs]) \c -> chooseTarget pid (CardIdTarget c.id)
 where
  locationDefs = [Locations.evilSmellingFens, Locations.shiftingQuagmire, Locations.hauntedMere, Locations.stagnantPools, Locations.softMires]

setUp :: PlayerId -> GameT ()
setUp = setUpWith [Locations.evilSmellingFens, Enemies.deadThings, Treacheries.bitterReek]

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

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

locationAttrs :: LocationId -> GameT LocationAttrs
locationAttrs lid = toAttrs <$> getLocation lid

locationCodes :: GameT [CardCode]
locationCodes = getsGame (map toCardCode . toList . (.entities.locations))

smeagol :: GameT (Maybe AllyAttrs)
smeagol = getsGame (fmap toAttrs . find ((== toCardCode Objectives.smeagolTheLandOfShadow) . toCardCode) . toList . (.entities.allies))

putSmeagolUnder :: PlayerId -> GameT AllyAttrs
putSmeagolUnder pid = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.smeagolTheLandOfShadow
  fromJustNote "no Sméagol" <$> smeagol

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

questionKey :: PlayerId -> GameT (Maybe Text)
questionKey pid = fmap (\case QuestionLabel l _ _ _ -> l; _ -> "") <$> getQuestion pid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Stage 1 defeated with Sméagol under the first player, stage 2's When Revealed resolved.
toStage2 :: PlayerId -> GameT ()
toStage2 pid = do
  _ <- putSmeagolUnder pid
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Progress 6
  resolveWhenRevealed pid

-- | So many locations put in the victory display.
toVictoryDisplay :: Int -> GameT ()
toVictoryDisplay n = replicateM_ n do
  lid <- stageLocation Locations.softMires
  run $ AddToVictoryDisplay (LocationTarget lid)

-- | Stage 2 defeated too, stage 3's When Revealed resolved.
toStage3 :: PlayerId -> GameT ()
toStage3 pid = do
  toStage2 pid
  toVictoryDisplay 4
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Progress 12
  resolveWhenRevealed pid

frodo :: GameT Hero
frodo = getsGame (fromJustNote "no Frodo" . find ((== toCardCode Heroes.frodoBagginsTheLandOfShadow) . toCardCode) . toList . (.entities.heroes))

spec :: Spec
spec = describe "The Passage of the Marshes" do
  it "is a scenario the game can be dealt" . marshesTest $ \_ ->
    isJust (lookupScenario "the-passage-of-the-marshes") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "builds the encounter deck from The Passage of the Marshes, Gollum and Morgul Nazgûl, without Gollum or The Searching Eye" . marshesTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      length deck `shouldBe` 36
      all ((`elem` map Just [ThePassageOfTheMarshes, GollumSaga, NazgulSaga]) . cdEncounterSet . toCardDef) deck `shouldBe` True
      map (`copies` deck) [Enemies.gollumTheLandOfShadow, Treacheries.theSearchingEye, Enemies.morgulWraith, Treacheries.callOfTheRing, Treacheries.weHatesThem, Locations.softMires]
        `shouldBe` [0, 0, 2, 2, 2, 4]
      map toCardCode . scenarioQuestDeck <$> scenarioAttrs `shouldReturn` map toCardCode [Quests.acrossTheMarshes, Quests.dontLookAtTheLights]

    it "gives the first player Frodo Baggins with The One Ring, engages Gollum with him and reveals the location he searches for" . marshesTest $ \pid -> do
      setUp pid
      f <- frodo
      f.owner `shouldBe` pid
      ring <- fromJustNote "no One Ring" <$> getSagaOneRing
      (toAttrs ring).objectivePlacement `shouldBe` AttachedTo (toTarget f.id)
      gollum <- enemyOf Enemies.gollumTheLandOfShadow
      placementOfEnemy gollum `shouldReturn` EngagedWith pid
      locationCodes `shouldReturn` [toCardCode Locations.evilSmellingFens]
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` notElem (toCardCode Locations.evilSmellingFens))
      activeQuestCode `shouldReturn` toCardCode Quests.theTamingOfSmeagol

    it "shuffles 1 Searching Eye into each player's deck in campaign mode" . campaignTestIn thePassageOfTheMarshes lordOfTheRings testDeck $ \pid -> do
      setUp pid
      deck <- (.deck) <$> getPlayer pid
      length [() | EncounterCard c <- deck, toCardCode c == toCardCode Treacheries.theSearchingEye] `shouldBe` 1

  -- \* Mire

  describe "the Mire keyword" do
    it "places a mire token on each Mire location in the staging area at the end of the refresh phase" . marshesTest $ \pid -> do
      setUp pid
      [fens] <- getsGame (map toId . toList . (.entities.locations))
      _ <- setEncounterDeck [Enemies.deadThings, Enemies.deadThings]
      run $ EndPhase RefreshPhase
      countTokens Resource . locationTokens <$> locationAttrs fens `shouldReturn` 1

    it "discards a location with as many mire tokens as its Mire X, resolving its Forced effect" . marshesTest $ \pid -> do
      setUp pid
      mires <- stageLocation Locations.softMires
      _ <- setEncounterDeck [Enemies.deadThings]
      run $ PlaceTokens GameSource (LocationTarget mires) Resource 1
      locationCodes >>= (`shouldSatisfy` notElem (toCardCode Locations.softMires))
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.softMires))
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.deadThings))

    it "raises each player's threat by 5 as Stagnant Pools is discarded by mire, which gets +1 threat per mire token" . marshesTest $ \pid -> do
      setUp pid
      pools <- stageLocation Locations.stagnantPools
      run $ PlaceTokens GameSource (LocationTarget pools) Resource 2
      getLocationThreat pools `shouldReturn` 5
      threat <- threatOf pid
      run $ PlaceTokens GameSource (LocationTarget pools) Resource 3
      threatOf pid `shouldReturn` (threat + 5)

    it "does not resolve the Forced effect when the location is discarded otherwise" . marshesTest $ \pid -> do
      setUp pid
      pools <- stageLocation Locations.stagnantPools
      threat <- threatOf pid
      run $ Discard (LocationTarget pools)
      threatOf pid `shouldReturn` threat

  -- \* Gollum and Sméagol

  describe "Gollum and Sméagol" do
    it "turns Gollum over to an exhausted Sméagol under the first player as he is defeated" . marshesTest $ \pid -> do
      setUp pid
      gollum <- enemyOf Enemies.gollumTheLandOfShadow
      getModifiers (EnemyTarget gollum) >>= (`shouldSatisfy` elem ImmuneToNonFellowshipPlayerCardEffects)
      run $ DealDamage GameSource (EnemyTarget gollum) 5
      enemyCodes >>= (`shouldSatisfy` notElem (toCardCode Enemies.gollumTheLandOfShadow))
      s <- fromJustNote "no Sméagol" <$> smeagol
      s.placement `shouldBe` InPlayArea pid
      allyExhausted s `shouldBe` True

    it "turns Sméagol over to a ready Gollum engaged with the first player after the players quest unsuccessfully" . marshesTest $ \pid -> do
      setUp pid
      gollum <- enemyOf Enemies.gollumTheLandOfShadow
      run $ DealDamage GameSource (EnemyTarget gollum) 5
      run $ CheckWindows [mkAfter QuestedUnsuccessfully]
      smeagol >>= (`shouldSatisfy` isNothing)
      gollum' <- enemyOf Enemies.gollumTheLandOfShadow
      placementOfEnemy gollum' `shouldReturn` EngagedWith pid
      getDamage (EnemyTarget gollum') `shouldReturn` 0

    it "loses the game if Sméagol is destroyed" . marshesTest $ \pid -> do
      setUp pid
      s <- putSmeagolUnder pid
      run $ DealDamage GameSource (toTarget s.id) 5
      getsGame (.outcome) `shouldReturn` Just Lost

    it "has Gollum engage the new first player and attack as the token passes in the refresh phase"
      . twoPlayerTestIn thePassageOfTheMarshes testDeck
      $ \p1 p2 -> do
        run . PutEnemyIntoPlayEngaged p1 =<< genEncounterCard Enemies.gollumTheLandOfShadow
        gollum <- enemyOf Enemies.gollumTheLandOfShadow
        placementOfEnemy gollum `shouldReturn` EngagedWith p1
        inPhase RefreshPhase
        run $ SetFirstPlayer p2
        placementOfEnemy gollum `shouldReturn` EngagedWith p2
        passActions p2
        passActions p1
        questionKey p2 >>= (`shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`))

    it "has Sméagol follow the first player token" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \p1 p2 -> do
      _ <- putSmeagolUnder p1
      run $ SetFirstPlayer p2
      (.placement) . fromJustNote "no Sméagol" <$> smeagol `shouldReturn` InPlayArea p2

  -- \* The stages

  describe "The Taming of Sméagol" do
    it "cannot be defeated until the first player controls Sméagol" . marshesTest $ \pid -> do
      setUp pid
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 6
      activeQuestCode `shouldReturn` toCardCode Quests.theTamingOfSmeagol
      gollum <- enemyOf Enemies.gollumTheLandOfShadow
      run $ DealDamage GameSource (EnemyTarget gollum) 5
      activeQuestCode `shouldReturn` toCardCode Quests.acrossTheMarshes

    it "gives Gollum the threat of the top encounter card as defense once attackers are declared against him" . marshesTest $ \pid -> do
      setUp pid
      gollum <- enemyOf Enemies.gollumTheLandOfShadow
      _ <- setEncounterDeck [Locations.hauntedMere]
      run $ CheckWindows [mkAfter $ AttackersDeclared gollum]
      getModifiers (EnemyTarget gollum) >>= (`shouldSatisfy` elem (DefenseModifier 4))

  describe "Across the Marshes" do
    it "keeps progress off the staging area's locations and advances only with Sméagol and 4 locations in the victory display" . marshesTest $ \pid -> do
      setUp pid
      toStage2 pid
      activeQuestCode `shouldReturn` toCardCode Quests.acrossTheMarshes
      [fens] <- getsGame (map toId . toList . (.entities.locations))
      getModifiers (LocationTarget fens) >>= (`shouldSatisfy` elem (ProgressLimit 0))
      stage2 <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource stage2 Progress 12
      activeQuestCode `shouldReturn` toCardCode Quests.acrossTheMarshes
      toVictoryDisplay 4
      activeQuestCode `shouldReturn` toCardCode Quests.dontLookAtTheLights

    it "adds a location discarded from the encounter deck as it is revealed with fewer locations staged than players" . marshesTest $ \pid -> do
      setUp pid
      run . Discard . LocationTarget =<< getsGame (fromJustNote "no location" . fmap toId . headMay . toList . (.entities.locations))
      _ <- setEncounterDeck [Enemies.deadThings, Locations.hauntedMere, Enemies.candleBearer]
      toStage2 pid
      locationCodes `shouldReturn` [toCardCode Locations.hauntedMere]
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Enemies.deadThings))

  describe "Don't Look at the Lights!" do
    it "engages an Undead enemy searched for with each player and shuffles the victory display's locations into the encounter deck" . marshesTest $ \pid -> do
      setUp pid
      cards <- setEncounterDeck [Enemies.candleBearer, Enemies.gropingDead, Locations.hauntedMere]
      toStage3 pid
      for_ (take 1 cards) \c -> chooseTarget pid (CardIdTarget c.id)
      activeQuestCode `shouldReturn` toCardCode Quests.dontLookAtTheLights
      candle <- enemyOf Enemies.candleBearer
      placementOfEnemy candle `shouldReturn` EngagedWith pid
      scenarioVictoryDisplay <$> scenarioAttrs `shouldReturn` []
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` elem (toCardCode Locations.softMires))
      q <- getActiveQuest
      getModifiers (toTarget q) >>= (`shouldSatisfy` elem (ProgressLimit 0))

    it "has each player choose the campaign card's resolution as it is defeated, then wins" . campaignTestIn thePassageOfTheMarshes lordOfTheRings testDeck $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.hauntedMere]
      toStage3 pid
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 24
      chooseLabel "$label.thePassageOfTheMarshes.threatPenalty" pid
      getsGame (.outcome) `shouldReturn` Just Won
      l <- getCampaignLog
      l.seatThreatPenalty `shouldBe` Map.fromList [(0, 2)]
      seatThreatPenalty 0 l `shouldBe` 2

    it "adds The Searching Eye to the Campaign Pool for a player who earns it" . campaignTestIn thePassageOfTheMarshes lordOfTheRings testDeck $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Locations.hauntedMere]
      toStage3 pid
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 24
      chooseLabel "$label.thePassageOfTheMarshes.searchingEye" pid
      l <- getCampaignLog
      l.pool `shouldSatisfy` elem (PoolCard (toCardCode Treacheries.theSearchingEye) (Just 0) Nothing)

  -- \* The encounter cards

  describe "The Searching Eye" do
    it "has its drawer exhaust each hero he controls or reveal an encounter card, stays in his hand, and goes to his discard pile" . marshesTest $ \pid -> do
      setUp pid
      eye <- genEncounterCard Treacheries.theSearchingEye
      run $ PutOnTopOfDeck pid (EncounterCard eye)
      run $ DrawCards (Drawing pid 1)
      questionKey pid >>= (`shouldSatisfy` maybe False ("$label.theSearchingEye.choose" `isPrefixOf`))
      chooseLabel "$label.theSearchingEye.exhaust" pid
      heroes <- getsGame (filter ((== pid) . (.owner)) . toList . (.entities.heroes))
      all (heroExhausted . toAttrs) heroes `shouldBe` True
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem eye.id)
      run $ DiscardFromHand pid eye.id
      map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem eye.id)

  describe "Morgul Wraith" do
    it "gets -30 engagement cost and immunity while The One Ring is exhausted" . marshesTest $ \pid -> do
      setUp pid
      run . AddToStaging =<< genEncounterCard Enemies.morgulWraith
      wraith <- enemyOf Enemies.morgulWraith
      getEngagementCost wraith `shouldReturn` 45
      ring <- fromJustNote "no One Ring" <$> getSagaOneRing
      run $ Exhaust (toTarget ring)
      getEngagementCost wraith `shouldReturn` 15
      getModifiers (EnemyTarget wraith) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

  describe "We Hates Them!" do
    it "has Gollum make an immediate attack when it cannot be canceled" . marshesTest $ \pid -> do
      setUp pid
      _ <- setEncounterDeck [Treacheries.weHatesThem, Enemies.deadThings]
      run RevealEncounterCard
      resolveRevealed
      passActions pid
      questionKey pid >>= (`shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`))

  describe "A Fell Light" do
    it "engages the topmost Undead enemy of the discard pile with the revealer, who is attacked by each Undead engaged with him" . marshesTest $ \pid -> do
      setUp pid
      dead <- genEncounterCard Enemies.deadThings
      run $ AddToEncounterDiscard [dead]
      reveal [Treacheries.aFellLight, Locations.softMires]
      eid <- enemyOf Enemies.deadThings
      placementOfEnemy eid `shouldReturn` EngagedWith pid
      passActions pid
      questionKey pid >>= (`shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`))

  describe "Creeping Marshes" do
    it "places a mire token on each location in the staging area at the end of the refresh phase, then is discarded" . marshesTest $ \pid -> do
      setUp pid
      [fens] <- getsGame (map toId . toList . (.entities.locations))
      reveal [Treacheries.creepingMarshes, Locations.hauntedMere, Enemies.deadThings]
      mere <- getsGame (fmap toId . find ((== toCardCode Locations.hauntedMere) . toCardCode) . toList . (.entities.locations))
      run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
      countTokens Resource . locationTokens <$> locationAttrs fens `shouldReturn` 1
      for_ mere \m -> countTokens Resource . locationTokens <$> locationAttrs m `shouldReturn` 1
      getsGame (toList . (.entities.treacheries)) >>= (`shouldSatisfy` null)

  describe "Groping Dead" do
    it "keeps the engaged player from reducing his threat and places a mire token on the active location after destroying a character" . marshesTest $ \pid -> do
      setUp pid
      [fens] <- getsGame (map toId . toList . (.entities.locations))
      run $ PlaceLocation fens ActiveLocation
      run . AddToStaging =<< genEncounterCard Enemies.gropingDead
      eid <- enemyOf Enemies.gropingDead
      run $ EngageEnemy pid eid
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotReduceThreat)
      guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Locations.softMires]
      inPhase CombatPhase
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid guard'
      passActions pid
      countTokens Resource . locationTokens <$> locationAttrs fens `shouldReturn` 1
