module LordOfTheRings.TheRoadDarkens.TheRingGoesSouthSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Campaign
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost, getStagingThreat)
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer), charactersGuardedBy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theRingGoesSouth)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

southTest :: (PlayerId -> GameT ()) -> IO ()
southTest = gameTestIn theRingGoesSouth testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyPlacement :: EnemyId -> GameT Placement
enemyPlacement eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationAttrs
locationOf def = getsGame (toAttrs . fromJustNote "no such location" . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  (.id) <$> locationOf def

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

damageOn :: LocationId -> GameT Int
damageOn lid = (.damage) . toAttrs <$> getLocation lid

treacheryPlacement :: CardDef -> GameT Placement
treacheryPlacement def = getsGame ((.placement) . fromJustNote "no such treachery" . find ((== toCardCode def) . toCardCode) . toList . (.entities.treacheries))

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Set the scenario up with The Road Darkens' Frodo Baggins as the Ring-bearer, keep the
-- hand and flip the first stage.
flipSouthQuest :: PlayerId -> GameT ()
flipSouthQuest pid = do
  run Setup
  chooseRingBearer pid (toCardCode Heroes.frodoBagginsTheRoadDarkens)
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)

frodo :: GameT Hero
frodo = getHeroWithCode (toCardCode Heroes.frodoBagginsTheRoadDarkens)

useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

chooseFirstTarget :: HasCallStack => PlayerId -> GameT ()
chooseFirstTarget pid = chooseOptionMatching "choose a target" pid \case TargetLabel {} -> True; _ -> False

openLabel :: PlayerId -> GameT (Maybe Text)
openLabel pid = getQuestion pid <&> \case
  Just (QuestionLabel l _ _ _) -> Just l
  _ -> Nothing

-- | Setup, with the stage given in play, flipped and its When Revealed resolved.
atStage :: PlayerId -> CardDef -> GameT ()
atStage pid def = do
  flipSouthQuest pid
  atStage' pid def

-- | Advance from the stage in play to the stage given, its When Revealed resolved.
atStage' :: PlayerId -> CardDef -> GameT ()
atStage' pid def = do
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL %~ dropWhile ((/= toCardCode def) . toCardCode))
  q <- getActiveQuest
  run $ CompleteQuest (toId q)
  resolveWhenRevealed pid

spec :: Spec
spec = describe "The Ring Goes South" do
  it "is a scenario the game can be dealt" . southTest $ \_ ->
    isJust (lookupScenario "the-ring-goes-south") `shouldBe` True

  describe "setup" do
    it "sets Lust for the Ring, Redhorn Pass, Doors of Durin and Watcher in the Water aside, Frodo in play with The One Ring" . southTest $ \pid -> do
      flipSouthQuest pid
      attrs <- scenarioAttrs
      sort (map toCardCode attrs.scenarioSetAside)
        `shouldBe` sort (map toCardCode [Treacheries.lustForTheRing, Locations.redhornPass, Locations.doorsOfDurinTheRingGoesSouth, Enemies.watcherInTheWater])
      all ((== Just TheRingGoesSouth) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.theNineWalkers, Quests.theHuntIsUp, Quests.theGatesOfMoria]
      activeQuestCode `shouldReturn` toCardCode Quests.theCouncilOfElrond
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True
      f <- frodo
      f.owner `shouldBe` pid
      rings <- getsGame (filter ((== toCardCode Objectives.theOneRingTheRoadDarkens) . toCardCode) . toList . (.entities.objectives))
      map (.placement) rings `shouldBe` [AttachedTo (toTarget f.id)]

    it "builds the encounter deck in normal mode" . southTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.greatWargChief, Enemies.houndOfSauron, Enemies.howlingWarg, Enemies.crebainFromDunland, Locations.treeCrownedHill, Locations.hillsOfHollin, Locations.redhornFoothills, Locations.eregion, Treacheries.regimentsOfCrows, Treacheries.snowdrifts, Treacheries.bitterCold, Treacheries.stormOfHowls]
        `shouldBe` [1, 4, 4, 2, 1, 3, 3, 3, 2, 2, 2, 3]

    it "builds the easy mode encounter deck" . gameTestIn (overAttrs (modeL .~ EasyMode) theRingGoesSouth) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.houndOfSauron, Locations.redhornFoothills, Locations.eregion, Treacheries.regimentsOfCrows, Treacheries.bitterCold, Treacheries.stormOfHowls]
        `shouldBe` [2, 2, 2, 1, 1, 2]

  describe "The Council of Elrond" do
    it "lays out 4 cards at the end of the planning phase, each given its fate, then raises threat by 5 and advances" . southTest $ \pid -> do
      flipSouthQuest pid
      deck0 <- length . (.deck) <$> getPlayer pid
      hand0 <- length . (.hand) <$> getPlayer pid
      -- 4 Guards of the Citadel on top of the deck, for the 4 faceup cards
      guards <- replicateM 4 (PlayerCard <$> genPlayerCard Allies.guardOfTheCitadel (Just pid))
      putGame . (playersL . ix pid . deckL %~ (guards <>)) =<< getGame
      threat <- threatOf pid
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      useForcedAbilities pid
      openLabel pid `shouldReturn` Just "$label.theCouncilOfElrond.play"
      chooseFirstTarget pid
      -- the played card may ask something of its own; the rest are assigned in turn
      let assignRest = openLabel pid >>= \case
            Just l | "$label.theCouncilOfElrond." `isPrefixOf` l && l /= "$label.theCouncilOfElrond.choose" -> chooseFirstTarget pid >> assignRest
            _ -> pure ()
      assignRest
      chooseLabel "$label.theCouncilOfElrond.raiseThreat" pid
      threatOf pid `shouldReturn` (threat + 5)
      player <- getPlayer pid
      -- one played, one to hand, one discarded, one shuffled back
      (length player.hand, length player.deck, length player.discard) `shouldBe` (hand0 + 1, deck0 + 1, 1)
      allies <- getsGame (filter ((== toCardCode Allies.guardOfTheCitadel) . toCardCode) . toList . (.entities.allies))
      length allies `shouldBe` 1
      activeQuestCode `shouldReturn` toCardCode Quests.theNineWalkers

    it "may shuffle Lust for the Ring into the encounter deck instead" . southTest $ \pid -> do
      flipSouthQuest pid
      run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
      useForcedAbilities pid
      let assignAll = openLabel pid >>= \case
            Just l | "$label.theCouncilOfElrond." `isPrefixOf` l && l /= "$label.theCouncilOfElrond.choose" -> chooseFirstTarget pid >> assignAll
            _ -> pure ()
      assignAll
      chooseLabel "$label.theCouncilOfElrond.shuffleLust" pid
      deck <- getEncounterDeck
      copies Treacheries.lustForTheRing deck `shouldBe` 1

  describe "The Nine Walkers" do
    it "makes Redhorn Pass active and reveals until there is twice the players' count in threat" . southTest $ \pid -> do
      flipSouthQuest pid
      _ <- setEncounterDeck [Locations.hillsOfHollin, Locations.eregion]
      atStage' pid Quests.theNineWalkers
      redhorn <- locationOf Locations.redhornPass
      redhorn.placement `shouldBe` ActiveLocation
      getModifiers (LocationTarget redhorn.id) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      getStagingThreat `shouldReturn` 2
      length <$> getEncounterDeck `shouldReturn` 1

    it "has the players travel if able and damages the active location as an enemy engages" . southTest $ \pid -> do
      atStage pid Quests.theNineWalkers
      hills <- stageLocation Locations.hillsOfHollin
      getModifiers (LocationTarget hills) >>= (`shouldSatisfy` elem MustBeTraveledTo)
      redhorn <- locationOf Locations.redhornPass
      damage0 <- damageOn redhorn.id
      eid <- stage Enemies.howlingWarg
      run $ EngageEnemy pid eid
      useForcedAbilities pid
      damageOn redhorn.id `shouldReturn` (damage0 + 1)

  describe "The Hunt is Up!" do
    it "has the first player find Great Warg Chief and damages each location entering play" . southTest $ \pid -> do
      flipSouthQuest pid
      _ <- setEncounterDeck [Enemies.howlingWarg, Enemies.greatWargChief]
      atStage' pid Quests.theHuntIsUp
      -- only Great Warg Chief may be chosen
      chooseFirstTarget pid
      chief <- enemyOf Enemies.greatWargChief
      enemyPlacement chief `shouldReturn` StagingArea
      hills <- stageLocation Locations.hillsOfHollin
      useForcedAbilities pid
      damageOn hills `shouldReturn` 1

  describe "The Gates of Moria" do
    it "puts the Ring-bearer facedown under Watcher in the Water, Doors of Durin active" . southTest $ \pid -> do
      flipSouthQuest pid
      f0 <- frodo
      run $ PlaceTokens GameSource (toTarget f0.id) Damage 1
      run $ PlaceTokens GameSource (toTarget f0.id) Resource 2
      atStage' pid Quests.theGatesOfMoria
      doors <- locationOf Locations.doorsOfDurinTheRingGoesSouth
      doors.placement `shouldBe` ActiveLocation
      watcher <- enemyOf Enemies.watcherInTheWater
      enemyPlacement watcher `shouldReturn` StagingArea
      w <- getsGame (toAttrs . fromJustNote "watcher" . lookup watcher . (.entities.enemies))
      charactersGuardedBy w.cardId `shouldReturn` [toTarget f0.id]
      f <- frodo
      f.damage `shouldBe` 0
      f.resources `shouldBe` 0
      -- no progress on the Doors while the first player has not the Ring-bearer
      getModifiers (LocationTarget doors.id) >>= (`shouldSatisfy` elem (ProgressLimit 0))
      getModifiers (EnemyTarget watcher) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, CannotLeaveStagingArea, EngagedWithEachPlayer AnyPlayer])

    it "frees the Ring-bearer to the first player, exhausted with 1 damage, at the Watcher's 6th damage" . southTest $ \pid -> do
      flipSouthQuest pid
      atStage' pid Quests.theGatesOfMoria
      watcher <- enemyOf Enemies.watcherInTheWater
      run $ PlaceTokens GameSource (EnemyTarget watcher) Damage 5
      f0 <- frodo
      heroGuardedBy (toAttrs f0) `shouldSatisfy` isJust
      run $ PlaceTokens GameSource (EnemyTarget watcher) Damage 1
      f <- frodo
      heroGuardedBy (toAttrs f) `shouldBe` Nothing
      f.owner `shouldBe` pid
      heroExhausted (toAttrs f) `shouldBe` True
      f.damage `shouldBe` 1
      doors <- locationOf Locations.doorsOfDurinTheRingGoesSouth
      getModifiers (LocationTarget doors.id) >>= (`shouldSatisfy` notElem (ProgressLimit 0))

    it "fills the other active location first, then the Doors, and wins as the Doors are explored" . southTest $ \pid -> do
      flipSouthQuest pid
      atStage' pid Quests.theGatesOfMoria
      watcher <- enemyOf Enemies.watcherInTheWater
      run $ PlaceTokens GameSource (EnemyTarget watcher) Damage 6
      hills <- stageLocation Locations.hillsOfHollin
      getModifiers (LocationTarget hills) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [MustBeTraveledTo, TravelableWithActiveLocation])
      run $ TravelTo hills
      (.placement) <$> getLocation hills `shouldReturn` ActiveLocation
      doors <- locationOf Locations.doorsOfDurinTheRingGoesSouth
      run $ PlaceProgressOnQuest 6
      useForcedAbilities pid
      (.progress) <$> getLocation doors.id `shouldReturn` 2
      run $ PlaceProgressOnQuest 7
      getsGame (.outcome) `shouldReturn` Just Won

    it "loses the game with 9 damage on the Doors, and damages each active location as an enemy engages" . southTest $ \pid -> do
      flipSouthQuest pid
      atStage' pid Quests.theGatesOfMoria
      doors <- locationOf Locations.doorsOfDurinTheRingGoesSouth
      eid <- stage Enemies.howlingWarg
      run $ EngageEnemy pid eid
      useForcedAbilities pid
      damageOn doors.id `shouldReturn` 1
      run $ PlaceTokens GameSource (LocationTarget doors.id) Damage 8
      getsGame (.outcome) `shouldReturn` Just Lost

  describe "the enemies" do
    it "gives Hound of Sauron -5 engagement cost for each enemy in the staging area" . southTest $ \_ -> do
      hound <- stage Enemies.houndOfSauron
      getEngagementCost hound `shouldReturn` 45
      _ <- stage Enemies.howlingWarg
      getEngagementCost hound `shouldReturn` 40

    it "has Great Warg Chief bring a Warg from the encounter deck engaged with the player it engages" . southTest $ \pid -> do
      _ <- setEncounterDeck [Locations.eregion, Enemies.howlingWarg, Enemies.houndOfSauron]
      chief <- stage Enemies.greatWargChief
      run $ EngageEnemy pid chief
      useForcedAbilities pid
      warg <- enemyOf Enemies.howlingWarg
      enemyPlacement warg `shouldReturn` EngagedWith pid
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.eregion]

    it "has Crebain from Dunland engage the revealer, who then reveals the top card" . southTest $ \pid -> do
      reveal [Enemies.crebainFromDunland, Locations.eregion]
      chooseLabel "$label.crebainFromDunland.engage" pid
      useForcedAbilities pid
      crebain <- enemyOf Enemies.crebainFromDunland
      enemyPlacement crebain `shouldReturn` EngagedWith pid
      (.placement) <$> locationOf Locations.eregion `shouldReturn` StagingArea

  describe "the locations" do
    it "raises each player's threat by the damage on Hills of Hollin as it is explored, its staged enemies +5 engagement cost while active" . southTest $ \pid -> do
      hills <- activeLocation Locations.hillsOfHollin
      hound <- stage Enemies.houndOfSauron
      getEngagementCost hound `shouldReturn` 50
      run $ PlaceTokens GameSource (LocationTarget hills) Damage 2
      threat <- threatOf pid
      run $ PlaceTokens GameSource (LocationTarget hills) Progress 4
      useForcedAbilities pid
      threatOf pid `shouldReturn` (threat + 2)

    it "gives each enemy -1 threat while Tree-crowned Hill is active" . southTest $ \_ -> do
      _ <- activeLocation Locations.treeCrownedHill
      _ <- stage Enemies.howlingWarg
      getStagingThreat `shouldReturn` 1

    it "has Redhorn Pass's damage assigned among the player's characters as it is explored" . southTest $ \pid -> do
      redhorn <- activeLocation Locations.redhornPass
      run $ PlaceTokens GameSource (LocationTarget redhorn) Damage 2
      run $ PlaceTokens GameSource (LocationTarget redhorn) Progress 6
      useForcedAbilities pid
      getQuestion pid >>= (`shouldSatisfy` isJust)

  describe "the treacheries" do
    it "places 3 damage on each active location with Regiments of Crows" . southTest $ \_ -> do
      hills <- activeLocation Locations.hillsOfHollin
      reveal [Treacheries.regimentsOfCrows]
      damageOn hills `shouldReturn` 3

    it "attaches Snowdrifts to the active location, limiting its progress to 3 each round" . southTest $ \_ -> do
      hills <- activeLocation Locations.eregion
      reveal [Treacheries.snowdrifts]
      treacheryPlacement Treacheries.snowdrifts `shouldReturn` AttachedTo (LocationTarget hills)
      getModifiers (LocationTarget hills) >>= (`shouldSatisfy` elem (ProgressPerRoundLimit 3))

    it "has Snowdrifts gain surge with no active location" . southTest $ \_ -> do
      reveal [Treacheries.snowdrifts, Locations.eregion]
      (.placement) <$> locationOf Locations.eregion `shouldReturn` StagingArea

    it "has Bitter Cold raise threat by 1 for each ally, if the player chooses" . southTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      threat <- threatOf pid
      reveal [Treacheries.bitterCold]
      chooseLabel "$label.bitterCold.raise" pid
      threatOf pid `shouldReturn` (threat + 1)

    it "has Storm of Howls give each Warg -20 engagement cost" . southTest $ \pid -> do
      warg <- stage Enemies.howlingWarg
      reveal [Treacheries.stormOfHowls]
      chooseLabel "$label.stormOfHowls.engagementCost" pid
      getEngagementCost warg `shouldReturn` 18

  describe "Lust for the Ring" do
    it "attaches to a non-Fellowship hero, raising threat and zeroing its willpower as The One Ring exhausts" . southTest $ \pid -> do
      flipSouthQuest pid
      beravor <- getHeroWithCode "01012"
      reveal [Treacheries.lustForTheRing, Locations.eregion]
      chooseTarget pid beravor.id
      treacheryPlacement Treacheries.lustForTheRing `shouldReturn` AttachedTo (toTarget beravor.id)
      ring <- getsGame (fromJustNote "ring" . find ((== toCardCode Objectives.theOneRingTheRoadDarkens) . toCardCode) . toList . (.entities.objectives))
      threat <- threatOf pid
      run $ CheckWindows [mkAfter $ Exhausted (toTarget ring)]
      useForcedAbilities pid
      threatOf pid `shouldReturn` (threat + 1)
      getWillpower (toTarget beravor.id) `shouldReturn` 0

  describe "campaign mode" do
    it "has the player choose a boon for his hand, the rest shuffled into his deck, all in the pool" . campaignTestIn theRingGoesSouth lotr testDeck $ \pid -> do
      run $ DoStep 9 Setup
      chooseFirstTarget pid
      player <- getPlayer pid
      let boonCodes = map toCardCode [Attachments.stingTheRoadDarkens, Attachments.mithrilShirtTheRoadDarkens, Attachments.glamdringTheRoadDarkens, Attachments.andurilTheRoadDarkens]
      length (filter ((`elem` boonCodes) . toCardCode) player.hand) `shouldBe` 1
      length (filter ((`elem` boonCodes) . toCardCode) player.deck) `shouldBe` 3
      pool <- (.pool) <$> getCampaignLog
      sort (map (.code) pool) `shouldBe` sort boonCodes
      all ((== Just 0) . (.seat)) pool `shouldBe` True

    it "earns Lust for the Ring if it is attached to a hero once won" . campaignTestIn theRingGoesSouth lotr testDeck $ \pid -> do
      beravor <- getHeroWithCode "01012"
      reveal [Treacheries.lustForTheRing, Locations.eregion]
      chooseTarget pid beravor.id
      run $ GameOver Won
      map (.code) . (.pool) <$> getCampaignLog `shouldReturn` [toCardCode Treacheries.lustForTheRing]
