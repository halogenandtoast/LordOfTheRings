module LordOfTheRings.TheRoadDarkens.JourneyInTheDarkSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Campaign
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCard, toCardDef)
import LordOfTheRings.Keyword (Keyword (Indestructible))
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getStagingThreat)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (FirstPlayer), getConditionAttachments)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (EncounterPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (journeyInTheDark, lookupScenario)
import LordOfTheRings.Scenario.Types (GameMode (EasyMode), ScenarioAttrs (..), modeL, questDeckL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import Data.Map.Strict qualified as Map
import TestImport

darkTest :: (PlayerId -> GameT ()) -> IO ()
darkTest = gameTestIn journeyInTheDark testDeck

lotr :: CampaignLog
lotr = (newCampaignLog "the-lord-of-the-rings") {campaignLogHeroes = [["01012", "01007", "01002"]]}

-- | Pursued by the Enemy and Grievous Wound earned before.
pooled :: CampaignLog
pooled = lotr {campaignLogPool = [PoolCard (toCardCode Treacheries.pursuedByTheEnemy) Nothing Nothing, PoolCard (toCardCode Objectives.grievousWound) Nothing Nothing]}

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

objectiveOf :: CardDef -> GameT ObjectiveAttrs
objectiveOf def = getsGame (toAttrs . fromJustNote "no such objective" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

stageObjective :: CardDef -> GameT ObjectiveId
stageObjective def = do
  run . AddToStaging =<< genEncounterCard def
  (.objectiveId) <$> objectiveOf def

doomDamage :: GameT Int
doomDamage = getDamage . ObjectiveTarget . (.objectiveId) =<< objectiveOf Objectives.doomDoomDoom

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

chooseFirstTarget :: HasCallStack => PlayerId -> GameT ()
chooseFirstTarget pid = chooseOptionMatching "choose a target" pid \case TargetLabel {} -> True; _ -> False

-- | Setup, with stage 1 in play and flipped, the player's location chosen: the Ring-bearer
-- is the first offered, The Road Darkens' Frodo Baggins, when there is a choice.
setUp :: PlayerId -> GameT ()
setUp pid = do
  run Setup
  chooseRingBearer
  chooseLabel "$label.keepHand" pid
  chooseOptionMatching "flip the quest" pid (const True)
  -- the player's search for a location, when there is a choice
  chooseRingBearer
 where
  chooseRingBearer =
    getQuestion pid >>= \case
      Just q | all (\case TargetLabel {} -> True; _ -> False) (questionChoices q) -> chooseFirstTarget pid
      _ -> pure ()

-- | Advance from the stage in play to the stage given, its When Revealed resolved.
atStage :: PlayerId -> CardDef -> GameT ()
atStage pid def = do
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL %~ dropWhile ((/= toCardCode def) . toCardCode))
  q <- getActiveQuest
  run $ CompleteQuest (toId q)
  resolveWhenRevealed pid

-- | Drums in the Deep may be left: Chamber of Mazarbul, which bars the next stage, has
-- left play, and the stage has its 3 resources.
leaveChamber :: GameT ()
leaveChamber = do
  chamber <- locationOf Locations.chamberOfMazarbul
  run $ Discard (LocationTarget chamber.id)
  q <- getActiveQuest
  run $ PlaceTokens GameSource (toTarget q) Resource 3

-- | The locations setup put in the staging area leave play.
clearLocations :: GameT ()
clearLocations = getsGame (map toId . toList . (.entities.locations)) >>= traverse_ (run . Discard . LocationTarget)

spec :: Spec
spec = describe "Journey in the Dark" do
  it "is a scenario the game can be dealt" . darkTest $ \_ ->
    isJust (lookupScenario "journey-in-the-dark") `shouldBe` True

  describe "setup" do
    it "sets The Balrog, The Great Bridge and Chamber of Mazarbul aside, Doom, Doom, Doom staged with 10 damage, and a location" . darkTest $ \pid -> do
      setUp pid
      attrs <- scenarioAttrs
      sort (map toCardCode attrs.scenarioSetAside)
        `shouldBe` sort (map toCardCode [Enemies.theBalrog, Locations.theGreatBridge, Locations.chamberOfMazarbul])
      all ((== Just JourneyInTheDark) . cdEncounterSet . toCardDef) attrs.encounterDeck `shouldBe` True
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.drumsInTheDeep, Quests.theBridgeOfKhazadDum]
      activeQuestCode `shouldReturn` toCardCode Quests.theLongDarkOfMoria
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True
      doom <- objectiveOf Objectives.doomDoomDoom
      doom.placement `shouldBe` StagingArea
      doomDamage `shouldReturn` 10
      locations <- getsGame (map (.placement) . toList . (.entities.locations))
      locations `shouldBe` [StagingArea]

    it "builds the encounter deck in normal mode" . darkTest $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.orcChieftain, Enemies.greatCaveTrollJourneyInTheDark, Enemies.urukFromMordor, Enemies.moriaOrc, Enemies.moriaArcher, Locations.minesOfMoria, Locations.manyPillaredHall, Locations.darkenedStairway, Locations.ancientGuardroom, Treacheries.theyAreComing, Treacheries.weCannotGetOut, Treacheries.deepFissure, Treacheries.foolOfATook]
        `shouldBe` [1, 2, 3, 3, 3, 4, 4, 2, 2, 2, 3, 3, 2]
      map (`copies` deck) [Treacheries.pursuedByTheEnemy, Treacheries.shadowOfFearJourneyInTheDark] `shouldBe` [0, 0]

    it "builds the easy mode encounter deck" . gameTestIn (overAttrs (modeL .~ EasyMode) journeyInTheDark) testDeck $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.greatCaveTrollJourneyInTheDark, Enemies.urukFromMordor, Enemies.moriaArcher, Locations.manyPillaredHall, Locations.darkenedStairway, Treacheries.theyAreComing, Treacheries.weCannotGetOut, Treacheries.foolOfATook]
        `shouldBe` [1, 2, 2, 3, 1, 1, 2, 1]

  describe "The Long Dark of Moria" do
    it "removes 1 damage from Doom, Doom, Doom as a player optionally engages an enemy" . darkTest $ \pid -> do
      setUp pid
      eid <- stage Enemies.moriaOrc
      run $ CheckWindows [mkAfter $ OptionallyEngaged eid pid]
      useForcedAbilities pid
      doomDamage `shouldReturn` 9

  describe "Doom, Doom, Doom" do
    it "loses 1 damage at the end of the quest phase, The Balrog added once it has none, and attacks after" . darkTest $ \pid -> do
      setUp pid
      doom <- (.objectiveId) <$> objectiveOf Objectives.doomDoomDoom
      run $ RemoveTokens GameSource (ObjectiveTarget doom) Damage 9
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      useForcedAbilities pid
      doomDamage `shouldReturn` 0
      balrog <- enemyOf Enemies.theBalrog
      enemyPlacement balrog `shouldReturn` StagingArea
      getModifiers (EnemyTarget balrog) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ImmuneToPlayerCardEffects, EngagedWithEachPlayer FirstPlayer])
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      getQuestion pid >>= (`shouldSatisfy` isJust)

  describe "Drums in the Deep" do
    it "adds Chamber of Mazarbul, and cannot be defeated until it has 3 resources per player, one for each enemy defeated" . darkTest $ \pid -> do
      setUp pid
      atStage pid Quests.drumsInTheDeep
      (.placement) <$> locationOf Locations.chamberOfMazarbul `shouldReturn` StagingArea
      q <- getActiveQuest
      getModifiers (toTarget q) >>= (`shouldSatisfy` elem CannotBeDefeated)
      eid <- stage Enemies.moriaOrc
      run $ CheckWindows [mkAfter $ EnemyDefeated eid]
      useForcedAbilities pid
      run $ PlaceTokens GameSource (toTarget q) Resource 1
      run $ PlaceTokens GameSource (toTarget q) Resource 1
      chamber <- locationOf Locations.chamberOfMazarbul
      -- Chamber of Mazarbul still bars the next stage
      getModifiers (toTarget q) >>= (`shouldSatisfy` elem CannotBeDefeated)
      run $ Discard (LocationTarget chamber.id)
      getModifiers (toTarget q) >>= (`shouldSatisfy` notElem CannotBeDefeated)

    it "has the first player reveal an extra card with no enemy in the staging area as the quest phase begins" . darkTest $ \pid -> do
      setUp pid
      atStage pid Quests.drumsInTheDeep
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))

  describe "The Bridge of Khazad-dûm" do
    it "adds The Great Bridge, each player revealing a card and removing 1 damage, and cannot be defeated while the Bridge is in play" . darkTest $ \pid -> do
      setUp pid
      atStage pid Quests.drumsInTheDeep
      leaveChamber
      _ <- setEncounterDeck [Locations.manyPillaredHall, Locations.minesOfMoria]
      atStage pid Quests.theBridgeOfKhazadDum
      (.placement) <$> locationOf Locations.theGreatBridge `shouldReturn` StagingArea
      (.placement) <$> locationOf Locations.manyPillaredHall `shouldReturn` StagingArea
      doomDamage `shouldReturn` 9
      q <- getActiveQuest
      getModifiers (toTarget q) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [CannotBeDefeated, ProgressPerRoundLimit 5])

    it "is won once defeated, the first player choosing a burden treachery and objective for the pool" . campaignTestIn journeyInTheDark lotr testDeck $ \pid -> do
      setUp pid
      atStage pid Quests.drumsInTheDeep
      leaveChamber
      _ <- setEncounterDeck [Locations.manyPillaredHall, Locations.minesOfMoria]
      atStage pid Quests.theBridgeOfKhazadDum
      bridge <- locationOf Locations.theGreatBridge
      run $ Discard (LocationTarget bridge.id)
      q <- getActiveQuest
      run $ CompleteQuest (toId q)
      chooseLabel "$label.journeyInTheDark.shadowOfFear" pid
      chooseLabel "$label.journeyInTheDark.grievousWound" pid
      getsGame (.outcome) `shouldReturn` Just Won
      sort . map (.code) . (.pool) <$> getCampaignLog
        `shouldReturn` sort (map toCardCode [Treacheries.shadowOfFearJourneyInTheDark, Objectives.grievousWound])

    it "earns all 4 burdens if The Balrog is still in play" . campaignTestIn journeyInTheDark lotr testDeck $ \pid -> do
      setUp pid
      _ <- stage Enemies.theBalrog
      atStage pid Quests.drumsInTheDeep
      leaveChamber
      _ <- setEncounterDeck [Locations.manyPillaredHall, Locations.minesOfMoria]
      atStage pid Quests.theBridgeOfKhazadDum
      bridge <- locationOf Locations.theGreatBridge
      run $ Discard (LocationTarget bridge.id)
      q <- getActiveQuest
      run $ CompleteQuest (toId q)
      getsGame (.outcome) `shouldReturn` Just Won
      sort . map (.code) . (.pool) <$> getCampaignLog
        `shouldReturn` sort (map toCardCode [Treacheries.pursuedByTheEnemy, Treacheries.shadowOfFearJourneyInTheDark, Objectives.grievousWound, Objectives.overcomeByGrief])

  describe "the locations" do
    it "has The Great Bridge's response discard a hero to damage The Balrog, which loses Indestructible" . darkTest $ \pid -> do
      setUp pid
      balrog <- stage Enemies.theBalrog
      bridge <- activeLocation Locations.theGreatBridge
      beravor <- getHeroWithCode "01012"
      run $ CheckWindows [mkWhen $ Explored bridge]
      chooseAbilityOf pid (LocationTarget bridge)
      chooseTarget pid beravor.id
      getDamage (EnemyTarget balrog) `shouldReturn` 10
      getModifiers (EnemyTarget balrog) >>= (`shouldSatisfy` elem (LoseKeyword Indestructible))

    it "places progress on Mines of Moria before the quest" . darkTest $ \pid -> do
      setUp pid
      clearLocations
      mines <- locationOf =<< (Locations.minesOfMoria <$ stageLocation Locations.minesOfMoria)
      run $ PlaceProgressOnQuest 6
      -- filled, it is explored from the staging area
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.minesOfMoria))
      getsGame (Map.member mines.id . (.entities.locations)) `shouldReturn` False
      (.progress) <$> getActiveQuest `shouldReturn` 2

    it "gives Many-pillared Hall +1 threat for each copy in play" . darkTest $ \_ -> do
      _ <- stageLocation Locations.manyPillaredHall
      getStagingThreat `shouldReturn` 2
      _ <- stageLocation Locations.manyPillaredHall
      getStagingThreat `shouldReturn` 6

    it "raises each player's threat by the threat of the card Ancient Guardroom discards to travel" . darkTest $ \pid -> do
      guardroom <- stageLocation Locations.ancientGuardroom
      _ <- setEncounterDeck [Locations.darkenedStairway]
      threat <- threatOf pid
      run $ TravelTo guardroom
      threatOf pid `shouldReturn` (threat + 4)
      eid <- stage Enemies.moriaOrc
      getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem (AttackModifier (-1)))

    it "has Chamber of Mazarbul's travel find Orc-chieftain" . darkTest $ \pid -> do
      chamber <- stageLocation Locations.chamberOfMazarbul
      _ <- setEncounterDeck [Enemies.moriaOrc, Enemies.orcChieftain]
      run $ TravelTo chamber
      chooseFirstTarget pid
      chief <- enemyOf Enemies.orcChieftain
      enemyPlacement chief `shouldReturn` StagingArea

  describe "the enemies" do
    it "has Orc-chieftain engage the first player as the encounter phase begins" . darkTest $ \pid -> do
      chief <- stage Enemies.orcChieftain
      run $ CheckWindows [mkAfter $ PhaseBegins EncounterPhase]
      useForcedAbilities pid
      enemyPlacement chief `shouldReturn` EngagedWith pid

    it "has Great Cave-troll's excess damage remove progress from the current quest" . darkTest $ \pid -> do
      setUp pid
      troll <- stage Enemies.greatCaveTrollJourneyInTheDark
      q <- getActiveQuest
      run $ PlaceTokens GameSource (toTarget q) Progress 3
      beravor <- getHeroWithCode "01012"
      run $ CheckWindows [mkAfter $ AttackDealtExcessDamage troll (toTarget beravor.id) 2]
      (.progress) <$> getActiveQuest `shouldReturn` 1

    it "gives Moria Archer threat equal to the players' count" . darkTest $ \_ -> do
      _ <- stage Enemies.moriaArcher
      getStagingThreat `shouldReturn` 1

    it "has Moria Orc remove 1 damage from Doom, Doom, Doom, if chosen" . darkTest $ \pid -> do
      setUp pid
      reveal [Enemies.moriaOrc]
      chooseLabel "$label.moriaOrc.removeDamage" pid
      doomDamage `shouldReturn` 9

  describe "the treacheries" do
    it "has They Are Coming! add an enemy from the encounter deck to the staging area" . darkTest $ \_ -> do
      _ <- setEncounterDeck [Treacheries.theyAreComing, Locations.minesOfMoria, Enemies.moriaOrc]
      run RevealEncounterCard
      resolveRevealed
      orc <- enemyOf Enemies.moriaOrc
      enemyPlacement orc `shouldReturn` StagingArea

    it "has We Cannot Get Out deal damage for each exhausted character" . darkTest $ \pid -> do
      beravor <- getHeroWithCode "01012"
      run $ Exhaust (toTarget beravor.id)
      reveal [Treacheries.weCannotGetOut]
      getQuestion pid >>= (`shouldSatisfy` isJust)

    it "has Fool of a Took! remove 1 damage from Doom, Doom, Doom without The Balrog" . darkTest $ \_ -> do
      _ <- stageObjective Objectives.doomDoomDoom
      doom <- (.objectiveId) <$> objectiveOf Objectives.doomDoomDoom
      run $ PlaceTokens GameSource (ObjectiveTarget doom) Damage 3
      reveal [Treacheries.foolOfATook]
      doomDamage `shouldReturn` 2

  describe "the burdens" do
    it "has campaign setup shuffle in the burden treacheries and add the burden objectives" . campaignTestIn journeyInTheDark lotr testDeck $ \pid -> do
      setUp pid
      deck <- getEncounterDeck
      map (`copies` deck) [Treacheries.pursuedByTheEnemy, Treacheries.shadowOfFearJourneyInTheDark] `shouldBe` [1, 1]
      (.placement) <$> objectiveOf Objectives.grievousWound `shouldReturn` StagingArea
      (.placement) <$> objectiveOf Objectives.overcomeByGrief `shouldReturn` StagingArea

    it "does not double a burden already in the Campaign Pool" . campaignTestIn journeyInTheDark pooled testDeck $ \pid -> do
      setUp pid
      deck <- getEncounterDeck
      map (`copies` deck) [Treacheries.pursuedByTheEnemy, Treacheries.shadowOfFearJourneyInTheDark] `shouldBe` [1, 1]
      objectives <- getsGame (map toCardCode . toList . (.entities.objectives))
      length (filter (== toCardCode Objectives.grievousWound) objectives) `shouldBe` 1
      length (filter (== toCardCode Objectives.overcomeByGrief) objectives) `shouldBe` 1

    it "attaches Grievous Wound to a damaged hero, dealing 1 damage as it exhausts" . darkTest $ \pid -> do
      wound <- stageObjective Objectives.grievousWound
      beravor <- getHeroWithCode "01012"
      run $ CheckWindows [mkAfter $ DealtDamage (toTarget beravor.id) 1]
      useForcedAbilities pid
      (.placement) <$> objectiveOf Objectives.grievousWound `shouldReturn` AttachedTo (toTarget beravor.id)
      getConditionAttachments >>= (`shouldSatisfy` elem (ObjectiveTarget wound))
      run $ CheckWindows [mkAfter $ Exhausted (toTarget beravor.id)]
      useForcedAbilities pid
      getDamage (toTarget beravor.id) `shouldReturn` 1

    it "attaches Overcome by Grief as a character is destroyed, exhausting its hero for the next" . darkTest $ \pid -> do
      _ <- stageObjective Objectives.overcomeByGrief
      beravor <- getHeroWithCode "01012"
      guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      card <- getsGame (fromJustNote "ally" . fmap toCard . find ((== toCardCode Allies.guardOfTheCitadel) . toCardCode) . toList . (.entities.allies))
      run $ CheckWindows [mkAfter $ CharacterDestroyed guard' card]
      useForcedAbilities pid
      chooseTarget pid beravor.id
      (.placement) <$> objectiveOf Objectives.overcomeByGrief `shouldReturn` AttachedTo (toTarget beravor.id)
      run $ CheckWindows [mkAfter $ CharacterDestroyedUnder pid guard']
      useForcedAbilities pid
      b <- getHeroWithCode "01012"
      heroExhausted (toAttrs b) `shouldBe` True
      getModifiers (toTarget beravor.id) >>= (`shouldSatisfy` elem CannotReady)

    it "has Pursued by the Enemy make each engaged enemy attack" . darkTest $ \pid -> do
      eid <- stage Enemies.moriaOrc
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Treacheries.pursuedByTheEnemy, Locations.minesOfMoria, Locations.manyPillaredHall]
      run RevealEncounterCard
      resolveRevealed
      getQuestion pid >>= (`shouldSatisfy` isJust)

    it "has Shadow of Fear blank each character until the end of the round" . darkTest $ \_ -> do
      beravor <- getHeroWithCode "01012"
      reveal [Treacheries.shadowOfFearJourneyInTheDark, Locations.minesOfMoria]
      getModifiers (toTarget beravor.id) >>= (`shouldSatisfy` elem Blank)
