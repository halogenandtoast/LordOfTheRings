module LordOfTheRings.TreasonOfSaruman.HelmsDeepSpec (spec) where

import LordOfTheRings.Campaign
import LordOfTheRings.Card (CardDef (cdEncounterSet), toCardDef)
import LordOfTheRings.EncounterSet (EncounterSet (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat, getStagingThreat)
import LordOfTheRings.Matcher (getArcheryTotal)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (..))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (helmsDeep, lookupScenario)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

helmsTest :: (PlayerId -> GameT ()) -> IO ()
helmsTest = gameTestIn helmsDeep testDeck

-- | The Lord of the Rings campaign, The Uruk-hai won, the test deck's heroes recorded.
lotr :: CampaignLog
lotr =
  (newCampaignLog "the-lord-of-the-rings")
    { campaignLogHeroes = [["01012", "01007", "01002"]]
    , campaignLogCompleted =
        [ CompletedScenario sid 100
        | sid <- ["a-shadow-of-the-past", "a-knife-in-the-dark", "flight-to-the-ford", "the-ring-goes-south", "journey-in-the-dark", "breaking-of-the-fellowship", "the-uruk-hai"]
        ]
    }

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

counsels :: [Card] -> Int
counsels cs = length [() | EncounterCard c <- cs, toCardCode c == toCardCode Treacheries.poisonedCounselsTheTreasonOfSaruman]

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

questProgress :: GameT Int
questProgress = (.progress) <$> getActiveQuest

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

emptyHand :: PlayerId -> GameT ()
emptyHand pid = putGame . (playersL . ix pid . handL .~ []) =<< getGame

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

enemyCodes :: GameT [CardCode]
enemyCodes = getsGame (map toCardCode . toList . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

placementOfEnemy :: EnemyId -> GameT Placement
placementOfEnemy eid = (.placement) <$> getsGame (fromJustNote "no such enemy" . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

locationAttrs :: LocationId -> GameT LocationAttrs
locationAttrs lid = toAttrs <$> getLocation lid

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

activeLocations :: GameT [CardCode]
activeLocations = getsGame (map toCardCode . filter ((== ActiveLocation) . (.placement)) . toList . (.entities.locations))

-- | The location put into play as the active one.
activeAt :: CardDef -> GameT LocationId
activeAt def = do
  run . PutIntoPlayAsActiveLocation =<< genEncounterCard def
  locationOf def

defense :: GameT ObjectiveId
defense = getsGame (fromJustNote "no Defense of Helm's Deep" . fmap toId . find ((== toCardCode Objectives.theDefenseOfHelmsDeep) . toCardCode) . toList . (.entities.objectives))

defenseTokens :: GameT Int
defenseTokens = getsGame (maybe 0 (countTokens Resource . objectiveTokens . toAttrs) . find ((== toCardCode Objectives.theDefenseOfHelmsDeep) . toCardCode) . toList . (.entities.objectives))

victoryCodes :: GameT [CardCode]
victoryCodes = map toCardCode . scenarioVictoryDisplay <$> scenarioAttrs

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Use each Forced ability the open question offers, in turn.
useForcedAbilities :: PlayerId -> GameT ()
useForcedAbilities pid =
  getQuestion pid >>= \case
    Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> do
      chooseOptionMatching "use a Forced ability" pid \case AbilityLabel {} -> True; _ -> False
      useForcedAbilities pid
    _ -> pure ()

-- | Setup, the group's choice made for stage 1A.
setUpChoosing :: Text -> PlayerId -> GameT ()
setUpChoosing choice pid = do
  flipFirstQuest pid
  chooseLabel ("$label.theKingOfTheGoldenHall." <> choice) pid

-- | Setup, advancing to stage 2A, its When Revealed resolved with Wild Hillman revealed.
toStageTwo :: PlayerId -> GameT ()
toStageTwo pid = do
  flipFirstQuest pid
  _ <- setEncounterDeck [Enemies.wildHillman]
  chooseLabel "$label.theKingOfTheGoldenHall.advance" pid
  resolveWhenRevealed pid
  resolveRevealed

-- | Stage 2 defeated, stage 3A's When Revealed resolved, Helm's Gate found in the deck.
toStageThree :: PlayerId -> GameT ()
toStageThree pid = do
  toStageTwo pid
  _ <- setEncounterDeck [Locations.helmsGate]
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Progress 8
  resolveWhenRevealed pid

-- | Stage 3 defeated, stage 4A's When Revealed resolved, discarding to a Soldier of
-- Isengard.
toStageFour :: PlayerId -> GameT ()
toStageFour pid = do
  toStageThree pid
  _ <- setEncounterDeck [Treacheries.devilryOfSaruman, Enemies.soldierOfIsengard, Locations.theHornburg]
  q <- toTarget <$> getActiveQuest
  run $ PlaceTokens GameSource q Progress 8
  resolveWhenRevealed pid

-- | The Defense of Helm's Deep reaches 8 resource tokens from stage 2, stage 5A's When
-- Revealed resolved, discarding to the given cards.
toStageFive :: PlayerId -> [CardDef] -> GameT ()
toStageFive pid deck = do
  toStageTwo pid
  _ <- setEncounterDeck deck
  o <- defense
  run $ PlaceTokens GameSource (ObjectiveTarget o) Resource 8
  resolveWhenRevealed pid

spec :: Spec
spec = describe "Helm's Deep" do
  it "is a scenario the game can be dealt" . helmsTest $ \_ ->
    isJust (lookupScenario "helms-deep") `shouldBe` True

  -- \* Setup

  describe "setup" do
    it "builds the encounter deck from Helm's Deep and Orcs of the White Hand, setting aside The Defense of Helm's Deep, a Deeping Wall and a Soldier of Isengard for each player" . helmsTest $ \_ -> do
      run $ DoStep 1 Setup
      attrs <- scenarioAttrs
      let deck = attrs.encounterDeck
      all ((`elem` map Just [HelmsDeep, OrcsOfTheWhiteHand]) . cdEncounterSet . toCardDef) deck `shouldBe` True
      map (`copies` deck) [Enemies.wildHillman, Enemies.warriorOfDunland, Enemies.soldierOfIsengard, Locations.deepingWall, Locations.helmsGate, Treacheries.nightWithoutEnd, Enemies.isengardUruk, Treacheries.fightingUrukHai, Treacheries.poisonedCounselsTheTreasonOfSaruman]
        `shouldBe` [3, 2, 3, 3, 2, 2, 3, 2, 0]
      sort (map toCardCode attrs.scenarioSetAside) `shouldBe` sort (map toCardCode [Objectives.theDefenseOfHelmsDeep, Locations.deepingWall, Enemies.soldierOfIsengard])
      map toCardCode attrs.questDeck `shouldBe` map toCardCode [Quests.assaultOnHelmsDeep, Quests.theHostsOfIsengard, Quests.theWallIsBreached, Quests.theKingRidesOut]

    it "shuffles 1 Poisoned Counsels into each player's deck, the stage turning to 1B" . helmsTest $ \pid -> do
      setUpChoosing "poisonedCounsels" pid
      counsels . (.deck) <$> getPlayer pid `shouldReturn` 1
      q <- getActiveQuest
      toCardCode q `shouldBe` toCardCode Quests.theKingOfTheGoldenHall
      q.flipped `shouldBe` True
      _ <- getHeroWithCode "143001"
      pure ()

    it "advances to stage 2A instead, bypassing stage 1B" . helmsTest $ \pid -> do
      toStageTwo pid
      counsels . (.deck) <$> getPlayer pid `shouldReturn` 0
      activeQuestCode `shouldReturn` toCardCode Quests.assaultOnHelmsDeep

    it "is set up the same way in campaign mode, with no hero penalty for the change" . campaignTestIn helmsDeep lotr testDeck $ \pid -> do
      setUpChoosing "poisonedCounsels" pid
      counsels . (.deck) <$> getPlayer pid `shouldReturn` 1
      activeQuestCode `shouldReturn` toCardCode Quests.theKingOfTheGoldenHall

  -- \* Stage 1

  describe "The King of the Golden Hall" do
    it "puts an ally from hand matching a hero's sphere into play, and skips the quest and combat phases" . helmsTest $ \pid -> do
      flipFirstQuest pid
      emptyHand pid
      -- Guard of the Citadel is Leadership, as Théodred; Beorn is Tactics, as no hero is
      _ <- putCardIntoHand pid (toCardDef ("01013" :: CardCode))
      _ <- putCardIntoHand pid (toCardDef ("01031" :: CardCode))
      chooseLabel "$label.theKingOfTheGoldenHall.poisonedCounsels" pid
      resolveWhenRevealed pid
      allies <- getsGame (map toCardCode . toList . (.entities.allies))
      allies `shouldBe` ["01013"]
      getModifiers GameTarget >>= (`shouldSatisfy` \ms -> SkipQuestPhase `elem` ms && SkipPhase CombatPhase `elem` ms)

    it "advances to stage 2A at the end of the round" . helmsTest $ \pid -> do
      flipFirstQuest pid
      emptyHand pid
      chooseLabel "$label.theKingOfTheGoldenHall.poisonedCounsels" pid
      resolveWhenRevealed pid
      _ <- setEncounterDeck [Enemies.wildHillman]
      run $ CheckWindows [mkWhen RoundEnds]
      useForcedAbilities pid
      activeQuestCode `shouldReturn` toCardCode Quests.assaultOnHelmsDeep

  -- \* Stage 2

  describe "Assault on Helm's Deep" do
    it "adds The Defense of Helm's Deep and the Soldiers to the staging area, makes Deeping Wall active and reveals a card for each player" . helmsTest $ \pid -> do
      toStageTwo pid
      _ <- defense
      soldier <- enemyOf Enemies.soldierOfIsengard
      placementOfEnemy soldier `shouldReturn` StagingArea
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.wildHillman))
      activeLocations `shouldReturn` [toCardCode Locations.deepingWall]
      (.scenarioSetAside) <$> scenarioAttrs `shouldReturn` []

    it "has the encounter deck place progress through the active location by questing, and no threat raised (Defense)" . helmsTest $ \pid -> do
      toStageTwo pid
      threat <- threatOf pid
      -- Soldier of Isengard 4 and Wild Hillman 3 against no willpower: Deeping Wall takes 4
      run QuestResolution
      useForcedAbilities pid
      locationOf Locations.deepingWall >>= \_ -> pure ()
      victoryCodes >>= (`shouldSatisfy` notElem (toCardCode Locations.deepingWall))
      activeLocations `shouldReturn` []
      questProgress `shouldReturn` 3
      -- Deeping Wall explored raises each player's threat by 4, questing nothing
      threatOf pid `shouldReturn` (threat + 4)

    it "places no progress when the players quest successfully" . helmsTest $ \pid -> do
      toStageTwo pid
      for_ [Enemies.soldierOfIsengard, Enemies.wildHillman] \def -> enemyOf def >>= run . Discard . EnemyTarget
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ CommitToQuest eowyn
      run QuestResolution
      questProgress `shouldReturn` 0
      (.progress) <$> (locationAttrs =<< locationOf Locations.deepingWall) `shouldReturn` 0

    it "reveals a card at the end of the combat phase with fewer enemies in the staging area than players, and places progress for each enemy there at the end of the round" . helmsTest $ \pid -> do
      toStageTwo pid
      for_ [Enemies.soldierOfIsengard, Enemies.wildHillman] \def -> enemyOf def >>= run . Discard . EnemyTarget
      _ <- setEncounterDeck [Enemies.wildHillman]
      run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
      useForcedAbilities pid
      resolveRevealed
      enemyCodes `shouldReturn` [toCardCode Enemies.wildHillman]
      _ <- stage Enemies.soldierOfIsengard
      run $ CheckWindows [mkWhen RoundEnds]
      useForcedAbilities pid
      questProgress `shouldReturn` 2

    it "advances to stage 3A once the encounter deck defeats it" . helmsTest $ \pid -> do
      toStageThree pid
      activeQuestCode `shouldReturn` toCardCode Quests.theHostsOfIsengard

  -- \* The Defense of Helm's Deep

  describe "The Defense of Helm's Deep" do
    it "takes a resource token at the end of the quest phase, each adding 1 threat to the staging area" . helmsTest $ \pid -> do
      toStageTwo pid
      earlier <- getStagingThreat
      run $ CheckWindows [mkWhen $ PhaseEnds QuestPhase]
      useForcedAbilities pid
      defenseTokens `shouldReturn` 1
      getStagingThreat `shouldReturn` (earlier + 1)

    it "goes to the victory display with 8 tokens, the players advancing to stage 5A" . helmsTest $ \pid -> do
      toStageFive pid [Enemies.wildHillman]
      victoryCodes >>= (`shouldSatisfy` elem (toCardCode Objectives.theDefenseOfHelmsDeep))
      activeQuestCode `shouldReturn` toCardCode Quests.theKingRidesOut

  -- \* Stage 3

  describe "The Hosts of Isengard" do
    it "makes Helm's Gate the active location, returning Deeping Wall to the staging area" . helmsTest $ \pid -> do
      toStageThree pid
      activeLocations `shouldReturn` [toCardCode Locations.helmsGate]
      wall <- locationAttrs =<< locationOf Locations.deepingWall
      wall.locationPlacement `shouldBe` StagingArea

    it "has the first player reveal a card after the quest resolves with no progress placed on it by questing" . helmsTest $ \pid -> do
      toStageThree pid
      for_ [Enemies.soldierOfIsengard, Enemies.wildHillman] \def -> enemyOf def >>= run . Discard . EnemyTarget
      run . Discard . LocationTarget =<< locationOf Locations.deepingWall
      _ <- setEncounterDeck [Enemies.wildHillman]
      inPhase QuestPhase
      run QuestResolution
      useForcedAbilities pid
      resolveRevealed
      enemyCodes `shouldReturn` [toCardCode Enemies.wildHillman]

    it "reveals nothing once questing placed progress on it" . helmsTest $ \pid -> do
      toStageThree pid
      for_ [Enemies.soldierOfIsengard, Enemies.wildHillman] \def -> enemyOf def >>= run . Discard . EnemyTarget
      gate <- locationOf Locations.helmsGate
      run $ Discard (LocationTarget gate)
      _ <- setEncounterDeck [Enemies.wildHillman]
      inPhase QuestPhase
      run QuestResolution
      useForcedAbilities pid
      questProgress >>= (`shouldSatisfy` (> 0))
      length <$> getEncounterDeck `shouldReturn` 1

  -- \* Stage 4

  describe "The Wall is Breached!" do
    it "discards from the encounter deck until an enemy for each player is discarded, adding it to the staging area" . helmsTest $ \pid -> do
      toStageFour pid
      activeQuestCode `shouldReturn` toCardCode Quests.theWallIsBreached
      soldiers <- getsGame (filter ((== toCardCode Enemies.soldierOfIsengard) . toCardCode) . toList . (.entities.enemies))
      length soldiers `shouldBe` 2

    it "takes 1 progress after a character is destroyed, and loses the game once defeated" . helmsTest $ \pid -> do
      toStageFour pid
      earlier <- questProgress
      guard' <- putAllyIntoPlay pid (toCardDef ("01013" :: CardCode))
      run $ DealDamage GameSource guard' 5
      useForcedAbilities pid
      questProgress `shouldReturn` (earlier + 1)
      outcome `shouldReturn` Nothing
      q <- toTarget <$> getActiveQuest
      run $ PlaceTokens GameSource q Progress 8
      outcome `shouldReturn` Just Lost

  -- \* Stage 5

  describe "The King Rides Out" do
    it "skips the quest phase, keeps threat from being reduced, and wins once no enemy is left in play" . helmsTest $ \pid -> do
      toStageFive pid [Treacheries.recklessHate, Enemies.wildHillman]
      getModifiers GameTarget >>= (`shouldSatisfy` elem SkipQuestPhase)
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotReduceThreat)
      outcome `shouldReturn` Nothing
      enemies <- getsGame (map toId . toList . (.entities.enemies))
      for_ enemies (run . Discard . EnemyTarget)
      outcome `shouldReturn` Just Won

    it "earns Poisoned Counsels for each player holding a copy as the campaign is resolved" . campaignTestIn helmsDeep lotr testDeck $ \pid -> do
      flipFirstQuest pid
      emptyHand pid
      chooseLabel "$label.theKingOfTheGoldenHall.poisonedCounsels" pid
      resolveWhenRevealed pid
      _ <- setEncounterDeck [Enemies.wildHillman]
      run $ CheckWindows [mkWhen RoundEnds]
      useForcedAbilities pid
      resolveWhenRevealed pid
      resolveRevealed
      _ <- setEncounterDeck [Enemies.wildHillman]
      o <- defense
      run $ PlaceTokens GameSource (ObjectiveTarget o) Resource 8
      resolveWhenRevealed pid
      enemies <- getsGame (map toId . toList . (.entities.enemies))
      for_ enemies (run . Discard . EnemyTarget)
      outcome `shouldReturn` Just Won
      (.pool) <$> getCampaignLog `shouldReturn` [PoolCard (toCardCode Treacheries.poisonedCounselsTheTreasonOfSaruman) (Just 0) Nothing]

    it "earns nothing for a player with no copy" . campaignTestIn helmsDeep lotr testDeck $ \pid -> do
      toStageFive pid [Enemies.wildHillman]
      enemies <- getsGame (map toId . toList . (.entities.enemies))
      for_ enemies (run . Discard . EnemyTarget)
      outcome `shouldReturn` Just Won
      (.pool) <$> getCampaignLog `shouldReturn` []

  -- \* Encounter cards

  describe "encounter cards" do
    it "Poisoned Counsels discards the rest of its player's hand as it is drawn, and stays there" . helmsTest $ \pid -> do
      toStageTwo pid
      card <- genEncounterCard Treacheries.poisonedCounselsTheTreasonOfSaruman
      run $ PutOnTopOfDeck pid (EncounterCard card)
      hand <- length . (.hand) <$> getPlayer pid
      hand `shouldSatisfy` (> 0)
      run $ DrawCards (Drawing pid 1)
      useForcedAbilities pid
      p <- getPlayer pid
      map toCardCode p.hand `shouldBe` [toCardCode Treacheries.poisonedCounselsTheTreasonOfSaruman]
      length p.discard `shouldBe` hand

    it "Night Without End removes a resource token from The Defense of Helm's Deep, or reveals 2 cards" . helmsTest $ \pid -> do
      toStageTwo pid
      o <- defense
      run $ PlaceTokens GameSource (ObjectiveTarget o) Resource 2
      reveal [Treacheries.nightWithoutEnd]
      chooseLabel "$label.nightWithoutEnd.removeToken" pid
      defenseTokens `shouldReturn` 1

    it "Night Without End reveals 2 cards with no token to remove" . helmsTest $ \pid -> do
      toStageTwo pid
      _ <- setEncounterDeck [Treacheries.nightWithoutEnd, Enemies.wildHillman, Enemies.warriorOfDunland]
      run RevealEncounterCard
      resolveRevealed
      enemyCodes >>= (`shouldSatisfy` elem (toCardCode Enemies.warriorOfDunland))

    it "Scaling Ladders attaches to the active location as a Siege attachment, each enemy getting +1 threat" . helmsTest $ \pid -> do
      toStageTwo pid
      earlier <- getStagingThreat
      reveal [Treacheries.scalingLadders, Locations.theHornburg]
      ladders <- getsGame (fromJustNote "no Scaling Ladders" . find ((== toCardCode Treacheries.scalingLadders) . toCardCode) . toList . (.entities.treacheries))
      wall <- locationOf Locations.deepingWall
      ladders.placement `shouldBe` AttachedTo (LocationTarget wall)
      -- the Soldier and Wild Hillman +1 each, The Hornburg revealed by surge +3
      getStagingThreat `shouldReturn` (earlier + 2 + 3)

    it "Devilry of Saruman places 3 progress on the quest past the active location and blanks each character" . helmsTest $ \pid -> do
      toStageTwo pid
      reveal [Treacheries.devilryOfSaruman]
      questProgress `shouldReturn` 3
      (.progress) <$> (locationAttrs =<< locationOf Locations.deepingWall) `shouldReturn` 0
      (getModifiers . toTarget =<< getHeroWithCode "01007") >>= (`shouldSatisfy` elem Blank)

    it "Reckless Hate has the enemy in the staging area with the highest attack attack you" . helmsTest $ \pid -> do
      toStageTwo pid
      soldier <- enemyOf Enemies.soldierOfIsengard
      _ <- setEncounterDeck [Treacheries.recklessHate, Enemies.wildHillman]
      run RevealEncounterCard
      resolveRevealed
      -- the Soldier's 4 attack is the highest: it attacks
      q <- getQuestion pid
      isJust q `shouldBe` True
      placementOfEnemy soldier `shouldReturn` StagingArea

    it "Reckless Hate gains surge with no enemy in the staging area" . helmsTest $ \pid -> do
      toStageTwo pid
      for_ [Enemies.soldierOfIsengard, Enemies.wildHillman] \def -> enemyOf def >>= run . Discard . EnemyTarget
      _ <- setEncounterDeck [Treacheries.recklessHate, Enemies.wildHillman]
      run RevealEncounterCard
      resolveRevealed
      enemyCodes `shouldReturn` [toCardCode Enemies.wildHillman]

    it "Wild Hillman places 1 progress on the active location after it attacks" . helmsTest $ \pid -> do
      toStageTwo pid
      hillman <- enemyOf Enemies.wildHillman
      run $ EngageEnemy pid hillman
      run $ CheckWindows [mkAfter $ EnemyAttacking hillman pid]
      useForcedAbilities pid
      (.progress) <$> (locationAttrs =<< locationOf Locations.deepingWall) >>= (`shouldSatisfy` (>= 1))

    it "Deeping Wall gives each hero +1 defense while active" . helmsTest $ \pid -> do
      toStageTwo pid
      (getModifiers . toTarget =<< getHeroWithCode "01007") >>= (`shouldSatisfy` elem (DefenseModifier 1))

    it "The Hornburg reduces the archery total by 1 for each player while active" . helmsTest $ \pid -> do
      toStageTwo pid
      wall <- locationOf Locations.deepingWall
      run $ Discard (LocationTarget wall)
      earlier <- getArcheryTotal
      _ <- activeAt Locations.theHornburg
      getArcheryTotal `shouldReturn` max 0 (earlier - 1)

    it "Deeping Culvert has 1 more threat than the first player's allies" . helmsTest $ \pid -> do
      toStageTwo pid
      culvert <- stageLocation Locations.deepingCulvert
      getLocationThreat culvert `shouldReturn` 1
      _ <- putAllyIntoPlay pid (toCardDef ("01013" :: CardCode))
      getLocationThreat culvert `shouldReturn` 2

    it "Postern Door, explored, keeps the players from declaring attacks this round" . helmsTest $ \pid -> do
      toStageTwo pid
      wall <- locationOf Locations.deepingWall
      run $ Discard (LocationTarget wall)
      door <- activeAt Locations.posternDoor
      (getModifiers . toTarget =<< getHeroWithCode "01007") >>= (`shouldSatisfy` elem (AttackModifier 1))
      run $ PlaceTokens GameSource (LocationTarget door) Progress 2
      useForcedAbilities pid
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotAttack)

    it "Helm's Gate reveals a card at the end of the staging step while a player is not engaged" . helmsTest $ \pid -> do
      toStageThree pid
      _ <- setEncounterDeck [Enemies.wildHillman]
      run $ CheckWindows [mkAfter StagingStepEnds]
      useForcedAbilities pid
      resolveRevealed
      length . filter (== toCardCode Enemies.wildHillman) <$> enemyCodes `shouldReturn` 2
