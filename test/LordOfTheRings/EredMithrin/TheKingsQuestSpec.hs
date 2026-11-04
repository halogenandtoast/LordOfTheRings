module LordOfTheRings.EredMithrin.TheKingsQuestSpec (spec) where

import LordOfTheRings.Attachment (AttachmentAttrs (..))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card (cdEncounterSet, toCardDef)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Location (LocationAttrs (..))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (playerCardsGuardedBy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theKingsQuest)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), cavesDeckL, questDeckL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter)
import TestImport

kqTest :: (PlayerId -> GameT ()) -> IO ()
kqTest = gameTestIn theKingsQuest testDeck

scenarioAttrs :: GameT ScenarioAttrs
scenarioAttrs = getsGame (toAttrs . (.scenario))

-- | The stage in play, on its B side, its When Revealed passed by.
flippedStage :: CardDef -> GameT Target
flippedStage def = do
  q <- putQuestStage def
  g <- (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) <$> getGame
  putGame g
  pure q

setQuestDeck :: [CardDef] -> GameT ()
setQuestDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (questDeckL .~ cards)

setSetAside :: [CardDef] -> GameT ()
setSetAside defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (setAsideL .~ cards)

setCavesDeck :: [CardDef] -> GameT [EncounterCard]
setCavesDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (cavesDeckL .~ cards)
  pure cards

cavesCodes :: GameT [CardCode]
cavesCodes = map toCardCode . (.cavesDeck) <$> scenarioAttrs

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

placementOfEnemy :: EnemyId -> GameT (Maybe Placement)
placementOfEnemy eid = getsGame (fmap (.placement) . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

activeCodes :: GameT [CardCode]
activeCodes = getsGame (map toCardCode . filter ((== ActiveLocation) . (.placement)) . toList . (.entities.locations))

-- | Locations put in the victory display.
toVictoryDisplay :: [CardDef] -> GameT ()
toVictoryDisplay defs = for_ defs \d -> run . AddToVictoryDisplay . toTarget =<< stageLocation d

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

inPhase :: Phase -> GameT ()
inPhase p = putGame . (phaseL .~ p) =<< getGame

questProgress :: GameT Int
questProgress = (.progress) <$> getActiveQuest

chooseLabelStarting :: HasCallStack => Text -> PlayerId -> GameT ()
chooseLabelStarting key pid = chooseOptionMatching ("choose " <> key) pid \case
  Label l _ -> key `isPrefixOf` l
  _ -> False

spec :: Spec
spec = describe "The King's Quest" do
  it "is a scenario the game can be dealt" . kqTest $ \_ ->
    isJust (lookupScenario "the-kings-quest") `shouldBe` True

  describe "setup" do
    it "builds the Caves deck, sets the drake, the hoard and Dragon Might aside, and stages an enemy or location" . kqTest $ \pid -> do
      flipFirstQuest pid
      s <- scenarioAttrs
      let setOf c = cdEncounterSet (toCardDef c)
      map setOf s.cavesDeck `shouldBe` replicate 6 (Just EncounterSet.LostCaves)
      sort (map toCardCode s.scenarioSetAside)
        `shouldBe` sort (map toCardCode [Enemies.fireDrake, Locations.dragonHoard, Treacheries.dragonBreath, Treacheries.dragonBreath, Treacheries.dragonScales, Treacheries.dragonScales])
      filter ((`elem` [Just EncounterSet.LostCaves, Just EncounterSet.DragonMight]) . setOf) s.encounterDeck `shouldBe` []
      staged <- getsGame \g -> length g.entities.enemies + length g.entities.locations
      staged `shouldBe` 1
      activeQuestCode `shouldReturn` toCardCode Quests.theIronHills
      (.flipped) . toAttrs <$> getActiveQuest `shouldReturn` True

  describe "stage 1B" do
    it "discards to a location at the beginning of the quest phase, raising each player's threat by its printed threat" . kqTest $ \pid -> do
      _ <- flippedStage Quests.theIronHills
      _ <- setEncounterDeck [Treacheries.denizenOfTheDeep, Locations.ironHillsMine]
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      chooseLabelStarting "$label.theIronHills.threat" pid
      threatOf pid `shouldReturn` 4
      map toCardCode <$> getEncounterDiscard `shouldReturn` map toCardCode [Treacheries.denizenOfTheDeep, Locations.ironHillsMine]

    it "or adds that location to the staging area" . kqTest $ \pid -> do
      _ <- flippedStage Quests.theIronHills
      _ <- setEncounterDeck [Locations.deepChasm]
      run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
      chooseLabel "$label.theIronHills.staging" pid
      threatOf pid `shouldReturn` 0
      void $ locationOf Locations.deepChasm

    it "is defeated once 3 locations with victory points are in the victory display" . kqTest $ \_ -> do
      _ <- flippedStage Quests.theIronHills
      setQuestDeck [Quests.theLowerDepths]
      toVictoryDisplay [Locations.lostArmory, Locations.ancientTreasury, Locations.forkedPassage]
      activeQuestCode `shouldReturn` toCardCode Quests.theIronHills
      toVictoryDisplay [Locations.frightfulDen]
      activeQuestCode `shouldReturn` toCardCode Quests.theLowerDepths

  describe "stage 2" do
    it "puts Dragon Hoard under the Caves deck, adds Fire-drake and shuffles Dragon Might in" . kqTest $ \pid -> do
      setSetAside [Enemies.fireDrake, Locations.dragonHoard, Treacheries.dragonBreath, Treacheries.dragonScales]
      _ <- setCavesDeck [Locations.lostArmory]
      _ <- setEncounterDeck [Locations.ironHillsMine]
      q <- putQuestStage Quests.theLowerDepths
      run $ Flip q
      resolveWhenRevealed pid
      cavesCodes `shouldReturn` map toCardCode [Locations.lostArmory, Locations.dragonHoard]
      drake <- enemyOf Enemies.fireDrake
      placementOfEnemy drake `shouldReturn` Just StagingArea
      map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` \d -> all (`elem` d) [toCardCode Treacheries.dragonBreath, toCardCode Treacheries.dragonScales])
      scenarioSetAside <$> scenarioAttrs `shouldReturn` []
      mods <- getModifiers drake
      mods `shouldSatisfy` \ms -> all (`elem` ms) [CannotLeaveStagingArea, CannotTakeDamage]
      run $ EngageEnemy pid drake
      placementOfEnemy drake `shouldReturn` Just StagingArea

    it "lets the players remove 5 progress after questing to arrange the Caves deck" . kqTest $ \pid -> do
      q <- flippedStage Quests.theLowerDepths
      [a, b, c] <- setCavesDeck [Locations.lostArmory, Locations.ancientTreasury, Locations.frightfulDen]
      run $ PlaceProgressOnQuest 6
      run $ CheckWindows [mkAfter PlacedProgressFromQuesting]
      chooseAbilityOf pid q
      chooseTarget pid (CardIdTarget b.id)
      map (.id) . (.cavesDeck) <$> scenarioAttrs `shouldReturn` map (.id) [b, c, a]
      questProgress `shouldReturn` 1

    it "does not offer the Caves deck without progress placed this phase" . kqTest $ \pid -> do
      q <- flippedStage Quests.theLowerDepths
      _ <- setCavesDeck [Locations.lostArmory, Locations.ancientTreasury]
      run $ PlaceTokens GameSource q Progress 6
      run $ EndPhase QuestPhase
      run $ CheckWindows [mkAfter PlacedProgressFromQuesting]
      offersAbilityOf pid q `shouldReturn` False

    it "advances to stage 3A as Dragon Hoard becomes the active location" . kqTest $ \_ -> do
      _ <- flippedStage Quests.theLowerDepths
      setQuestDeck [Quests.theFireWorm]
      _ <- activeLocation Locations.dragonHoard
      activeQuestCode `shouldReturn` toCardCode Quests.theFireWorm

  describe "stage 3" do
    it "attaches a Dragon treachery from the encounter deck to Fire-drake and ignores Deep" . kqTest $ \pid -> do
      drake <- stage Enemies.fireDrake
      [scales, _] <- setEncounterDeck [Treacheries.dragonScales, Locations.ironHillsMine]
      breath <- genEncounterCard Treacheries.dragonBreath
      run $ AddToEncounterDiscard [breath]
      q <- putQuestStage Quests.theFireWorm
      run $ Flip q
      resolveWhenRevealed pid
      chooseTarget pid (CardIdTarget scales.id)
      getsGame (map (\t -> (toCardCode t, t.placement)) . toList . (.entities.treacheries))
        `shouldReturn` [(toCardCode Treacheries.dragonScales, AttachedTo (EnemyTarget drake))]
      map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.ironHillsMine]
      getModifiers GameTarget >>= (`shouldSatisfy` elem IgnoreDeep)
      getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)

    it "makes an attacker against Fire-drake remove progress from the main quest" . kqTest $ \pid -> do
      q <- flippedStage Quests.theFireWorm
      drake <- stage Enemies.fireDrake
      run $ PlaceTokens GameSource q Progress 3
      run $ EngageEnemy pid drake
      beravor <- hero "01012"
      -- 3 attack against its 2 defense
      run $ CreateEffect $ Effect GameSource beravor [AttackModifier 1] UntilEndOfRound
      inPhase CombatPhase
      run $ DeclareAttackers pid drake
      chooseTarget pid beravor
      chooseLabel "$label.doneAttacking" pid
      chooseLabelStarting "$label.removeMainQuestProgress" pid
      questProgress `shouldReturn` 2
      passActions pid
      getDamage (toTarget drake) `shouldReturn` 1

    it "cancels the attack without the progress to remove" . kqTest $ \pid -> do
      _ <- flippedStage Quests.theFireWorm
      drake <- stage Enemies.fireDrake
      run $ EngageEnemy pid drake
      inPhase CombatPhase
      run $ DeclareAttackers pid drake
      chooseTarget pid =<< hero "01012"
      chooseLabel "$label.doneAttacking" pid
      passActions pid
      getDamage (toTarget drake) `shouldReturn` 0
      heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

    it "is won as Fire-drake is defeated" . kqTest $ \_ -> do
      _ <- flippedStage Quests.theFireWorm
      drake <- stage Enemies.fireDrake
      run $ Destroy GameSource (EnemyTarget drake)
      getsGame (.outcome) `shouldReturn` Just Won

  describe "Fire-drake" do
    it "has threat and defense 1 more than the number of players, and no player attachments" . kqTest $ \_ -> do
      drake <- stage Enemies.fireDrake
      getEnemyThreat drake `shouldReturn` 2
      getEnemyDefense drake `shouldReturn` 2
      getModifiers drake >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)

    it "returns to the staging area and attacks each player as the active location is explored" . kqTest $ \pid -> do
      drake <- stage Enemies.fireDrake
      run $ EngageEnemy pid drake
      lid <- activeLocation Locations.deepChasm
      run $ PlaceTokens GameSource (toTarget lid) Progress 6
      placementOfEnemy drake `shouldReturn` Just StagingArea
      passActions pid
      Just (QuestionLabel l _ _ _) <- getQuestion pid
      l `shouldSatisfy` ("$label.declareDefender" `isPrefixOf`)

  describe "Dragon Hoard" do
    it "is immune to player card effects and keeps Fire-drake from taking damage" . kqTest $ \_ -> do
      drake <- stage Enemies.fireDrake
      hoard <- stageLocation Locations.dragonHoard
      getModifiers hoard >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ DealDamage GameSource (toTarget drake) 3
      getDamage (toTarget drake) `shouldReturn` 0

    it "guards an Artifact from hand once active, which its owner attaches once it is explored" . kqTest $ \pid -> do
      stone <- putCardIntoHand pid Attachments.celebriansStone
      hoard <- activeLocation Locations.dragonHoard
      chooseAbilityOf pid hoard
      cid <- locationCardId . toAttrs <$> getLocation hoard
      map (.id) <$> playerCardsGuardedBy cid `shouldReturn` [stone.id]
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` notElem stone.id)
      run $ PlaceTokens GameSource (toTarget hoard) Progress 7
      beravor <- hero "01012"
      chooseTarget pid beravor
      getsGame (map (attachmentPlacement . toAttrs) . filter ((== stone.id) . attachmentCardId . toAttrs) . toList . (.entities.attachments))
        `shouldReturn` [AttachedTo beravor]

  describe "Iron Hills Mine" do
    it "has each player discard the top 4 cards of his deck to travel there" . kqTest $ \pid -> do
      lid <- stageLocation Locations.ironHillsMine
      was <- length . (.deck) <$> getPlayer pid
      run $ TravelTo lid
      length . (.deck) <$> getPlayer pid `shouldReturn` (was - 4)

    it "lets each player return a card from his discard pile as it is explored while active" . kqTest $ \pid -> do
      lid <- activeLocation Locations.ironHillsMine
      run $ DiscardTopOfDeck pid 1
      [card] <- (.discard) <$> getPlayer pid
      run $ PlaceTokens GameSource (toTarget lid) Progress 4
      chooseAbilityOf pid lid
      chooseTarget pid card
      map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)

  describe "Contested Depths" do
    it "gets +1 threat for each location in the victory display, and raises threat by as much as the players travel there" . kqTest $ \pid -> do
      toVictoryDisplay [Locations.lostArmory, Locations.ironHillsMine]
      lid <- stageLocation Locations.contestedDepths
      getLocationThreat lid `shouldReturn` 3
      _ <- setCavesDeck [Locations.ancientTreasury]
      run $ TravelTo lid
      chooseLabelStarting "$label.contestedDepths.threat" pid
      threatOf pid `shouldReturn` 2

    it "or is replaced by the top card of the Caves deck" . kqTest $ \pid -> do
      lid <- stageLocation Locations.contestedDepths
      _ <- setCavesDeck [Locations.ironHillsMine]
      run $ TravelTo lid
      chooseLabel "$label.contestedDepths.replace" pid
      activeCodes `shouldReturn` [toCardCode Locations.ironHillsMine]

  describe "Deep Chasm" do
    it "must be traveled to while there is no active location" . kqTest $ \_ -> do
      lid <- stageLocation Locations.deepChasm
      getModifiers lid >>= (`shouldSatisfy` elem MustBeTraveledTo)
      _ <- activeLocation Locations.ironHillsMine
      getModifiers lid >>= (`shouldSatisfy` notElem MustBeTraveledTo)

  describe "Denizen of the Deep" do
    it "has the highest attack enemy in the staging area attack the first player" . kqTest $ \pid -> do
      _ <- stage Enemies.fireDrake
      _ <- setEncounterDeck [Treacheries.denizenOfTheDeep]
      run RevealEncounterCard
      resolveRevealed
      passActions pid
      Just (QuestionLabel l _ _ _) <- getQuestion pid
      l `shouldSatisfy` ("$label.declareDefender" `isPrefixOf`)

    it "surges with no enemy in the staging area" . kqTest $ \_ -> do
      _ <- setEncounterDeck [Treacheries.denizenOfTheDeep, Locations.ironHillsMine]
      run RevealEncounterCard
      resolveRevealed
      void $ locationOf Locations.ironHillsMine
