module LordOfTheRings.AngmarAwakened.NightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card (CardType (HeroType), cdCardType, toCard, toCardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer), getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase))
import LordOfTheRings.Scenario (escapeFromMountGram, theWastesOfEriador)
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedDeck, capturedUnder)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)
import LordOfTheRings.Scenario.Types (GameMode (..), encounterDeckL, modeL, scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (RoundEnds))
import TestImport

nightmare :: Scenario -> (PlayerId -> GameT ()) -> IO ()
nightmare s = gameTestIn (overAttrs (modeL .~ NightmareMode) s) testDeck

-- | Reveal cards from the encounter deck, the first on top.
reveal :: [CardDef] -> GameT ()
reveal defs = setEncounterDeck defs >> run RevealEncounterCard >> resolveRevealed

copies :: CardDef -> [EncounterCard] -> Int
copies def = length . filter ((== toCardCode def) . toCardCode)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

-- | Use every forced ability offered, one at a time, then pass the responses offered.
resolveForced :: PlayerId -> GameT ()
resolveForced pid =
  getQuestion pid >>= \case
    Just (ChooseOneAtATime (AbilityLabel {} : _)) -> chooseAbility pid >> resolveForced pid
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> resolveForced pid
    _ -> pure ()

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

cardOf :: CardDef -> GameT CardId
cardOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ fromJustNote "no such card"
    $ headMay
    $ [(toCard l).id | l <- toList g.entities.locations, toCardCode l == code]
    <> [(toCard e).id | e <- toList g.entities.enemies, toCardCode e == code]
    <> [(toCard q).id | q <- toList g.entities.quests, toCardCode q == code]

-- | An ally of the player's captured underneath the card.
captureAlly :: PlayerId -> CardDef -> Target -> GameT Card
captureAlly pid def host = do
  ally <- putAllyIntoPlay pid def
  card <- getsGame (toCard . fromJustNote "no ally" . find ((== ally) . toTarget) . toList . (.entities.allies))
  run $ PlaceUnderneath host [card]
  pure card

-- | Player cards shuffled into the player's captured deck.
intoCapturedDeck :: PlayerId -> [CardDef] -> GameT ()
intoCapturedDeck pid defs = do
  cards <- traverse (`genPlayerCard` Just pid) defs
  run $ ScenarioSpecific "mountGram.shuffleIn" (toJSON (pid, cards))

timeShowing :: CardDef -> GameT ()
timeShowing side = run . AddToStaging =<< genEncounterCard side

timeObjective :: GameT ObjectiveId
timeObjective = getsGame (fromJustNote "no Time objective" . fmap toId . headMay . toList . (.entities.objectives))

spec :: Spec
spec = describe "Angmar Awakened Nightmare mode" do
  describe "The Wastes of Eriador" do
    it "builds the nightmare deck: removals out, the nightmare set in" . nightmare theWastesOfEriador $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.northernWarg, Enemies.whiteWarg, Treacheries.bitingWind, Enemies.packHunters, Quests.makeAStand]
        `shouldBe` [2, 2, 0, 5, 1]
      copies Nightmare.theWastesOfEriadorNightmare deck `shouldBe` 0

    it "Pack Hunters loses 30 engagement cost at Night, and finds a Warg as it engages" . nightmare theWastesOfEriador $ \pid -> do
      timeShowing Objectives.nightfall
      hunters <- staged Enemies.packHunters
      getModifiers (toTarget hunters) >>= (`shouldSatisfy` elem (EngagementCostModifier (-30)))
      [_, warg] <- setEncounterDeck [Locations.northDowns, Enemies.wolfOfAngmar]
      run $ EngageEnemy pid hunters
      resolveForced pid
      getQuestion pid >>= \case
        Just _ -> chooseTarget pid (CardIdTarget warg.id)
        Nothing -> pure ()
      resolveForced pid
      wolf <- enemyOf Enemies.wolfOfAngmar
      (.placement) <$> getEnemy wolf `shouldReturn` EngagedWith pid

    it "Shrouded Wilderness turns Day to Night as the players travel there" . nightmare theWastesOfEriador $ \pid -> do
      timeShowing Objectives.daybreak
      wild <- stagedLocation Locations.shroudedWilderness
      _ <- setEncounterDeck [Locations.northDowns]
      run $ TravelTo wild
      resolveForced pid
      isNight `shouldReturn` True
      getModifiers (toTarget wild) >>= (`shouldSatisfy` elem (ProgressLimit 0))

    it "Snowy Badlands keeps damage off the enemies in the staging area" . nightmare theWastesOfEriador $ \_ -> do
      warg <- staged Enemies.northernWarg
      _ <- stagedLocation Locations.snowyBadlands
      run $ DealDamage GameSource (toTarget warg) 2
      (.damage) <$> getEnemy warg `shouldReturn` 0

    it "Cunning Wolves adds 3 quest points to the current quest and bars engaging Wargs" . nightmare theWastesOfEriador $ \_ -> do
      q <- putQuestStage Quests.acrossTheWastes
      warg <- staged Enemies.northernWarg
      reveal [Treacheries.cunningWolves]
      getModifiers q >>= (`shouldSatisfy` elem (QuestPointModifier 3))
      getModifiers (toTarget warg) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))

    it "Torrent of Hail deals 1 damage to each questing character" . nightmare theWastesOfEriador $ \_ -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ CommitToQuest eowyn
      reveal [Treacheries.torrentOfHail]
      getCharacterDamage eowyn `shouldReturn` 1

    it "Make a Stand turns Day to Night and keeps it Night" . nightmare theWastesOfEriador $ \pid -> do
      timeShowing Objectives.daybreak
      _ <- setEncounterDeck [Locations.northDowns, Locations.northDowns]
      card <- genEncounterCard Quests.makeAStand
      g <- getGame
      putGame $ g & scenarioL %~ overAttrs (encounterDeckL %~ (card :))
      run RevealEncounterCard
      resolveRevealed
      resolveForced pid
      isNight `shouldReturn` True
      o <- timeObjective
      getModifiers (toTarget o) >>= (`shouldSatisfy` elem CannotBeFlipped)
      run $ CheckWindows [Window After RoundEnds]
      resolveForced pid
      isNight `shouldReturn` True

    it "Forced Off Track removes a progress from the main quest per Warg at the end of the round" . nightmare theWastesOfEriador $ \pid -> do
      q <- putQuestStage Quests.acrossTheWastes
      run $ PlaceTokens GameSource q Progress 5
      _ <- staged Enemies.northernWarg
      _ <- staged Enemies.whiteWarg
      run . AddToStaging =<< genEncounterCard Quests.forcedOffTrack
      run $ CheckWindows [Window After RoundEnds]
      resolveForced pid
      quests <- getsGame (toList . (.entities.quests))
      [p | p <- quests, toCardCode p == toCardCode Quests.acrossTheWastes] & map (.progress) & (`shouldBe` [3])

  describe "Escape from Mount Gram" do
    it "builds the nightmare deck, the nightmare Jailor and Orc of Thaurdir set aside" . nightmare escapeFromMountGram $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.prisonCell, Locations.prisonCellNightmare, Enemies.dungeonGuard, Enemies.prisonGuard, Enemies.orcOfThaurdir]
        `shouldBe` [0, 4, 0, 3, 0]
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      map (`copies` aside) [Enemies.jailorGornakh, Enemies.jailorGornakhNightmare, Enemies.orcOfThaurdir] `shouldBe` [0, 1, 3]

    it "puts the hero set aside on the bottom of the captured deck" . nightmare escapeFromMountGram $ \pid -> do
      flipFirstQuest pid
      chooseTarget pid =<< getHeroWithCode "01007"
      deck <- capturedDeck pid
      fmap ((== HeroType) . cdCardType . toCardDef) (lastMay deck) `shouldBe` Just True

    it "cannot be won while a hero is captured" . nightmare escapeFromMountGram $ \pid -> do
      flight <- putQuestStage Quests.flightFromMountGram
      intoCapturedDeck pid [Heroes.aragorn]
      run $ PlaceTokens GameSource flight Progress 14
      getsGame (.outcome) `shouldReturn` Nothing

    it "Prison Guard rescues a captured card at random, raising each threat by its cost" . nightmare escapeFromMountGram $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.prisonGuard
      guard' <- enemyOf Enemies.prisonGuard
      _ <- captureAlly pid Allies.silverlodeArcher (toTarget guard')
      run $ EngageEnemy pid guard'
      threat <- threatOf pid
      run $ BeginPhase PlanningPhase
      chooseAbilityOf pid guard'
      threatOf pid `shouldReturn` threat + 3
      (capturedUnder =<< cardOf Enemies.prisonGuard) `shouldReturn` []

    it "Torture Chamber has twice its captured cards in quest points, and hurts the exhausted" . nightmare escapeFromMountGram $ \pid -> do
      chamber <- stagedLocation Locations.tortureChamberMountGram
      _ <- captureAlly pid Allies.silverlodeArcher (toTarget chamber)
      getModifiers (toTarget chamber) >>= (`shouldSatisfy` elem (QuestPointModifier 2))
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ Exhaust eowyn
      run $ EndPhase CombatPhase
      resolveForced pid
      getCharacterDamage eowyn `shouldReturn` 1

    it "Cruel Torment trims each hand to 6 cards" . nightmare escapeFromMountGram $ \pid -> do
      _ <- putQuestStage Quests.flightFromMountGram
      replicateM_ 8 $ putCardIntoHand pid Allies.silverlodeArcher
      reveal [Treacheries.cruelTorment, Locations.prisonCellNightmare]
      getQuestion pid >>= (`shouldSatisfy` isJust)

    it "Goblin Patrol raises each threat by the cards left in hand" . nightmare escapeFromMountGram $ \pid -> do
      _ <- putQuestStage Quests.flightFromMountGram
      replicateM_ 3 $ putCardIntoHand pid Allies.silverlodeArcher
      threat <- threatOf pid
      reveal [Treacheries.goblinPatrolNightmare]
      -- capture 0, 1, 2 or 3: capture 2
      void $ answerQuestion pid 2
      threatOf pid `shouldReturn` threat + 1
      length . (.hand) <$> getPlayer pid `shouldReturn` 1

    it "Orc of Thaurdir's shadow discards from the captured deck per damage" . nightmare escapeFromMountGram $ \pid -> do
      intoCapturedDeck pid [Allies.silverlodeArcher, Allies.silverlodeArcher, Allies.silverlodeArcher]
      orc <- staged Enemies.northernWarg
      card <- genEncounterCard Enemies.orcOfThaurdir
      run $ AttachShadowCard orc card
      run $ ResolveShadow orc pid []
      eowyn <- getHeroWithCode "01007"
      run $ DealDamage (EnemyAttackSource orc) (toTarget eowyn) 2
      length <$> capturedDeck pid `shouldReturn` 1

    it "the nightmare Jailor Gornákh captures each hero from the captured decks" . nightmare escapeFromMountGram $ \pid -> do
      intoCapturedDeck pid [Heroes.aragorn, Allies.silverlodeArcher]
      run . AddToStaging =<< genEncounterCard Enemies.jailorGornakhNightmare
      resolveForced pid
      map toCardCode <$> (capturedUnder =<< cardOf Enemies.jailorGornakhNightmare) `shouldReturn` [toCardCode Heroes.aragorn]
      length <$> capturedDeck pid `shouldReturn` 1

    it "Raid Golfimbul's Vault captures a card from each hand as another card captures" . nightmare escapeFromMountGram $ \pid -> do
      run . AddToStaging =<< genEncounterCard Quests.raidGolfimbulsVault
      _ <- putCardIntoHand pid Allies.silverlodeArcher
      cell <- stagedLocation Locations.prisonCellNightmare
      _ <- captureAlly pid Allies.guardOfTheCitadel (toTarget cell)
      length <$> (capturedUnder =<< cardOf Quests.raidGolfimbulsVault) `shouldReturn` 1
