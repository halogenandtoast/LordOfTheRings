module LordOfTheRings.Haradrim.TheLongArmOfMordorSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getEngagementCost, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (CardMatcher (AnyCard), HeroMatcher (..), PlayerMatcher (PlayerWithId), select)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase, ResourcePhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theLongArmOfMordor)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), questDeckL, setAsideL)
import LordOfTheRings.Sphere (Sphere (Leadership))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (CommittedToQuest, DeclaredAsAttacker, PhaseBegins, PlacedProgressFromQuesting, RoundEnds), mkAfter)
import TestImport

longArmTest :: (PlayerId -> GameT ()) -> IO ()
longArmTest = gameTestIn theLongArmOfMordor testDeck

starting :: [CardCode]
starting = ["01012", "01007", "01002"]

setAside :: GameT [EncounterCard]
setAside = getsGame (scenarioSetAside . toAttrs . (.scenario))

-- | The game set up, the player taking Kahliel as his objective-hero.
setUp :: PlayerId -> GameT ()
setUp pid = do
  flipFirstQuest pid
  chooseObjectiveHero pid Objectives.kahliel

chooseObjectiveHero :: PlayerId -> CardDef -> GameT ()
chooseObjectiveHero pid def = do
  card <- fromJustNote "not set aside" . find ((== toCardCode def) . toCardCode) <$> setAside
  chooseTarget pid (CardIdTarget card.id)

heroIn :: CardCode -> GameT HeroAttrs
heroIn code = toAttrs <$> getHeroWithCode code

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

controlled :: PlayerId -> GameT [CardCode]
controlled pid = do
  hids <- select (HeroControlledBy $ PlayerWithId pid)
  getsGame \g -> [toCardCode h | h <- toList g.entities.heroes, toId h `elem` hids]

-- | Heroes put in the staging area, as stage 1A's setup does.
toStaging :: [CardCode] -> GameT ()
toStaging codes = for_ codes \c -> run . MoveHeroToStagingArea . toId =<< getHeroWithCode c

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such enemy" . fmap toId . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

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

spec :: Spec
spec = describe "The Long Arm of Mordor" do
  it "is a scenario the game can be dealt" . longArmTest $ \_ ->
    isJust (lookupScenario "the-long-arm-of-mordor") `shouldBe` True

  describe "setup" do
    it "moves the heroes to the staging area, gives the player his objective-hero with 2 resources, and adds Sauron's Enforcers" . longArmTest $ \pid -> do
      setUp pid
      for_ starting \c -> heroInStagingArea <$> heroIn c `shouldReturn` True
      controlled pid `shouldReturn` [toCardCode Objectives.kahliel]
      kahliel <- getHeroWithCode (toCardCode Objectives.kahliel)
      kahliel.resources `shouldBe` 2
      kahliel.owner `shouldBe` pid
      enemies <- getsGame (map toCardCode . toList . (.entities.enemies))
      enemies `shouldBe` [toCardCode Enemies.sauronsEnforcers]
      -- Uruk Chieftain waits aside; the objective-heroes no one chose are gone
      map toCardCode <$> setAside `shouldReturn` [toCardCode Enemies.urukChieftain]
      deck <- getEncounterDeck
      filter ((`elem` map toCardCode [Objectives.firyal, Objectives.yazan, Objectives.jubayr, Enemies.urukChieftain]) . toCardCode) deck `shouldBe` []
      activeQuestCode `shouldReturn` toCardCode Quests.surpriseAttack

  describe "heroes in the staging area" do
    it "are blank, immune to player card effects, and collect no resources" . longArmTest $ \pid -> do
      setUp pid
      h <- hero "01012"
      mods <- getModifiers h
      (Blank `elem` mods, ImmuneToPlayerCardEffects `elem` mods) `shouldBe` (True, True)
      run $ DoPhase ResourcePhase
      (.resources) <$> heroIn "01012" `shouldReturn` 0
      (.resources) <$> getHeroWithCode (toCardCode Objectives.kahliel) `shouldReturn` 3

    it "take the damage of undefended attacks while stages 1B and 2B are in play" . longArmTest $ \pid -> do
      setUp pid
      staged <- traverse hero starting
      mods <- getModifiers (PlayerTarget pid)
      sort [t | UndefendedDamageTo t <- mods] `shouldBe` sort staged

    it "lets the first card a player plays each phase ignore resource match" . longArmTest $ \pid -> do
      setUp pid
      kahliel <- hero (toCardCode Objectives.kahliel)
      getModifiers kahliel >>= (`shouldSatisfy` elem (GainResourceIconFor AnyCard Leadership))
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [PlayedACardThisPhase] UntilEndOfPhase
      getModifiers kahliel >>= (`shouldSatisfy` notElem (GainResourceIconFor AnyCard Leadership))

    it "is destroyed to its owner's discard pile with no hit points left" . longArmTest $ \pid -> do
      setUp pid
      h <- hero "01002"
      run $ DealDamage GameSource h 10
      getsGame (any ((== "01002") . toCardCode) . (.entities.heroes)) `shouldReturn` False
      (any ((== "01002") . toCardCode) . (.discard) <$> getPlayer pid) `shouldReturn` True
      (.eliminated) <$> getPlayer pid `shouldReturn` False

  describe "objective-heroes" do
    it "eliminate their controller as they leave play" . longArmTest $ \pid -> do
      setUp pid
      kahliel <- hero (toCardCode Objectives.kahliel)
      run $ Discard kahliel
      (.eliminated) <$> getPlayer pid `shouldReturn` True
      getsGame (.outcome) `shouldReturn` Just Lost
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Objectives.kahliel))

    it "has Kahliel add a resource to a hero at the beginning of the planning phase" . longArmTest $ \pid -> do
      setUp pid
      kahliel <- hero (toCardCode Objectives.kahliel)
      run $ CheckWindows [mkAfter $ PhaseBegins PlanningPhase]
      chooseAbilityOf pid kahliel
      (.resources) <$> getHeroWithCode (toCardCode Objectives.kahliel) `shouldReturn` 3

    it "has Yazan deal 1 damage to a non-unique enemy after he attacks" . longArmTest $ \pid -> do
      flipFirstQuest pid
      chooseObjectiveHero pid Objectives.yazan
      yazan <- hero (toCardCode Objectives.yazan)
      enforcers <- getsGame (toId . fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
      run $ CheckWindows [mkAfter $ DeclaredAsAttacker yazan enforcers]
      chooseAbilityOf pid yazan
      getDamage (EnemyTarget enforcers) `shouldReturn` 1

    it "has Firyal look at the top encounter card and discard it after she commits" . longArmTest $ \pid -> do
      flipFirstQuest pid
      chooseObjectiveHero pid Objectives.firyal
      firyal <- hero (toCardCode Objectives.firyal)
      [top, _] <- setEncounterDeck [Locations.jungleTrail, Locations.jungleTrail]
      run $ CheckWindows [mkAfter $ CommittedToQuest firyal]
      chooseAbilityOf pid firyal
      chooseLabel "$label.discardIt" pid
      map (.id) <$> getEncounterDiscard `shouldReturn` [top.id]

    it "readies an objective-hero defending against Southron Boma's shadow, and cancels the damage" . longArmTest $ \pid -> do
      setUp pid
      kahliel <- hero (toCardCode Objectives.kahliel)
      eid <- getsGame (toId . fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
      run $ EngageEnemy pid eid
      _ <- setEncounterDeck [Locations.southronBoma]
      run $ DealShadowCard eid
      putGame . (phaseL .~ CombatPhase) =<< getGame
      run $ EnemyAttacks pid
      passActions pid
      chooseTarget pid kahliel
      passActions pid
      resolveShadowsInOrder pid
      passActions pid
      getDamage kahliel `shouldReturn` 0
      heroExhausted <$> heroIn (toCardCode Objectives.kahliel) `shouldReturn` False

  describe "stages" do
    it "has stage 1B give each player a hero back, exhausted, and advance after progress from questing" . longArmTest $ \pid -> do
      setUp pid
      run $ CheckWindows [mkAfter PlacedProgressFromQuesting]
      chooseTarget pid =<< hero "01007"
      controlled pid >>= (`shouldSatisfy` elem "01007")
      h <- heroIn "01007"
      (heroInStagingArea h, heroExhausted h) `shouldBe` (False, True)
      activeQuestCode `shouldReturn` toCardCode Quests.findTheOthers

    it "has stage 2B give each player a hero back before stage 3A is revealed" . longArmTest $ \pid -> do
      q <- putQuestStage Quests.findTheOthers
      run $ Flip q
      setQuestDeck [Quests.freeTheCaptives]
      toStaging ["01012", "01007"]
      run $ PlaceTokens GameSource q Progress 5
      chooseTarget pid =<< hero "01012"
      heroInStagingArea <$> heroIn "01012" `shouldReturn` False
      activeQuestCode `shouldReturn` toCardCode Quests.freeTheCaptives
      Just (QuestionLabel l _ _ _) <- getQuestion pid
      l `shouldSatisfy` ("$label.resolveWhenRevealed" `isPrefixOf`)

    it "has stage 3A put each hero in the staging area under an Orc's guard, until that Orc is defeated" . longArmTest $ \pid -> do
      q <- putQuestStage Quests.freeTheCaptives
      toStaging ["01012"]
      [orc] <- setEncounterDeck [Enemies.urukOfMordor]
      run $ Flip q
      resolveWhenRevealed pid
      chooseTarget pid (CardIdTarget orc.id)
      eid <- getsGame (fromJustNote "no Orc" . fmap toId . find ((== orc.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))
      h <- heroIn "01012"
      (heroGuardedBy h, heroInStagingArea h) `shouldBe` (Just orc.id, False)
      -- the guard gets +1 threat and is immune; the stage cannot be defeated
      getEnemyThreat eid `shouldReturn` 3
      getModifiers eid >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      quest <- toTarget <$> getActiveQuest
      getModifiers quest >>= (`shouldSatisfy` elem CannotBeDefeated)
      run $ Destroy GameSource (EnemyTarget eid)
      h' <- heroIn "01012"
      (heroGuardedBy h', heroExhausted h') `shouldBe` (Nothing, True)
      controlled pid >>= (`shouldSatisfy` elem "01012")
      getModifiers quest >>= (`shouldSatisfy` notElem CannotBeDefeated)

    it "has stage 4 add Uruk Chieftain, which bars defeating it and attacks as it engages" . longArmTest $ \pid -> do
      q <- putQuestStage Quests.escapeIntoTheJungle
      setSetAside [Enemies.urukChieftain]
      _ <- setEncounterDeck [Locations.jungleTrail]
      run $ Flip q
      resolveWhenRevealed pid
      chief <- getsGame (fromJustNote "no chieftain" . fmap toId . find ((== toCardCode Enemies.urukChieftain) . toCardCode) . toList . (.entities.enemies))
      quest <- toTarget <$> getActiveQuest
      getModifiers quest >>= (`shouldSatisfy` elem CannotBeDefeated)
      getModifiers chief >>= (`shouldSatisfy` elem CannotTakeNonCombatDamage)
      uruk <- stage Enemies.urukOfMordor
      getEngagementCost uruk `shouldReturn` 25
      run $ EngageEnemy pid chief
      passActions pid
      Just (QuestionLabel _ source _ _) <- getQuestion pid
      source `shouldBe` Just (EnemyTarget chief)

    it "is won as stage 4 is defeated" . longArmTest $ \pid -> do
      q <- putQuestStage Quests.escapeIntoTheJungle
      _ <- setEncounterDeck [Locations.jungleTrail]
      run $ Flip q
      resolveWhenRevealed pid
      run $ PlaceTokens GameSource q Progress 15
      getsGame (.outcome) `shouldReturn` Just Won

  describe "encounter cards" do
    it "gives Sauron's Enforcers X of each, and has it assign X damage at the end of the round" . longArmTest $ \pid -> do
      setUp pid
      eid <- getsGame (toId . fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
      getEnemyThreat eid `shouldReturn` 1
      getEnemyAttack eid `shouldReturn` 1
      run $ CheckWindows [mkAfter RoundEnds]
      kahliel <- hero (toCardCode Objectives.kahliel)
      chooseTarget pid kahliel
      getDamage kahliel `shouldReturn` 1

    it "gives Southron Boma X threat and has each player spend a resource to travel there" . longArmTest $ \pid -> do
      setUp pid
      lid <- stageLocation Locations.southronBoma
      getLocationThreat lid `shouldReturn` 1
      run $ TravelTo lid
      (.resources) <$> getHeroWithCode (toCardCode Objectives.kahliel) `shouldReturn` 1

    it "bars travel to Southron Boma while a player has no resource" . longArmTest $ \pid -> do
      setUp pid
      kahliel <- hero (toCardCode Objectives.kahliel)
      run $ RemoveTokens GameSource kahliel Resource 2
      lid <- stageLocation Locations.southronBoma
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

    it "has Village Hut add a discarded enemy to the staging area" . longArmTest $ \_ -> do
      lid <- stageLocation Locations.villageHut
      _ <- setEncounterDeck [Enemies.urukOfMordor]
      run $ TravelTo lid
      getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.urukOfMordor]

    it "has Village Hut put the first player's top card into play exhausted, if an ally" . longArmTest $ \pid -> do
      lid <- stageLocation Locations.villageHut
      _ <- setEncounterDeck []
      card <- PlayerCard <$> genPlayerCard Allies.guardOfTheCitadel (Just pid)
      g <- getGame
      putGame $ g & playersL . ix pid . deckL %~ (card :)
      run $ TravelTo lid
      chooseAbilityOf pid (LocationTarget lid)
      allies <- getsGame (map toAttrs . toList . (.entities.allies))
      map (\a -> (allyCardId a, allyExhausted a, a.placement)) allies `shouldBe` [(card.id, True, InPlayArea pid)]

    it "has Overrun by Orcs raise the staging area's threat by twice the stage number" . longArmTest $ \_ -> do
      _ <- putQuestStage Quests.findTheOthers
      _ <- setEncounterDeck [Treacheries.overrunByOrcs]
      run RevealEncounterCard
      resolveRevealed
      getModifiers GameTarget >>= (`shouldSatisfy` elem (StagingThreatBonus 4))

    it "has Savage Attack reveal another card when the player has no ally to discard" . longArmTest $ \_ -> do
      _ <- putQuestStage Quests.surpriseAttack
      _ <- setEncounterDeck [Treacheries.savageAttack, Treacheries.overrunByOrcs]
      run RevealEncounterCard
      resolveRevealed
      getModifiers GameTarget >>= (`shouldSatisfy` elem (StagingThreatBonus 2))

    it "has Savage Attack discard the highest cost ally if the player chooses" . longArmTest $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Treacheries.savageAttack]
      run RevealEncounterCard
      resolveRevealed
      chooseLabel "$label.savageAttack.discardAlly" pid
      getsGame (length . (.entities.allies)) `shouldReturn` 0
