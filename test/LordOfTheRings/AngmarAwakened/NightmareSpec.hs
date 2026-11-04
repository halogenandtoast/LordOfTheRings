module LordOfTheRings.AngmarAwakened.NightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Card.Synthetic qualified as Synthetic
import LordOfTheRings.Card (CardDef (cdKeywords), CardType (HeroType), cdCardType, toCard, toCardDef)
import LordOfTheRings.Keyword (Keyword (Safe))
import LordOfTheRings.Quest.Types (isSideQuest)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer), getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Phase (Phase (CombatPhase, PlanningPhase))
import LordOfTheRings.Scenario (acrossTheEttenmoors, escapeFromMountGram, theBattleOfCarnDum, theDreadRealm, theTreacheryOfRhudaur, theWastesOfEriador)
import LordOfTheRings.Scenario.Scenarios.EscapeFromMountGram.Helpers (capturedDeck, capturedUnder)
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isNight)
import LordOfTheRings.Scenario.Types (GameMode (..), encounterDeckL, modeL, scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (AdvancedToStage, DeclaredAsAttacker, LastTimeCounterRemoved, PhaseEnds, PlayedFromHand, RoundEnds, ThreatRaisedByEncounter), mkAfter, mkWhen)
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

questOf :: CardDef -> GameT Quest
questOf def = getsGame (fromJustNote "no such quest" . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

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

-- | Deal the enemy the card as a shadow card and resolve it, against the defenders.
shadow :: PlayerId -> EnemyId -> CardDef -> [Target] -> GameT ()
shadow pid eid def defenders = do
  card <- genEncounterCard def
  run $ AttachShadowCard eid card
  run $ ResolveShadow eid pid defenders

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

  describe "Across the Ettenmoors" do
    it "builds the nightmare deck, its own stage 1 in play" . nightmare acrossTheEttenmoors $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Locations.trollFells, Enemies.goblinPursuer, Enemies.giantBeetle, Quests.isolated]
        `shouldBe` [0, 0, 2, 1]
      toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.intoTheEttenmoorsNightmare

    it "stages a side quest guarding a safe location, and discards down to an enemy per player" . nightmare acrossTheEttenmoors $ \pid -> do
      flipFirstQuest pid
      sideQuests <- getsGame (filter isSideQuest . toList . (.entities.quests))
      length sideQuests `shouldBe` 1
      safe <- getsGame (filter (member Safe . cdKeywords . toCardDef) . toList . (.entities.locations))
      map (.placement) safe `shouldBe` [AttachedTo (CardIdTarget (toCard q).id) | q <- sideQuests]
      getsGame (length . (.entities.enemies)) `shouldReturn` 1

    it "has stage 2A guard the side quest it reveals with a safe location" . nightmare acrossTheEttenmoors $ \_ -> do
      q <- putQuestStage Quests.aMiserableJourney
      _ <- setEncounterDeck [Quests.forageForFood, Enemies.goblinPursuer, Locations.secludedCave]
      -- past its shuffle of the discard pile into the deck
      run $ DoStep 1 (WhenRevealed q)
      resolveRevealed
      cave <- locationOf Locations.secludedCave
      forage <- cardOf Quests.forageForFood
      (.placement) <$> getLocation cave `shouldReturn` AttachedTo (CardIdTarget forage)

    it "Scavenging Stone-troll bars travel to safe locations until damaged, and locks the discard piles" . nightmare acrossTheEttenmoors $ \_ -> do
      cave <- stagedLocation Locations.secludedCave
      troll <- staged Enemies.scavengingStoneTroll
      getModifiers (toTarget cave) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      getModifiers GameTarget >>= (`shouldSatisfy` elem DiscardPilesLocked)
      run $ DealDamage GameSource (toTarget troll) 1
      getModifiers (toTarget cave) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)

    it "Hoarwell River Serpent engages as the players travel to a safe location, hurting the damaged" . nightmare acrossTheEttenmoors $ \pid -> do
      cave <- stagedLocation Locations.secludedCave
      _ <- staged Enemies.hoarwellRiverSerpent
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ DealDamage GameSource eowyn 1
      run $ TravelTo cave
      resolveForced pid
      getCharacterDamage eowyn `shouldReturn` 2

    it "Rocky Foothills must be traveled to, with 2 quest points per quest" . nightmare acrossTheEttenmoors $ \_ -> do
      _ <- putQuestStage Quests.aMiserableJourney
      foothills <- stagedLocation Locations.rockyFoothills
      getModifiers (toTarget foothills) >>= (`shouldSatisfy` \ms -> MustBeTraveledTo `elem` ms && QuestPointModifier 2 `elem` ms)

    it "Vast Troll-fells stops heroes gaining resources by card effects" . nightmare acrossTheEttenmoors $ \_ -> do
      _ <- stagedLocation Locations.vastTrollFells
      eowyn <- getHeroWithCode "01007"
      getModifiers (toTarget eowyn) >>= (`shouldSatisfy` elem CannotGainResourcesFromCardEffects)

    it "Crashing Thunder exhausts each damaged character" . nightmare acrossTheEttenmoors $ \pid -> do
      _ <- putQuestStage Quests.aMiserableJourney
      eowyn <- toTarget <$> getHeroWithCode "01007"
      reveal [Treacheries.crashingThunder]
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 1
      heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True

    it "Barren and Desolate takes 1 willpower from each character while its side quest is current" . nightmare acrossTheEttenmoors $ \_ -> do
      _ <- putQuestStage Quests.aMiserableJourney
      run . AddToStaging =<< genEncounterCard Quests.lieLow
      reveal [Treacheries.barrenAndDesolate, Locations.secludedCave]
      lie <- toTarget <$> questOf Quests.lieLow
      run $ SetCurrentQuest lie
      -- the current quest is itself a modifier, read as the modifiers are next gathered
      run $ CheckWindows []
      eowyn <- getHeroWithCode "01007"
      getModifiers (toTarget eowyn) >>= (`shouldSatisfy` elem (WillpowerModifier (-1)))

    it "Isolated stops card effects drawing cards" . nightmare acrossTheEttenmoors $ \pid -> do
      run . AddToStaging =<< genEncounterCard Quests.isolated
      hand <- length . (.hand) <$> getPlayer pid
      run $ DrawCards $ Drawing pid 1
      length . (.hand) <$> getPlayer pid `shouldReturn` hand + 1
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotDrawCardsFromCardEffects)

    it "Rescue from the Trolls takes the lowest threat cost hero until it is defeated" . nightmare acrossTheEttenmoors $ \_ -> do
      reveal [Quests.rescueFromTheTrolls, Locations.secludedCave]
      getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` notElem "01002")
      rescue <- questOf Quests.rescueFromTheTrolls
      run $ CompleteQuest (toId rescue)
      getsGame (map toCardCode . toList . (.entities.heroes)) >>= (`shouldSatisfy` elem "01002")

  describe "The Treachery of Rhudaur" do
    it "builds the nightmare deck, each side quest's set aside" . nightmare theTreacheryOfRhudaur $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.cursedDead, Enemies.thaurdirsHunter, Enemies.apostateOfAngmar, Locations.secretAntechamber, Treacheries.lifeDrain]
        `shouldBe` [0, 2, 0, 0, 0]
      aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
      map (`copies` aside) [Enemies.apostateOfAngmar, Locations.secretAntechamber, Treacheries.lifeDrain] `shouldBe` [3, 3, 3]

    it "shuffles in the sets of the side quests not defeated as stage 2 begins" . nightmare theTreacheryOfRhudaur $ \_ -> do
      run $ DoStep 1 Setup
      _ <- putQuestStage Quests.thaurdirsPursuit
      run . AddToStaging =<< genEncounterCard Objectives.daechanarsBrand
      run $ CheckWindows [mkAfter AdvancedToStage]
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.apostateOfAngmar, Locations.secretAntechamber, Treacheries.lifeDrain] `shouldBe` [0, 3, 3]

    it "Thaurdir's Hunter's shadow removes 5 progress from a quest when undefended" . nightmare theTreacheryOfRhudaur $ \pid -> do
      q <- putQuestStage Quests.thaurdirsPursuit
      run $ PlaceTokens GameSource q Progress 7
      warg <- staged Enemies.wightOfRhudaur
      shadow pid warg Enemies.thaurdirsHunter []
      (.progress) <$> getActiveQuest `shouldReturn` 2

    it "Undead Thrall's shadow puts it into play engaged, dealt a shadow card" . nightmare theTreacheryOfRhudaur $ \pid -> do
      wight <- staged Enemies.wightOfRhudaur
      _ <- setEncounterDeck [Locations.forgottenCourtyard]
      shadow pid wight Enemies.undeadThrall [toTarget wight]
      thrall <- enemyOf Enemies.undeadThrall
      e <- getEnemy thrall
      e.placement `shouldBe` EngagedWith pid
      length e.shadowCards `shouldBe` 1

    it "Forgotten Courtyard gains threat as Undead enemies enter play" . nightmare theTreacheryOfRhudaur $ \pid -> do
      court <- stagedLocation Locations.forgottenCourtyard
      _ <- staged Enemies.wightOfRhudaur
      resolveForced pid
      getModifiers (toTarget court) >>= (`shouldSatisfy` elem (ThreatModifier 1))

    it "Charred Cellar, active, deals 2 damage as an encounter card raises a threat" . nightmare theTreacheryOfRhudaur $ \pid -> do
      cellar <- stagedLocation Locations.charredCellar
      run $ PlaceLocation cellar ActiveLocation
      run $ CheckWindows [mkAfter $ ThreatRaisedByEncounter pid 1]
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 2

    it "Malevolent Forces removes the strongest questing character, raising the threat by its willpower" . nightmare theTreacheryOfRhudaur $ \pid -> do
      eowyn <- toTarget <$> getHeroWithCode "01007"
      run $ CommitToQuest eowyn
      threat <- threatOf pid
      reveal [Treacheries.malevolentForces]
      threatOf pid `shouldReturn` threat + 4

    it "Secret Antechamber caps the main quest's progress and raises each threat by 3 to travel" . nightmare theTreacheryOfRhudaur $ \pid -> do
      q <- putQuestStage Quests.thaurdirsPursuit
      room <- stagedLocation Locations.secretAntechamber
      getModifiers q >>= (`shouldSatisfy` elem (ProgressPerRoundLimit 5))
      threat <- threatOf pid
      run $ TravelTo room
      threatOf pid `shouldReturn` threat + 3

    it "Apostate of Angmar keeps its player from attacking Thaurdir" . nightmare theTreacheryOfRhudaur $ \pid -> do
      thaurdir <- staged Enemies.thaurdirTheTreacheryOfRhudaur
      apostate <- staged Enemies.apostateOfAngmar
      run $ EngageEnemy pid apostate
      getModifiers (toTarget thaurdir) >>= (`shouldSatisfy` elem (CannotBeAttackedByCharactersOf pid))

    it "Life Drain is dealt to Thaurdir as a shadow card" . nightmare theTreacheryOfRhudaur $ \pid -> do
      thaurdir <- staged Enemies.thaurdirTheTreacheryOfRhudaur
      reveal [Treacheries.lifeDrain]
      resolveForced pid
      map toCardCode . (.shadowCards) <$> getEnemy thaurdir `shouldReturn` [toCardCode Treacheries.lifeDrain]

  describe "The Battle of Carn Dûm" do
    it "builds the nightmare deck" . nightmare theBattleOfCarnDum $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.angmarMarauder, Treacheries.terrorOfTheNorth, Enemies.orcWight, Quests.severTheHead]
        `shouldBe` [0, 0, 3, 1]

    it "Orc Wight is dealt 2 shadow cards as it enters play" . nightmare theBattleOfCarnDum $ \pid -> do
      _ <- setEncounterDeck [Locations.strongholdTower, Locations.strongholdTower]
      wight <- staged Enemies.orcWight
      resolveForced pid
      length . (.shadowCards) <$> getEnemy wight `shouldReturn` 2

    it "Thaurdir's Elite sheds its shadow cards at 3, each player discarding an ally" . nightmare theBattleOfCarnDum $ \pid -> do
      _ <- putAllyIntoPlay pid Allies.silverlodeArcher
      elite <- staged Enemies.thaurdirsElite
      _ <- setEncounterDeck [Locations.strongholdTower, Locations.strongholdTower, Locations.strongholdTower]
      replicateM_ 3 $ run $ DealShadowCard elite
      length . (.shadowCards) <$> getEnemy elite `shouldReturn` 0
      getsGame (length . (.entities.allies)) `shouldReturn` 0

    it "Stronghold Tower deals each player 2 damage to travel there" . nightmare theBattleOfCarnDum $ \pid -> do
      tower <- stagedLocation Locations.strongholdTower
      getModifiers GameTarget >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)
      run $ TravelTo tower
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 2

    it "Gates of Carn Dûm, explored, is dealt to the staged enemy with the lowest engagement cost" . nightmare theBattleOfCarnDum $ \pid -> do
      gates <- stagedLocation Locations.gatesOfCarnDum
      wight <- staged Enemies.orcWight
      _ <- setEncounterDeck [Locations.strongholdTower, Locations.strongholdTower]
      resolveForced pid
      elite <- staged Enemies.thaurdirsElite
      run $ PlaceLocation gates ActiveLocation
      run $ PlaceTokens GameSource (toTarget gates) Progress 2
      resolveForced pid
      map toCardCode . (.shadowCards) <$> getEnemy wight >>= (`shouldSatisfy` elem (toCardCode Locations.gatesOfCarnDum))
      length . (.shadowCards) <$> getEnemy elite `shouldReturn` 0

    it "Death and Decay surges when it discards no attachment" . nightmare theBattleOfCarnDum $ \_ -> do
      reveal [Treacheries.deathAndDecay, Locations.strongholdTower]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

    it "Howling of Wargs can deal a shadow card to each enemy in the staging area" . nightmare theBattleOfCarnDum $ \pid -> do
      elite <- staged Enemies.thaurdirsElite
      reveal [Treacheries.howlingOfWargs, Locations.strongholdTower]
      chooseLabel "$label.howlingOfWargs.shadow" pid
      length . (.shadowCards) <$> getEnemy elite `shouldReturn` 1

    it "Black Mare gives Thaurdir 3 hit points, and raises each threat as he flips" . nightmare theBattleOfCarnDum $ \pid -> do
      thaurdir <- staged Enemies.thaurdirCaptain
      reveal [Treacheries.blackMare]
      getModifiers (toTarget thaurdir) >>= (`shouldSatisfy` elem (HitPointModifier 3))
      threat <- threatOf pid
      run $ Flip (toTarget thaurdir)
      resolveForced pid
      threatOf pid >>= (`shouldSatisfy` (>= threat + 1))

    it "Sever the Head shields Thaurdir and strengthens him" . nightmare theBattleOfCarnDum $ \_ -> do
      thaurdir <- staged Enemies.thaurdirCaptain
      run . AddToStaging =<< genEncounterCard Quests.severTheHead
      getModifiers (toTarget thaurdir) >>= (`shouldSatisfy` \ms -> all (`elem` ms) [ThreatModifier 2, AttackModifier 2, CannotTakeDamage])

    it "Protect the Flanks deals damage per shadow card in the staging area at the end of combat" . nightmare theBattleOfCarnDum $ \pid -> do
      elite <- staged Enemies.thaurdirsElite
      _ <- setEncounterDeck [Locations.strongholdTower]
      run $ DealShadowCard elite
      run . AddToStaging =<< genEncounterCard Quests.protectTheFlanks
      run $ CheckWindows [mkWhen $ PhaseEnds CombatPhase]
      resolveForced pid
      eowyn <- toTarget <$> getHeroWithCode "01007"
      chooseTarget pid eowyn
      getCharacterDamage eowyn `shouldReturn` 1

  describe "The Dread Realm" do
    let reanimated = getsGame (map toId . filter ((== toCardCode Synthetic.reanimatedDead) . toCardCode) . toList . (.entities.enemies))
        onTop pid def = do
          card <- PlayerCard <$> genPlayerCard def (Just pid)
          g <- getGame
          putGame $ g & playersL . ix pid . deckL %~ (card :)
          pure card
        inDiscard pid def = do
          card <- PlayerCard <$> genPlayerCard def (Just pid)
          g <- getGame
          putGame $ g & playersL . ix pid . discardL %~ (card :)
          pure card

    it "builds the nightmare deck" . nightmare theDreadRealm $ \_ -> do
      run $ DoStep 1 Setup
      deck <- getEncounterDeck
      map (`copies` deck) [Enemies.witchOfAngmar, Enemies.deadLord, Treacheries.terrorOfTheNorth, Enemies.silentGuardian, Quests.theWitchKingsGift]
        `shouldBe` [0, 0, 0, 2, 1]

    it "Silent Guardian reanimates the first event its player plays" . nightmare theDreadRealm $ \pid -> do
      guardian <- staged Enemies.silentGuardian
      run $ EngageEnemy pid guardian
      -- played, the event lies in its player's discard pile
      event <- inDiscard pid Events.everVigilant
      run $ CheckWindows [mkAfter $ PlayedFromHand (CardIdTarget event.id)]
      length <$> reanimated `shouldReturn` 1
      other <- inDiscard pid Events.everVigilant
      run $ CheckWindows [mkAfter $ PlayedFromHand (CardIdTarget other.id)]
      length <$> reanimated `shouldReturn` 1

    it "Necromancer of Carn Dûm reanimates the top of the deck in place of its attack" . nightmare theDreadRealm $ \pid -> do
      necro <- staged Enemies.necromancerOfCarnDum
      run $ EngageEnemy pid necro
      _ <- onTop pid Allies.silverlodeArcher
      run $ EnemyAttack pid necro
      resolveForced pid
      length <$> reanimated `shouldReturn` 1

    it "Necromancer of Carn Dûm, its attack canceled, has not attacked for Blight of Carn Dûm" . nightmare theDreadRealm $ \pid -> do
      _ <- stagedLocation Locations.blightOfCarnDum
      necro <- staged Enemies.necromancerOfCarnDum
      run $ EngageEnemy pid necro
      run $ DealDamage GameSource (toTarget necro) 1
      _ <- onTop pid Allies.silverlodeArcher
      run $ EnemyAttack pid necro
      resolveForced pid
      getDamage (toTarget necro) `shouldReturn` 1

    it "Inner Sanctum takes no progress in the staging area, and resolves the Sorcery it discards" . nightmare theDreadRealm $ \_ -> do
      _ <- putQuestStage Quests.theCatacombsOfCarnDum
      sanctum <- stagedLocation Locations.innerSanctum
      getModifiers (toTarget sanctum) >>= (`shouldSatisfy` elem (ProgressLimit 0))
      _ <- setEncounterDeck [Treacheries.powerBeyondDeath, Locations.cursedHalls, Locations.cursedHalls]
      run $ TravelTo sanctum
      resolveRevealed
      getsGame (map (.placement) . filter ((== toCardCode Treacheries.powerBeyondDeath) . toCardCode) . toList . (.entities.treacheries))
        >>= (`shouldSatisfy` (\ps -> length ps == 1 && all (/= StagingArea) ps))

    it "Power Beyond Death gives Daechanar +2 defense" . nightmare theDreadRealm $ \_ -> do
      _ <- putQuestStage Quests.theCatacombsOfCarnDum
      daechanar <- staged Enemies.daechanar
      reveal [Treacheries.powerBeyondDeath, Locations.cursedHalls]
      getModifiers (toTarget daechanar) >>= (`shouldSatisfy` elem (DefenseModifier 2))

    it "Cursed Halls makes Sorcery attachments immune to player cards" . nightmare theDreadRealm $ \_ -> do
      _ <- putQuestStage Quests.theCatacombsOfCarnDum
      reveal [Treacheries.powerBeyondDeath, Locations.cursedHalls]
      power <- getsGame (fromJustNote "no treachery" . headMay . map toTarget . toList . (.entities.treacheries))
      getModifiers power >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

    it "Daechanar's Curse reanimates the next cards drawn" . nightmare theDreadRealm $ \pid -> do
      reveal [Treacheries.daechanarsCurse, Locations.cursedHalls]
      _ <- onTop pid Allies.silverlodeArcher
      hand <- length . (.hand) <$> getPlayer pid
      run $ DrawCards $ Drawing pid 1
      length . (.hand) <$> getPlayer pid `shouldReturn` hand
      length <$> reanimated `shouldReturn` 1
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0

    it "All is Lost raises its player's threat as he attacks a Reanimated Dead" . nightmare theDreadRealm $ \pid -> do
      reveal [Treacheries.allIsLost]
      card <- onTop pid Allies.silverlodeArcher
      eid <- getRandom
      run $ Reanimate card.id eid
      [dead] <- reanimated
      eowyn <- toTarget <$> getHeroWithCode "01007"
      threat <- threatOf pid
      run $ CheckWindows [mkAfter $ DeclaredAsAttacker eowyn dead]
      threatOf pid `shouldReturn` threat + 1

    it "The Witch-king's Gift reanimates each discard pile as its time runs out" . nightmare theDreadRealm $ \pid -> do
      run . AddToStaging =<< genEncounterCard Quests.theWitchKingsGift
      _ <- inDiscard pid Allies.silverlodeArcher
      _ <- inDiscard pid Allies.guardOfTheCitadel
      gift <- toTarget <$> questOf Quests.theWitchKingsGift
      run $ CheckWindows [mkAfter $ LastTimeCounterRemoved gift]
      resolveForced pid
      length <$> reanimated `shouldReturn` 2
