module LordOfTheRings.Standalone.BelegostSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Character (getAttack)
import LordOfTheRings.Helpers.Window (getActionAbilities)
import LordOfTheRings.Helpers.Attachment (attachmentHosts)
import LordOfTheRings.Helpers.Staging (getStagingThreat, getEnemyThreat, getEnemyAttack, getEnemyDefense, getEngagementCost, getLocationThreat)
import LordOfTheRings.Matcher (getAttachmentsControlledBy, getEnemy, getHero, select, getThreat, getDiscoverValue, playerCardsGuardedBy, AttachMatcher (AttachToEnemy), AllyMatcher (AllyControlledBy), EnemyMatcher (AnyEnemy, EnemyEngagedWith, EnemyWithCardCode), PlayerMatcher (PlayerWithId))
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Keyword (Keyword (Ranged, Sentinel))
import LordOfTheRings.Keyword qualified as Keyword
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), encounterDiscardL)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Phase (Phase (QuestPhase, PlanningPhase))
import LordOfTheRings.Trait (Trait (Weapon))
import LordOfTheRings.Window (WindowType (PhaseBegins, EnemyAttacking, AdvancedToStage, WhenRevealedCanceled), mkAfter, mkWhen)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "missing enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies)

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame $ toId . fromJustNote "missing location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations)

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

claimLoot :: PlayerId -> CardDef -> GameT ()
claimLoot pid def = do
  hero <- heroOf pid "01012"
  run . (`PutObjectiveIntoPlayAttached` hero) =<< genEncounterCard def

stats :: EnemyId -> GameT [Int]
stats eid = sequence [getEnemyThreat eid, getEnemyAttack eid, getEnemyDefense eid]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame $ toId . fromJustNote "missing enemy" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies)

clock :: GameT ObjectiveAttrs
clock = getsGame $ toAttrs . fromJustNote "missing Stalking the Ruins" . find ((== toCardCode Objectives.stalkingTheRuins) . toCardCode) . toList . (.entities.objectives)

stageClock :: GameT ObjectiveId
stageClock = do
  run . AddToStaging =<< genEncounterCard Objectives.stalkingTheRuins
  (.id) <$> clock

questBegins :: GameT ()
questBegins = run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]

strongDefender :: PlayerId -> GameT Target
strongDefender pid = do
  h <- heroOf pid "01012"
  run $ CreateEffect $ Effect GameSource h [DefenseModifier 10] UntilEndOfRound
  pure h

defendAttack :: PlayerId -> Target -> GameT ()
defendAttack pid h = passAttackActions >> chooseTarget pid h >> passAttackActions

passAttackActions :: GameT ()
passAttackActions = do
  questions <- getsGame (Map.toList . (.question))
  for_ (headMay [pid | (pid, QuestionLabel prompt _ _ _) <- questions, prompt == "$label.takeAction"]) \pid -> do
    passActions pid
    passAttackActions

lootOf :: CardDef -> GameT ObjectiveAttrs
lootOf def = getsGame $ toAttrs . fromJustNote "missing Loot objective" . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives)

lootAvailable :: PlayerId -> CardDef -> GameT Bool
lootAvailable pid def = do
  oid <- (.id) <$> lootOf def
  any ((== ObjectiveSource oid) . abilitySource) <$> getActionAbilities pid

useLoot :: PlayerId -> CardDef -> GameT ()
useLoot pid def = do
  oid <- (.id) <$> lootOf def
  abilities <- getActionAbilities pid
  let ab = fromJustNote "Loot action unavailable" $ find ((== ObjectiveSource oid) . abilitySource) abilities
  run $ UseAbility pid ab []

belegostLoot :: [CardDef]
belegostLoot = [Objectives.swordOfBelegost, Objectives.mapOfEredLuin, Objectives.keysOfBelegost, Objectives.bookOfAulesChildren, Objectives.dwarvenTorch, Objectives.blueMountainGem]

belegostTable :: Int -> ([PlayerId] -> GameT ()) -> IO ()
belegostTable size body = do
  players <- for [1 .. size] \n -> either (error . show) id <$> loadPlayer ("Player " <> tshow n) testDeck
  env <- newGameEnv (newTableIn passageThroughMirkwood $ fromJustNote "empty table" $ nonEmpty players) (const $ pure ()) 1
  runGameT env $ body =<< getsGame (.playerOrder)

finishDiscover :: PlayerId -> GameT ()
finishDiscover pid = getQuestion pid >>= \case
  Just (QuestionLabel label _ _ _) | label `elem` ["$label.discoverLooked", "$label.discoverChooseLoot", "$label.discoverExtraHazard"] -> do
    chooseOptionMatching "finish Discover" pid (const True)
    finishDiscover pid
  _ -> pure ()

setEncounterDiscard :: [CardDef] -> GameT ()
setEncounterDiscard defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)

revealTreachery :: CardDef -> GameT ()
revealTreachery def = do
  _ <- setEncounterDeck [def, Locations.oldForestRoad, Locations.oldForestRoad]
  run RevealEncounterCard
  resolveRevealed

resolveTreacheryShadow :: PlayerId -> EnemyId -> CardDef -> GameT ()
resolveTreacheryShadow pid eid def = do
  [card, _] <- setEncounterDeck [def, Locations.oldForestRoad]
  run $ DealShadowCard eid
  run $ ResolveShadowCard card.id eid pid []

spec :: Spec
spec = describe "Belegost enemies" do
  describe "Echoes in the Dark" do
    it "recalculates the first player's characters, stacks, and expires at phase end (FAQ 1.55)" . twoPlayerTest testDeck $ \one two -> do
      _ <- stageEnemy Enemies.forestSpider
      revealTreachery Treacheries.echoesInTheDark
      getStagingThreat `shouldReturn` 5
      getThreat one `shouldReturn` 1
      getThreat two `shouldReturn` 1
      ally <- putAllyIntoPlay one Allies.snowbournScout
      getStagingThreat `shouldReturn` 6
      _ <- putAllyIntoPlay two Allies.snowbournScout
      _ <- putAllyIntoPlay two Allies.guardOfTheCitadel
      getStagingThreat `shouldReturn` 6
      revealTreachery Treacheries.echoesInTheDark
      getStagingThreat `shouldReturn` 10
      run $ Destroy GameSource ally
      getStagingThreat `shouldReturn` 8
      run $ SetFirstPlayer two
      getStagingThreat `shouldReturn` 12
      run $ EndEffects UntilEndOfPhase
      getStagingThreat `shouldReturn` 2

    for_ [(Events.aTestOfWill, True), (Allies.snowbournScout, False)] \(def, event) ->
      it ("uses the actual randomly discarded card: " <> show event) . gameTest $ \pid -> do
        _ <- putCardIntoHand pid def
        eid <- stageEnemy Enemies.forestSpider
        resolveTreacheryShadow pid eid Treacheries.echoesInTheDark
        length . (.hand) <$> getPlayer pid `shouldReturn` 0
        getEnemyAttack eid `shouldReturn` if event then 4 else 2
        run $ EndEffects (UntilEnemyAttackEnds eid)
        getEnemyAttack eid `shouldReturn` 2

    it "does not gain attack when discarding from hand is prohibited" . gameTest $ \pid -> do
      _ <- putCardIntoHand pid Events.aTestOfWill
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfPhase
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow pid eid Treacheries.echoesInTheDark
      length . (.hand) <$> getPlayer pid `shouldReturn` 1
      getEnemyAttack eid `shouldReturn` 2

    it "has no attack bonus with an empty hand" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow pid eid Treacheries.echoesInTheDark
      getEnemyAttack eid `shouldReturn` 2

  describe "Spawn of Thangorodrim" do
    for_ [0, 1, 2] \n ->
      it ("adds a resource and summons only on reaching three, from " <> show n) . gameTest $ \pid -> do
        oid <- stageClock
        run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource n
        dragon <- genEncounterCard Enemies.naurlhug
        run $ SetAside dragon
        revealTreachery Treacheries.spawnOfThangorodrim
        (.resources) <$> clock `shouldReturn` n + 1
        dragons <- select $ EnemyWithCardCode (toCardCode Enemies.naurlhug)
        length dragons `shouldBe` if n == 2 then 1 else 0
        for_ dragons \eid -> (.cardId) . toAttrs <$> getEnemy eid `shouldReturn` dragon.id
        getQuestion pid `shouldReturn` Nothing

    it "attacks the first player without moving the dragon or adding a resource" . twoPlayerTest testDeck $ \one two -> do
      _ <- stageClock
      eid <- stageEnemy Enemies.naurlhug
      run $ EngageEnemy two eid
      h <- strongDefender one
      revealTreachery Treacheries.spawnOfThangorodrim
      defendAttack one h
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith two
      (.resources) <$> clock `shouldReturn` 0
      getDamage h `shouldReturn` 0
      getQuestion two `shouldReturn` Nothing

    it "makes one additional attack with a fresh shadow card" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      h <- strongDefender pid
      secondDefender <- heroOf pid "01007"
      _ <- setEncounterDeck [Treacheries.spawnOfThangorodrim, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid h
      defendAttack pid secondDefender
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion pid `shouldReturn` Nothing

  describe "Concealed Spikes" do
    for_ [False, True] \active ->
      it ("assigns damage to each player's own characters, active Discover: " <> show active) . twoPlayerTest testDeck $ \one two -> do
        when active $ void $ activeLocation Locations.abandonedMineBelegost
        revealTreachery Treacheries.concealedSpikes
        h1 <- heroOf one "01012"
        h2 <- heroOf two "01012"
        let amount = if active then 3 else 1
        replicateM_ amount $ chooseTarget one h1
        replicateM_ amount $ chooseTarget two h2
        getDamage h1 `shouldReturn` amount
        getDamage h2 `shouldReturn` amount

    it "does nothing as a shadow when the defender controls no attachments" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow pid eid Treacheries.concealedSpikes
      getQuestion pid `shouldReturn` Nothing

    it "counts claimed Loot and player attachments but only for the defending player" . twoPlayerTest testDeck $ \one two -> do
      h <- heroOf one "01012"
      _ <- putAttachmentOn one Attachments.daggerOfWesternesse h
      claimLoot one Objectives.keysOfBelegost
      claimLoot two Objectives.mapOfEredLuin
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow one eid Treacheries.concealedSpikes
      chooseLabel "$label.belegost.spikesDamage" one
      replicateM_ 2 $ chooseTarget one h
      getDamage h `shouldReturn` 2
      getQuestion two `shouldReturn` Nothing

    it "allows a zero-cost Loot attachment to cancel the shadow" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow pid eid Treacheries.concealedSpikes
      chooseLabel "$label.belegost.spikesDiscard" pid
      map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Objectives.keysOfBelegost))
      getQuestion pid `shouldReturn` Nothing

    for_ [ShadowCannotBeCanceled, CannotCancelEncounterEffects] \prohibition ->
      it ("requires damage when cancellation is prohibited: " <> show prohibition) . gameTest $ \pid -> do
        claimLoot pid Objectives.keysOfBelegost
        h <- heroOf pid "01012"
        run $ CreateEffect $ Effect GameSource GameTarget [prohibition] UntilEndOfPhase
        eid <- stageEnemy Enemies.forestSpider
        resolveTreacheryShadow pid eid Treacheries.concealedSpikes
        chooseTarget pid h
        getDamage h `shouldReturn` 1
        (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` AttachedTo h

    it "discards the highest-cost attachment, leaving cheaper attachments and avoiding damage" . gameTest $ \pid -> do
      h <- heroOf pid "01012"
      expensive <- putAttachmentOn pid Attachments.stewardOfGondor h
      cheap <- putAttachmentOn pid Attachments.daggerOfWesternesse h
      claimLoot pid Objectives.keysOfBelegost
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow pid eid Treacheries.concealedSpikes
      chooseLabel "$label.belegost.spikesDiscard" pid
      attachments <- getAttachmentsControlledBy pid
      attachments `shouldSatisfy` notElem expensive
      attachments `shouldSatisfy` elem cheap
      getDamage h `shouldReturn` 0
      getQuestion pid `shouldReturn` Nothing

    it "offers every attachment tied for highest cost and excludes cheaper Loot" . gameTest $ \pid -> do
      h <- heroOf pid "01012"
      steward <- putAttachmentOn pid Attachments.stewardOfGondor h
      courage <- putAttachmentOn pid Attachments.unexpectedCourage h
      claimLoot pid Objectives.keysOfBelegost
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow pid eid Treacheries.concealedSpikes
      chooseLabel "$label.belegost.spikesDiscard" pid
      q <- fromJustNote "missing highest-cost choice" <$> getQuestion pid
      let offered = [t | TargetLabel t _ <- questionChoices q]
      length offered `shouldBe` 2
      offered `shouldSatisfy` elem steward
      offered `shouldSatisfy` elem courage
      chooseTarget pid courage
      getAttachmentsControlledBy pid >>= (`shouldSatisfy` elem steward)
      getDamage h `shouldReturn` 0

    it "does not offer a cheaper discard when the highest-cost attachment cannot leave play" . gameTest $ \pid -> do
      h <- heroOf pid "01012"
      expensive <- putAttachmentOn pid Attachments.stewardOfGondor h
      _ <- putAttachmentOn pid Attachments.daggerOfWesternesse h
      run $ CreateEffect $ Effect GameSource expensive [CannotLeavePlay] UntilEndOfPhase
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow pid eid Treacheries.concealedSpikes
      replicateM_ 2 $ chooseTarget pid h
      getDamage h `shouldReturn` 2
      getAttachmentsControlledBy pid >>= (`shouldSatisfy` elem expensive)

  describe "Covered Pit" do
    it "discards an ally belonging to each player" . twoPlayerTest testDeck $ \one two -> do
      _ <- putAllyIntoPlay one Allies.guardOfTheCitadel
      _ <- putAllyIntoPlay two Allies.guardOfTheCitadel
      revealTreachery Treacheries.coveredPit
      select (AllyControlledBy (PlayerWithId one)) `shouldReturn` []
      select (AllyControlledBy (PlayerWithId two)) `shouldReturn` []
      getQuestion one `shouldReturn` Nothing
      getQuestion two `shouldReturn` Nothing

    it "does nothing for a player without allies" . gameTest $ \pid -> do
      revealTreachery Treacheries.coveredPit
      getQuestion pid `shouldReturn` Nothing

    it "moves only the defender's Loot under a non-unique attacker" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.mapOfEredLuin
      claimLoot two Objectives.keysOfBelegost
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow two eid Treacheries.coveredPit
      cid <- (.cardId) . toAttrs <$> getEnemy eid
      objectiveGuard <$> lootOf Objectives.keysOfBelegost `shouldReturn` Just cid
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` StagingArea
      objectiveGuard <$> lootOf Objectives.mapOfEredLuin `shouldReturn` Nothing

    it "releases guarded Loot to the first player when its attacker is defeated" . twoPlayerTest testDeck $ \one two -> do
      claimLoot two Objectives.keysOfBelegost
      eid <- stageEnemy Enemies.forestSpider
      resolveTreacheryShadow two eid Treacheries.coveredPit
      run $ Destroy GameSource (EnemyTarget eid)
      h <- heroOf one "01012"
      chooseTarget one h
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` AttachedTo h
      objectiveGuard <$> lootOf Objectives.keysOfBelegost `shouldReturn` Nothing

    it "does not take Loot when the attacker is unique" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      eid <- stageEnemy Enemies.naurlhug
      resolveTreacheryShadow pid eid Treacheries.coveredPit
      h <- heroOf pid "01012"
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` AttachedTo h

    it "keeps a surviving attacker's Loot guarded and its engagement unchanged" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      eid <- stageEnemy Enemies.forestSpider
      run $ EngageEnemy pid eid
      h <- strongDefender pid
      _ <- setEncounterDeck [Treacheries.coveredPit, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid h
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith pid
      cid <- (.cardId) . toAttrs <$> getEnemy eid
      objectiveGuard <$> lootOf Objectives.keysOfBelegost `shouldReturn` Just cid

    it "returns a unique attacker after a kill while leaving its defender's Loot attached" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      eid <- stageEnemy Enemies.naurlhug
      run $ EngageEnemy pid eid
      ally <- putAllyIntoPlay pid Allies.snowbournScout
      _ <- setEncounterDeck [Treacheries.coveredPit, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ EnemyAttack pid eid
      defendAttack pid ally
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      h <- heroOf pid "01012"
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` AttachedTo h

    for_ [Enemies.forestSpider, Enemies.naurlhug] \def ->
      it ("returns an attacker to staging after a kill even without Loot: " <> show (toCardCode def)) . gameTest $ \pid -> do
        eid <- stageEnemy def
        run $ EngageEnemy pid eid
        ally <- putAllyIntoPlay pid Allies.snowbournScout
        _ <- setEncounterDeck [Treacheries.coveredPit, Locations.oldForestRoad]
        run $ DealShadowCard eid
        run $ EnemyAttack pid eid
        defendAttack pid ally
        (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

  describe "Abandoned Mine" do
    for_ [1 .. 4] \n ->
      it ("examines " <> show (2 + n) <> " cards for " <> show n <> " players without increasing Discover") . belegostTable n $ \players -> do
        let lead = fromJustNote "no player" $ headMay players
        lid <- activeLocation Locations.abandonedMineBelegost
        _ <- setEncounterDeck $ replicate (3 + n) Locations.oldForestRoad
        eid <- stageEnemy Enemies.namelessCaveDweller
        run $ ResolveDiscover lead lid
        q <- fromJustNote "no Discover question" <$> getQuestion lead
        length (searchedCards q) `shouldBe` 2 + n
        getDiscoverValue lid `shouldReturn` Just 2
        stats eid `shouldReturn` [3, 3, 3]
        finishDiscover lead
        length <$> getEncounterDeck `shouldReturn` 1

    it "reshuffles the discard pile for its additional examined cards" . gameTest $ \pid -> do
      lid <- activeLocation Locations.abandonedMineBelegost
      _ <- setEncounterDeck [Locations.oldForestRoad]
      setEncounterDiscard $ replicate 3 Locations.oldForestRoad
      run $ ResolveDiscover pid lid
      q <- fromJustNote "no Discover question" <$> getQuestion pid
      length (searchedCards q) `shouldBe` 3
      finishDiscover pid
      length <$> getEncounterDeck `shouldReturn` 1
      length <$> getEncounterDiscard `shouldReturn` 3

    it "protects other staged locations while allowing progress on either printing of Abandoned Mine and on the active location" . gameTest $ \_ -> do
      mine <- stageLocation Locations.abandonedMineBelegost
      oldMine <- stageLocation Locations.abandonedMine
      other <- stageLocation Locations.oldForestRoad
      active <- activeLocation Locations.oldForestRoad
      for_ [mine, oldMine, other, active] \lid -> run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
      (.progress) . toAttrs <$> getLocation mine `shouldReturn` 1
      (.progress) . toAttrs <$> getLocation oldMine `shouldReturn` 1
      (.progress) . toAttrs <$> getLocation other `shouldReturn` 0
      (.progress) . toAttrs <$> getLocation active `shouldReturn` 1
      run $ PlaceLocation active StagingArea
      run $ PlaceLocation mine ActiveLocation
      run $ PlaceTokens GameSource (LocationTarget other) Progress 1
      (.progress) . toAttrs <$> getLocation other `shouldReturn` 1

  describe "Flooded Hall" do
    for_ [1 .. 4] \n ->
      it ("reveals up to " <> show n <> " looked-at Hazards for " <> show n <> " players") . belegostTable n $ \players -> do
        let lead = fromJustNote "no player" $ headMay players
        lid <- activeLocation Locations.floodedHall
        _ <- setEncounterDeck $ replicate 4 Enemies.namelessCaveDweller <> [Locations.oldForestRoad]
        run $ ResolveDiscover lead lid
        finishDiscover lead
        length <$> select (EnemyWithCardCode $ toCardCode Enemies.namelessCaveDweller) `shouldReturn` n
        length <$> getEncounterDiscard `shouldReturn` 4 - n
        length <$> getEncounterDeck `shouldReturn` 1

    it "lets the first player choose the additional Hazard after the random first one" . twoPlayerTest testDeck $ \one two -> do
      lid <- activeLocation Locations.floodedHall
      _ <- setEncounterDeck $ replicate 4 Enemies.namelessCaveDweller <> [Locations.oldForestRoad]
      run $ ResolveDiscover one lid
      chooseLabel "$label.continue" one
      q <- fromJustNote "no additional Hazard choice" <$> getQuestion one
      let targets = [t | TargetLabel t _ <- questionChoices q]
      length targets `shouldBe` 3
      let chosen = fromJustNote "no target" $ lastMay targets
      getQuestion two `shouldReturn` Nothing
      chooseTarget one chosen
      case chosen of
        CardIdTarget cid -> getsGame (any ((== cid) . (.cardId) . toAttrs) . toList . (.entities.enemies)) `shouldReturn` True
        _ -> error "expected encounter card target"

    it "reveals only the available Hazards when there are fewer than players" . belegostTable 4 $ \players -> do
      let lead = fromJustNote "no player" $ headMay players
      lid <- activeLocation Locations.floodedHall
      _ <- setEncounterDeck [Enemies.namelessCaveDweller, Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ ResolveDiscover lead lid
      finishDiscover lead
      length <$> select (EnemyWithCardCode $ toCardCode Enemies.namelessCaveDweller) `shouldReturn` 1
      length <$> getEncounterDiscard `shouldReturn` 3

    it "increases actual ally play costs only while staged" . gameTest $ \pid -> do
      g <- getGame
      putGame $ g & phaseL .~ PlanningPhase
      hall <- stageLocation Locations.floodedHall
      card <- putCardIntoHand pid Allies.guardOfTheCitadel
      h <- heroOf pid "01002"
      run $ PlaceTokens GameSource h Resource 2
      player <- getPlayer pid
      canPlay player card `shouldReturn` False
      run $ PlaceLocation hall ActiveLocation
      canPlay player card `shouldReturn` True

    it "keeps the chosen Loot guarded and discards the unchosen Loot after revealing its Hazards" . twoPlayerTest testDeck $ \one _ -> do
      lid <- activeLocation Locations.floodedHall
      [keyCard, torchCard, _, _, _] <- setEncounterDeck [Objectives.keysOfBelegost, Objectives.dwarvenTorch, Enemies.namelessCaveDweller, Enemies.namelessCaveDweller, Locations.oldForestRoad]
      run $ ResolveDiscover one lid
      chooseTarget one keyCard
      finishDiscover one
      guardian <- (.cardId) . toAttrs <$> getLocation lid
      objectiveGuard <$> lootOf Objectives.keysOfBelegost `shouldReturn` Just guardian
      map (.id) <$> getEncounterDiscard `shouldReturn` [torchCard.id]
      length <$> select (EnemyWithCardCode $ toCardCode Enemies.namelessCaveDweller) `shouldReturn` 2

  describe "Darkened Tunnel" do
    it "reveals a discarded Hazard fully, including its when-revealed effect" . gameTest $ \pid -> do
      _ <- stageLocation Locations.darkenedTunnel
      active <- activeLocation Locations.oldForestRoad
      run $ PlaceTokens GameSource (LocationTarget active) Progress 2
      _ <- setEncounterDeck [Treacheries.slickFooting, Locations.oldForestRoad]
      run $ CheckWindows [mkAfter $ WhenRevealedCanceled pid GameTarget]
      resolveRevealed
      (.progress) . toAttrs <$> getLocation active `shouldReturn` 1
      length <$> getEncounterDeck `shouldReturn` 1

    it "discards a non-Hazard without revealing it" . gameTest $ \pid -> do
      _ <- stageLocation Locations.darkenedTunnel
      _ <- setEncounterDeck [Enemies.forestSpider, Locations.oldForestRoad]
      run $ CheckWindows [mkAfter $ WhenRevealedCanceled pid GameTarget]
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.forestSpider]
      select (EnemyWithCardCode $ toCardCode Enemies.forestSpider) `shouldReturn` []

    it "has no cancellation penalty while active and places a clock resource as its travel cost" . gameTest $ \pid -> do
      _ <- stageClock
      lid <- stageLocation Locations.darkenedTunnel
      _ <- setEncounterDeck $ replicate 4 Locations.oldForestRoad
      run $ TravelTo lid
      finishDiscover pid
      (.resources) <$> clock `shouldReturn` 1
      deckSize <- length <$> getEncounterDeck
      run $ CheckWindows [mkAfter $ WhenRevealedCanceled pid GameTarget]
      length <$> getEncounterDeck `shouldReturn` deckSize

    it "cannot pay its travel cost without Stalking the Ruins in play" . gameTest $ \_ -> do
      lid <- stageLocation Locations.darkenedTunnel
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  describe "Sunken Treasury" do
    it "counts only Loot attached to heroes in its threat" . gameTest $ \pid -> do
      lid <- stageLocation Locations.sunkenTreasury
      getLocationThreat lid `shouldReturn` 1
      claimLoot pid Objectives.keysOfBelegost
      claimLoot pid Objectives.mapOfEredLuin
      guardian <- (.cardId) . toAttrs <$> getLocation lid
      treasure <- genEncounterCard Objectives.dwarvenTorch
      run $ PutObjectiveIntoPlayGuardedBy treasure guardian
      getLocationThreat lid `shouldReturn` 3

    it "requires every player to have a card to discard, then discards one random card from each hand" . twoPlayerTest testDeck $ \one two -> do
      lid <- stageLocation Locations.sunkenTreasury
      _ <- putCardIntoHand one Allies.guardOfTheCitadel
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
      _ <- putCardIntoHand two Allies.guardOfTheCitadel
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
      _ <- setEncounterDeck $ replicate 4 Locations.oldForestRoad
      run $ TravelTo lid
      finishDiscover one
      length . (.hand) <$> getPlayer one `shouldReturn` 0
      length . (.hand) <$> getPlayer two `shouldReturn` 0

    it "exhausts one character controlled by the defending player as its shadow" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.forestSpider
      [card, _] <- setEncounterDeck [Locations.sunkenTreasury, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadowCard card.id eid two []
      h <- heroOf two "01012"
      other <- heroOf one "01012"
      chooseTarget two h
      case (h, other) of
        (HeroTarget hid, HeroTarget otherId) -> do
          heroExhausted . toAttrs <$> getHero hid `shouldReturn` True
          heroExhausted . toAttrs <$> getHero otherId `shouldReturn` False
        _ -> error "expected heroes"

  describe "Secret Chamber" do
    it "pays two threat per player and can guard a Loot objective from the discard pile after Discover finds none" . twoPlayerTest testDeck $ \one two -> do
      lid <- stageLocation Locations.secretChamber
      setEncounterDiscard [Objectives.keysOfBelegost]
      _ <- setEncounterDeck $ replicate 4 Locations.oldForestRoad
      run $ TravelTo lid
      finishDiscover one
      getThreat one `shouldReturn` 2
      getThreat two `shouldReturn` 2
      offersAbilityOf one (LocationTarget lid) `shouldReturn` True
      chooseAbilityOf one (LocationTarget lid)
      guardian <- (.cardId) . toAttrs <$> getLocation lid
      objectiveGuard <$> lootOf Objectives.keysOfBelegost `shouldReturn` Just guardian
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 2
      h <- heroOf one "01012"
      chooseTarget one h
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` AttachedTo h
      objectiveGuard <$> lootOf Objectives.keysOfBelegost `shouldReturn` Nothing

    it "offers no response when Discover already found Loot, even with another in discard" . gameTest $ \pid -> do
      lid <- stageLocation Locations.secretChamber
      setEncounterDiscard [Objectives.mapOfEredLuin]
      _ <- setEncounterDeck [Objectives.keysOfBelegost, Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ TravelTo lid
      finishDiscover pid
      offersAbilityOf pid (LocationTarget lid) `shouldReturn` False
      getQuestion pid `shouldReturn` Nothing

    it "offers no response when no Loot can be found in discard" . gameTest $ \pid -> do
      lid <- stageLocation Locations.secretChamber
      _ <- setEncounterDeck $ replicate 4 Locations.oldForestRoad
      run $ TravelTo lid
      finishDiscover pid
      getQuestion pid `shouldReturn` Nothing

  describe "Naurlhûg's Lair" do
    for_ [1 .. 4] \n ->
      it ("places " <> show n <> " resources on Stalking the Ruins to travel") . belegostTable n $ \players -> do
        let lead = fromJustNote "no player" $ headMay players
        _ <- stageClock
        lid <- stageLocation Locations.naurlhugsLair
        _ <- setEncounterDeck $ replicate 6 Locations.oldForestRoad
        run $ TravelTo lid
        finishDiscover lead
        (.resources) <$> clock `shouldReturn` n

    it "is immune to player cards and releases the hidden treasure faceup when explored" . gameTest $ \pid -> do
      oid <- stageClock
      treasure <- genEncounterCard Objectives.bookOfAulesChildren
      run $ PlaceUnderneath (ObjectiveTarget oid) [EncounterCard treasure]
      lid <- activeLocation Locations.naurlhugsLair
      getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 6
      h <- heroOf pid "01012"
      chooseTarget pid h
      (.objectiveUnderneath) <$> clock `shouldReturn` []
      (.placement) <$> lootOf Objectives.bookOfAulesChildren `shouldReturn` AttachedTo h
      (.cardId) <$> lootOf Objectives.bookOfAulesChildren `shouldReturn` treasure.id
      map toCardCode . (.victoryDisplay) . toAttrs <$> getsGame (.scenario) `shouldReturn` [toCardCode Locations.naurlhugsLair]

  describe "Loot" do
    for_ belegostLoot \def ->
      it ("claims freed " <> show (toCardCode def) <> " for a first-player hero") . twoPlayerTest testDeck $ \one two -> do
        lid <- activeLocation Locations.oldForestRoad
        card <- genEncounterCard def
        guardian <- (.cardId) . toAttrs <$> getLocation lid
        run $ PutObjectiveIntoPlayGuardedBy card guardian
        (.placement) <$> lootOf def `shouldReturn` StagingArea
        lootAvailable one def `shouldReturn` False
        run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
        h1 <- heroOf one "01012"
        h2 <- heroOf two "01012"
        question <- fromJustNote "no claim question" <$> getQuestion one
        let hosts = [t | TargetLabel t _ <- questionChoices question]
        hosts `shouldSatisfy` elem h1
        hosts `shouldSatisfy` notElem h2
        chooseTarget one h1
        (.placement) <$> lootOf def `shouldReturn` AttachedTo h1

    it "waits without looping when no first-player hero can receive an attachment" . gameTest $ \pid -> do
      for_ ["01012", "01007", "01002"] \code -> do
        h <- heroOf pid code
        run $ CreateEffect $ Effect GameSource h [CannotHaveAttachments] UntilEndOfPhase
      run . AddToStaging =<< genEncounterCard Objectives.keysOfBelegost
      getQuestion pid `shouldReturn` Nothing
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` StagingArea
      run $ EndEffects UntilEndOfPhase
      h <- heroOf pid "01012"
      chooseTarget pid h
      (.placement) <$> lootOf Objectives.keysOfBelegost `shouldReturn` AttachedTo h

    it "respects required attachment traits and restrictions on Restricted cards" . gameTest $ \pid -> do
      h <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource h [CannotHaveAttachmentsWithoutTrait Weapon] UntilEndOfPhase
      for_ ["01007", "01002"] \code -> do
        other <- heroOf pid code
        run $ CreateEffect $ Effect GameSource other [CannotHaveAttachments] UntilEndOfPhase
      run . AddToStaging =<< genEncounterCard Objectives.mapOfEredLuin
      getQuestion pid `shouldReturn` Nothing
      run $ CreateEffect $ Effect GameSource h [CannotHaveRestrictedAttachments] UntilEndOfPhase
      run . AddToStaging =<< genEncounterCard Objectives.swordOfBelegost
      getQuestion pid `shouldReturn` Nothing
      (.placement) <$> lootOf Objectives.swordOfBelegost `shouldReturn` StagingArea

    it "discards Loot to the encounter pile when its hero leaves play" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      h <- heroOf pid "01012"
      run $ Destroy GameSource h
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.keysOfBelegost]
      getsGame (any ((== toCardCode Objectives.keysOfBelegost) . toCardCode) . toList . (.entities.objectives)) `shouldReturn` False

    it "only enters play through Discover when revealed from the encounter deck" . gameTest $ \pid -> do
      _ <- setEncounterDeck [Objectives.keysOfBelegost, Enemies.forestSpider, Locations.oldForestRoad]
      run RevealEncounterCard
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.keysOfBelegost]
      select (EnemyWithCardCode $ toCardCode Enemies.forestSpider) >>= (`shouldSatisfy` notNull)
      getQuestion pid `shouldReturn` Nothing

    it "keeps discovered Loot guarded until its location is explored" . gameTest $ \pid -> do
      lid <- activeLocation Locations.moonlitPassage
      _ <- setEncounterDeck [Objectives.keysOfBelegost, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ ResolveDiscover pid lid
      chooseLabel "$label.continue" pid
      lootAvailable pid Objectives.keysOfBelegost `shouldReturn` False
      run $ PlaceTokens GameSource (LocationTarget lid) Progress 4
      h <- heroOf pid "01012"
      chooseTarget pid h
      lootAvailable pid Objectives.keysOfBelegost `shouldReturn` True

  describe "Sword of Belegost" do
    it "gives the attached hero four attack even while the Sword is exhausted" . gameTest $ \pid -> do
      claimLoot pid Objectives.swordOfBelegost
      h <- heroOf pid "01012"
      sword <- (.id) <$> lootOf Objectives.swordOfBelegost
      getAttack h `shouldReturn` 6
      run $ Exhaust (ObjectiveTarget sword)
      getAttack h `shouldReturn` 6
      run $ Discard GameSource (ObjectiveTarget sword)
      getAttack h `shouldReturn` 2
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.swordOfBelegost]

    it "cancels a shadow on an attack against another player without exhausting its hero" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.swordOfBelegost
      sword <- (.id) <$> lootOf Objectives.swordOfBelegost
      h <- heroOf one "01012"
      eid <- stageEnemy Enemies.forestSpider
      _ <- setEncounterDeck [Enemies.namelessCaveDweller, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadow eid two []
      chooseAbilityOf one (ObjectiveTarget sword)
      getEnemyAttack eid `shouldReturn` 2
      (.exhausted) <$> lootOf Objectives.swordOfBelegost `shouldReturn` True
      case h of
        HeroTarget hid -> getsGame (maybe True (heroExhausted . toAttrs) . Map.lookup hid . (.entities.heroes)) `shouldReturn` False
        _ -> error "expected hero"

    it "cannot cancel explicitly uncancelable shadow effects" . gameTest $ \pid -> do
      claimLoot pid Objectives.swordOfBelegost
      sword <- (.id) <$> lootOf Objectives.swordOfBelegost
      eid <- stageEnemy Enemies.forestSpider
      run $ CreateEffect $ Effect GameSource GameTarget [ShadowCannotBeCanceled] UntilEndOfPhase
      _ <- setEncounterDeck [Enemies.namelessCaveDweller, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadow eid pid []
      offersAbilityOf pid (ObjectiveTarget sword) `shouldReturn` False
      getEnemyAttack eid `shouldReturn` 4
      (.exhausted) <$> lootOf Objectives.swordOfBelegost `shouldReturn` False

    it "counts towards the restricted limit and can itself be discarded for it" . gameTest $ \pid -> do
      h <- heroOf pid "01012"
      _ <- putAttachmentOn pid Attachments.daggerOfWesternesse h
      _ <- putAttachmentOn pid Attachments.daggerOfWesternesse h
      run . AddToStaging =<< genEncounterCard Objectives.swordOfBelegost
      chooseTarget pid h
      sword <- (.id) <$> lootOf Objectives.swordOfBelegost
      chooseTarget pid (ObjectiveTarget sword)
      map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.swordOfBelegost]

  describe "Map of Ered Luin" do
    it "reduces a non-unique location's threat until phase end" . gameTest $ \pid -> do
      claimLoot pid Objectives.mapOfEredLuin
      lid <- activeLocation Locations.oldForestRoad
      useLoot pid Objectives.mapOfEredLuin
      chooseTarget pid lid
      getLocationThreat lid `shouldReturn` 0
      (.exhausted) <$> lootOf Objectives.mapOfEredLuin `shouldReturn` True
      lootAvailable pid Objectives.mapOfEredLuin `shouldReturn` False
      run $ EndEffects UntilEndOfPhase
      getLocationThreat lid `shouldReturn` 1

    it "has no action when only unique locations are in play" . gameTest $ \pid -> do
      claimLoot pid Objectives.mapOfEredLuin
      _ <- activeLocation Locations.withywindle
      lootAvailable pid Objectives.mapOfEredLuin `shouldReturn` False

  describe "Keys of Belegost" do
    it "adds a resource to any player's hero and only exhausts the Keys" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.keysOfBelegost
      target <- heroOf two "01012"
      useLoot one Objectives.keysOfBelegost
      chooseTarget one target
      case target of
        HeroTarget hid -> getHeroResources hid `shouldReturn` 1
        _ -> error "expected hero"
      (.exhausted) <$> lootOf Objectives.keysOfBelegost `shouldReturn` True
      lootAvailable two Objectives.keysOfBelegost `shouldReturn` False

    it "offers no action when every hero is barred from encounter resource gains" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      for_ ["01012", "01007", "01002"] \code -> do
        h <- heroOf pid code
        run $ CreateEffect $ Effect GameSource h [CannotGainResourcesFromCardEffects] UntilEndOfPhase
      lootAvailable pid Objectives.keysOfBelegost `shouldReturn` False
      run $ EndEffects UntilEndOfPhase
      lootAvailable pid Objectives.keysOfBelegost `shouldReturn` True

    it "does not treat encounter resources as a player-card effect" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      h <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource h [CannotGainResourcesFromPlayerCardEffects] UntilEndOfPhase
      useLoot pid Objectives.keysOfBelegost
      chooseTarget pid h
      case h of
        HeroTarget hid -> getHeroResources hid `shouldReturn` 1
        _ -> error "expected hero"

    it "excludes a hero who can gain resources only from Treasure cards" . gameTest $ \pid -> do
      claimLoot pid Objectives.keysOfBelegost
      h <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource h [CannotGainResourcesFromNonTreasureCards] UntilEndOfPhase
      useLoot pid Objectives.keysOfBelegost
      question <- fromJustNote "no resource target question" <$> getQuestion pid
      [t | TargetLabel t _ <- questionChoices question] `shouldSatisfy` notElem h

    it "follows its hero's controller when that hero changes control" . twoPlayerTest (coreDeck "tactics") $ \one two -> do
      claimLoot one Objectives.keysOfBelegost
      h <- heroOf one "01012"
      case h of
        HeroTarget hid -> run $ ChangeHeroController hid two
        _ -> error "expected hero"
      lootAvailable one Objectives.keysOfBelegost `shouldReturn` False
      lootAvailable two Objectives.keysOfBelegost `shouldReturn` True

  describe "Book of Aulë's Children" do
    it "draws a card for the chosen player" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.bookOfAulesChildren
      handSize <- length . (.hand) <$> getPlayer two
      useLoot one Objectives.bookOfAulesChildren
      chooseTarget one two
      length . (.hand) <$> getPlayer two `shouldReturn` handSize + 1
      (.exhausted) <$> lootOf Objectives.bookOfAulesChildren `shouldReturn` True

    it "has no action while its only player cannot draw from card effects" . gameTest $ \pid -> do
      claimLoot pid Objectives.bookOfAulesChildren
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDrawCardsFromCardEffects] UntilEndOfPhase
      lootAvailable pid Objectives.bookOfAulesChildren `shouldReturn` False
      run $ EndEffects UntilEndOfPhase
      lootAvailable pid Objectives.bookOfAulesChildren `shouldReturn` True

    it "can draw through a prohibition on player-card draws" . gameTest $ \pid -> do
      claimLoot pid Objectives.bookOfAulesChildren
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDrawCardsFromPlayerCardEffects] UntilEndOfPhase
      useLoot pid Objectives.bookOfAulesChildren
      length . (.hand) <$> getPlayer pid `shouldReturn` 1

    it "has no action when all player decks are empty" . gameTest $ \pid -> do
      claimLoot pid Objectives.bookOfAulesChildren
      run $ DiscardTopOfDeck pid 100
      lootAvailable pid Objectives.bookOfAulesChildren `shouldReturn` False

  describe "Dwarven Torch" do
    it "weakens attack and defense for the phase, including enemies immune to player cards" . gameTest $ \pid -> do
      claimLoot pid Objectives.dwarvenTorch
      eid <- stageEnemy Enemies.forestSpider
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [ImmuneToPlayerCardEffects] UntilEndOfPhase
      useLoot pid Objectives.dwarvenTorch
      chooseTarget pid eid
      getEnemyAttack eid `shouldReturn` 1
      getEnemyDefense eid `shouldReturn` 0
      run $ EndEffects UntilEndOfPhase
      getEnemyAttack eid `shouldReturn` 2
      getEnemyDefense eid `shouldReturn` 1

    it "cannot target unique enemies" . gameTest $ \pid -> do
      claimLoot pid Objectives.dwarvenTorch
      _ <- stageEnemy Enemies.naurlhug
      lootAvailable pid Objectives.dwarvenTorch `shouldReturn` False

  describe "Blue Mountain Gem" do
    it "reduces any eligible player's threat by one as an encounter effect" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.blueMountainGem
      run $ RaiseThreat two 5
      run $ CreateEffect $ Effect GameSource (PlayerTarget two) [PlayerCardThreatReductionLimit 0] UntilEndOfPhase
      useLoot one Objectives.blueMountainGem
      getThreat two `shouldReturn` 4
      (.exhausted) <$> lootOf Objectives.blueMountainGem `shouldReturn` True

    it "cannot be spent at zero threat or against a threat floor" . gameTest $ \pid -> do
      claimLoot pid Objectives.blueMountainGem
      lootAvailable pid Objectives.blueMountainGem `shouldReturn` False
      run $ RaiseThreat pid 10
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotReduceThreatBelow 10] UntilEndOfPhase
      lootAvailable pid Objectives.blueMountainGem `shouldReturn` False
      run $ EndEffects UntilEndOfPhase
      lootAvailable pid Objectives.blueMountainGem `shouldReturn` True

    it "respects a spent round-wide threat reduction limit" . gameTest $ \pid -> do
      claimLoot pid Objectives.blueMountainGem
      run $ RaiseThreat pid 10
      run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [ThreatReductionPerRoundLimit 1, ThreatReducedThisRound 1] UntilEndOfRound
      lootAvailable pid Objectives.blueMountainGem `shouldReturn` False

  describe "Stalking the Ruins" do
    it "counts resources, not progress, and summons the set-aside dragon on the third quest phase" . gameTest $ \_ -> do
      oid <- stageClock
      dragon <- genEncounterCard Enemies.naurlhug
      run $ SetAside dragon
      questBegins
      (.resources) <$> clock `shouldReturn` 1
      (.progress) <$> clock `shouldReturn` 0
      select (EnemyWithCardCode $ toCardCode Enemies.naurlhug) `shouldReturn` []
      questBegins
      (.resources) <$> clock `shouldReturn` 2
      questBegins
      eid <- enemyOf Enemies.naurlhug
      (.cardId) . toAttrs <$> getEnemy eid `shouldReturn` dragon.id
      getEnemyThreat eid `shouldReturn` 3
      run $ RemoveTokens GameSource (ObjectiveTarget oid) Resource 2
      getEnemyThreat eid `shouldReturn` 1

    it "returns an engaged dragon to staging at three or more resources without recreating it" . gameTest $ \pid -> do
      oid <- stageClock
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 4
      eid <- stageEnemy Enemies.naurlhug
      run $ EngageEnemy pid eid
      questBegins
      (.resources) <$> clock `shouldReturn` 5
      select (EnemyWithCardCode $ toCardCode Enemies.naurlhug) `shouldReturn` [eid]
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "clears every token and sets the dragon aside before the new stage is revealed" . gameTest $ \pid -> do
      oid <- stageClock
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 5
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Progress 2
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Damage 1
      eid <- stageEnemy Enemies.naurlhug
      dragon <- (.cardId) . toAttrs <$> getEnemy eid
      run $ EngageEnemy pid eid
      run . AdvanceToQuest =<< genEncounterCard Quests.theWickedWillow
      (.resources) <$> clock `shouldReturn` 0
      (.progress) <$> clock `shouldReturn` 0
      getDamage (ObjectiveTarget oid) `shouldReturn` 0
      select (EnemyWithCardCode $ toCardCode Enemies.naurlhug) `shouldReturn` []
      getsGame (any ((== dragon) . (.id)) . (.scenarioSetAside) . toAttrs . (.scenario)) `shouldReturn` True

    it "can summon the same physical dragon again after retreat" . gameTest $ \_ -> do
      oid <- stageClock
      eid <- stageEnemy Enemies.naurlhug
      dragon <- (.cardId) . toAttrs <$> getEnemy eid
      run $ CheckWindows [mkWhen AdvancedToStage]
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 2
      questBegins
      returning <- enemyOf Enemies.naurlhug
      (.cardId) . toAttrs <$> getEnemy returning `shouldReturn` dragon
      getsGame (filter ((== dragon) . (.id)) . (.scenarioSetAside) . toAttrs . (.scenario)) `shouldReturn` []

  describe "Naurlhûg" do
    it "has zero variable threat without Stalking the Ruins and cannot take damage" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.naurlhug
      getEnemyThreat eid `shouldReturn` 0
      getModifiers eid >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
      run $ DealDamage GameSource (EnemyTarget eid) 4
      getDamage (EnemyTarget eid) `shouldReturn` 0

    it "assigns X damage only among the defending player's characters when it attacks" . twoPlayerTest testDeck $ \one two -> do
      oid <- stageClock
      run $ PlaceTokens GameSource (ObjectiveTarget oid) Resource 2
      eid <- stageEnemy Enemies.naurlhug
      h <- heroOf two "01012"
      other <- heroOf one "01012"
      run $ CheckWindows [mkWhen $ EnemyAttacking eid two]
      chooseTarget two h
      chooseTarget two h
      getDamage h `shouldReturn` 2
      getDamage other `shouldReturn` 0

    it "assigns no extra damage when the clock has no resources" . gameTest $ \pid -> do
      _ <- stageClock
      eid <- stageEnemy Enemies.naurlhug
      run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
      getQuestion pid `shouldReturn` Nothing

  describe "Orc of Ered Luin" do
    it "makes no revelation attack when nobody controls Loot" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.orcOfEredLuin
      run $ WhenRevealed (EnemyTarget eid)
      getQuestion pid `shouldReturn` Nothing

    it "attacks each Loot holder once in player order, remaining in staging" . twoPlayerTest testDeck $ \one two -> do
      claimLoot one Objectives.cryptKey
      claimLoot one Objectives.oldLantern
      claimLoot two Objectives.scathasTeeth
      h1 <- strongDefender one
      h2 <- strongDefender two
      eid <- stageEnemy Enemies.orcOfEredLuin
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ WhenRevealed (EnemyTarget eid)
      passAttackActions
      getQuestion two `shouldReturn` Nothing
      defendAttack one h1
      getQuestion one `shouldReturn` Nothing
      defendAttack two h2
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion one `shouldReturn` Nothing
      getQuestion two `shouldReturn` Nothing

    it "skips players without Loot when revealing the Orc" . twoPlayerTest testDeck $ \one two -> do
      claimLoot two Objectives.cryptKey
      h <- strongDefender two
      eid <- stageEnemy Enemies.orcOfEredLuin
      _ <- setEncounterDeck [Locations.oldForestRoad, Locations.oldForestRoad]
      run $ WhenRevealed (EnemyTarget eid)
      passAttackActions
      getQuestion one `shouldReturn` Nothing
      defendAttack two h
      length <$> getEncounterDeck `shouldReturn` 1

    it "moves the shadow's attacker to the next player and attacks after the original attack" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.forestSpider
      h <- strongDefender two
      [card, _, _] <- setEncounterDeck [Enemies.orcOfEredLuin, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      pushAll [ResolveShadowCard card.id eid one [], CheckWindows [mkAfter $ EnemyAttacking eid one]]
      void mainLoop
      defendAttack two h
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` EngagedWith two
      length <$> getEncounterDeck `shouldReturn` 1

    it "makes no next-player attack in a solo game (FAQ 1.46)" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      [card, _] <- setEncounterDeck [Enemies.orcOfEredLuin, Locations.oldForestRoad]
      run $ DealShadowCard eid
      pushAll [ResolveShadowCard card.id eid pid [], CheckWindows [mkAfter $ EnemyAttacking eid pid]]
      void mainLoop
      getQuestion pid `shouldReturn` Nothing
      length <$> getEncounterDeck `shouldReturn` 1

  describe "Blue Mountain Goblin" do
    it "gains surge from any hero's Loot but not from guarded Loot" . twoPlayerTest testDeck $ \_ two -> do
      eid <- stageEnemy Enemies.blueMountainGoblin
      getModifiers eid >>= (`shouldSatisfy` notElem (GainKeyword Keyword.Surge))
      run . (`PutObjectiveIntoPlayAttached` (EnemyTarget eid)) =<< genEncounterCard Objectives.cryptKey
      getModifiers eid >>= (`shouldSatisfy` notElem (GainKeyword Keyword.Surge))
      claimLoot two Objectives.oldLantern
      getModifiers eid >>= (`shouldSatisfy` elem (GainKeyword Keyword.Surge))

    it "actually surges on revelation while Loot is held" . gameTest $ \pid -> do
      claimLoot pid Objectives.cryptKey
      _ <- setEncounterDeck [Enemies.blueMountainGoblin, Enemies.forestSpider, Locations.oldForestRoad]
      run RevealEncounterCard
      length <$> getEncounterDeck `shouldReturn` 1
      select (EnemyWithCardCode $ toCardCode Enemies.forestSpider) >>= (`shouldSatisfy` notNull)

    it "enters from shadow engaged with its Loot holder and is dealt a facedown shadow, without surging" . gameTest $ \pid -> do
      claimLoot pid Objectives.cryptKey
      eid <- stageEnemy Enemies.forestSpider
      [card, shadow, _] <- setEncounterDeck [Enemies.blueMountainGoblin, Locations.oldForestRoad, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadowCard card.id eid pid []
      goblin <- enemyOf Enemies.blueMountainGoblin
      a <- toAttrs <$> getEnemy goblin
      a.placement `shouldBe` EngagedWith pid
      map (.id) a.shadowCards `shouldBe` [shadow.id]
      length <$> getEncounterDeck `shouldReturn` 1
      getQuestion pid `shouldReturn` Nothing

    it "does not enter from shadow when only another player controls Loot" . twoPlayerTest testDeck $ \one two -> do
      claimLoot two Objectives.cryptKey
      eid <- stageEnemy Enemies.forestSpider
      [card, _] <- setEncounterDeck [Enemies.blueMountainGoblin, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ ResolveShadowCard card.id eid one []
      select (EnemyWithCardCode $ toCardCode Enemies.blueMountainGoblin) `shouldReturn` []
      length <$> getEncounterDeck `shouldReturn` 1

    it "opens enters-play reactions when the shared shadow-to-enemy operation puts an enemy into play" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.forestSpider
      deckSize <- length . (.deck) <$> getPlayer pid
      [card, _] <- setEncounterDeck [Enemies.hobgoblin, Locations.oldForestRoad]
      run $ DealShadowCard eid
      run $ PutShadowEnemyIntoPlay eid card.id pid
      length . (.deck) <$> getPlayer pid `shouldReturn` deckSize - 1
      length <$> playerCardsGuardedBy card.id `shouldReturn` 1

  describe "Lurker of the Depths" do
    it "is considered engaged with every player while remaining in staging" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      for_ [one, two] \pid -> do
        select (EnemyEngagedWith $ PlayerWithId pid) >>= (`shouldSatisfy` elem eid)
        ownAttackers pid eid >>= (`shouldSatisfy` notNull)
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "rejects both forced engagement and direct engaged placement" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      run $ EngageEnemy pid eid
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea
      run $ PlaceEnemy eid (EngagedWith pid)
      (.placement) . toAttrs <$> getEnemy eid `shouldReturn` StagingArea

    it "takes at most four damage across separate hits each round and can be defeated" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 3
      getDamage (EnemyTarget eid) `shouldReturn` 4
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 8
      getDamage (EnemyTarget eid) `shouldReturn` 8
      run $ EndEffects UntilEndOfRound
      run $ PlaceTokens GameSource (EnemyTarget eid) Damage 8
      run $ CheckDefeated GameSource (EnemyTarget eid)
      getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))
        `shouldReturn` [toCardCode Enemies.lurkerOfTheDepths]

    it "prohibits attachments without preventing removal from play" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.lurkerOfTheDepths
      getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)
      attachmentHosts (AttachToEnemy AnyEnemy) >>= (`shouldSatisfy` notElem (EnemyTarget eid))
      run $ Discard GameSource (EnemyTarget eid)
      getsGame (any ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` False

  describe "Nameless Cave Dweller" do
    it "has one threat, attack and defense without an active Discover location" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      stats eid `shouldReturn` [1, 1, 1]
      _ <- activeLocation Locations.oldForestRoad
      stats eid `shouldReturn` [1, 1, 1]
      getModifiers eid >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)
      attachmentHosts (AttachToEnemy AnyEnemy) >>= (`shouldSatisfy` notElem (EnemyTarget eid))

    it "reads the active location's modified Discover value dynamically" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      lid <- activeLocation Locations.hauntedHall
      stats eid `shouldReturn` [5, 5, 5]
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [DiscoverValueModifier 2] UntilEndOfPhase
      stats eid `shouldReturn` [7, 7, 7]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfPhase
      stats eid `shouldReturn` [1, 1, 1]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]
      run $ PlaceLocation lid StagingArea
      stats eid `shouldReturn` [1, 1, 1]

    it "loses its variable stats while its text is blank" . gameTest $ \_ -> do
      eid <- stageEnemy Enemies.namelessCaveDweller
      _ <- activeLocation Locations.hauntedHall
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfPhase
      stats eid `shouldReturn` [0, 0, 0]
      run $ EndEffects UntilEndOfPhase
      stats eid `shouldReturn` [5, 5, 5]

    for_ [False, True] \hasLoot ->
      it ("gives its shadow +" <> if hasLoot then "2 when the defender controls Loot" else "1 without Loot") . gameTest $ \pid -> do
        when hasLoot $ claimLoot pid Objectives.cryptKey
        eid <- stageEnemy Enemies.namelessCaveDweller
        cid <- getsGame $ (.cardId) . toAttrs . fromJustNote "missing dweller" . find ((== eid) . toId) . toList . (.entities.enemies)
        run $ ResolveShadowEffect cid eid pid []
        getEnemyAttack eid `shouldReturn` if hasLoot then 3 else 2
        run $ EndEffects (UntilEnemyAttackEnds eid)
        getEnemyAttack eid `shouldReturn` 1

    it "does not count another player's Loot for its shadow bonus" . twoPlayerTest testDeck $ \one two -> do
      claimLoot two Objectives.cryptKey
      eid <- stageEnemy Enemies.namelessCaveDweller
      cid <- (.cardId) . toAttrs <$> getEnemy eid
      run $ ResolveShadowEffect cid eid one []
      getEnemyAttack eid `shouldReturn` 2

  describe "Old Stone Troll" do
    it "reduces engagement cost for each claimed Loot, excluding guarded Loot" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      getEngagementCost eid `shouldReturn` 50
      claimLoot pid Objectives.cryptKey
      claimLoot pid Objectives.oldLantern
      getEngagementCost eid `shouldReturn` 30
      run . (`PutObjectiveIntoPlayAttached` (EnemyTarget eid)) =<< genEncounterCard Objectives.scathasTeeth
      getEngagementCost eid `shouldReturn` 30
      getModifiers eid >>= (`shouldSatisfy` elem CannotHavePlayerAttachments)

    it "allows only its engaged player's attackers, while permitting sentinel defense" . twoPlayerTest testDeck $ \one two -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      other <- heroOf two "01012"
      run $ CreateEffect $ Effect GameSource other [GainKeyword Ranged, GainKeyword Sentinel] UntilEndOfRound
      run $ EngageEnemy one eid
      ownAttackers one eid >>= (`shouldSatisfy` notNull)
      ownAttackers two eid `shouldReturn` []
      mayAttack one eid >>= (`shouldSatisfy` notElem other)
      canAttack eid other `shouldReturn` False
      canDefend eid other `shouldReturn` True
      getModifiers eid >>= (`shouldSatisfy` notElem OnlyEngagedPlayerFights)
      run $ DeclareRangedAttackers one eid [two]
      getQuestion two `shouldReturn` Nothing

    it "does not allow staging attacks before it engages a player" . gameTest $ \pid -> do
      eid <- stageEnemy Enemies.oldStoneTroll
      hero <- heroOf pid "01012"
      run $ CreateEffect $ Effect GameSource hero [CanAttackStagingArea] UntilEndOfRound
      ownAttackers pid eid `shouldReturn` []
