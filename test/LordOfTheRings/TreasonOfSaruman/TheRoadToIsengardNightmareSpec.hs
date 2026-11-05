module LordOfTheRings.TreasonOfSaruman.TheRoadToIsengardNightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getEngagementCost)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationCardId))
import LordOfTheRings.Message.Lifted qualified as Lifted
import LordOfTheRings.Game.Combat qualified as Combat
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Scenario.Types (modeL, GameMode (NightmareMode), scenarioSetAside)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank, CannotDiscardFromHand, CannotBeReadiedByPlayerCards, ImmuneToCardEffects, CannotAttack, CannotReady, ThreatModifier))
import LordOfTheRings.Scenario (theRoadToIsengard)
import LordOfTheRings.Window (WindowType (EnemyAttacking), mkWhen)
import TestImport

isengard :: (PlayerId -> GameT ()) -> IO ()
isengard = gameTestIn theRoadToIsengard testDeck

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn (overAttrs (modeL .~ NightmareMode) theRoadToIsengard) testDeck

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

controlledEnt :: PlayerId -> GameT Target
controlledEnt pid = do
  card <- genEncounterCard Objectives.entOfFangorn
  run $ AddToStaging card
  oid <- getsGame (toId . fromJustNote "Ent objective" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.objectives))
  run $ PutObjectiveAllyIntoPlay oid pid
  getsGame (toTarget . fromJustNote "Ent ally" . find ((== card.id) . allyCardId . toAttrs) . toList . (.entities.allies))

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

emptyHand :: PlayerId -> GameT ()
emptyHand pid = putGame . (playersL . ix pid . handL .~ []) =<< getGame

revealArcher :: GameT ()
revealArcher = do
  _ <- setEncounterDeck [Enemies.orthancArcher]
  run RevealEncounterCard
  resolveRevealed

halfOrcAttacks :: EnemyId -> PlayerId -> GameT ()
halfOrcAttacks eid pid = run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged location" . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

enemyStats :: EnemyId -> GameT [Int]
enemyStats eid = sequence [getEnemyThreat eid, getEnemyAttack eid, getEnemyDefense eid]

resolveForcedChoices :: PlayerId -> GameT ()
resolveForcedChoices pid = getQuestion pid >>= \case
  Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> chooseAbility pid >> resolveForcedChoices pid
  _ -> pure ()

spec :: Spec
spec = describe "The Road to Isengard Nightmare" do
  it "choosing Traitor's discard option does not schedule the unchosen extra attack" . isengard $ \pid -> do
    emptyHand pid
    card <- putCardIntoHand pid Events.fortuneOrFate
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- setEncounterDeck [Enemies.hostOfIsengard]
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.traitorOfManyColors
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid hero
    passActions pid
    chooseLabel "$label.traitorOfManyColors.discard" pid
    passActions pid
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.hostOfIsengard]
    getQuestion pid `shouldReturn` Nothing

  it "Traitor's shadow must discard when the enemy cannot make another attack" . isengard $ \pid -> do
    emptyHand pid
    card <- putCardIntoHand pid Events.fortuneOrFate
    eid <- stage Enemies.forestSpider
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttack] UntilEndOfRound
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.traitorOfManyColors
    run $ ResolveShadow eid pid []
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    getQuestion pid `shouldReturn` Nothing

  it "Daunting Power prevents Quick Strike from declaring an attacker" . isengard $ \pid -> do
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    reveal [Treacheries.dauntingPower]
    card <- genPlayerCard Events.quickStrike (Just pid)
    run $ PlayEvent pid card []
    getQuestion pid `shouldReturn` Nothing
    selectCharacters AttackingHero AttackingAlly `shouldReturn` []
    getDamage (EnemyTarget eid) `shouldReturn` 0

  it "Fire and Foul Fumes assigns damage separately to each player's questing characters" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
    lid <- stageLocation Locations.poisonousShaft
    run $ PlaceLocation lid ActiveLocation
    firstHero <- toTarget . fromJustNote "first hero" . headMay <$> select (HeroControlledBy $ PlayerWithId one)
    otherHero <- toTarget . fromJustNote "other hero" . headMay <$> select (HeroControlledBy $ PlayerWithId two)
    run $ CommitToQuest firstHero
    run $ CommitToQuest otherHero
    reveal [Treacheries.fireAndFoulFumes]
    replicateM_ 2 $ chooseTarget one firstHero
    replicateM_ 2 $ chooseTarget two otherHero
    getDamage firstHero `shouldReturn` 2
    getDamage otherHero `shouldReturn` 2

  it "Traitor of Many Colors makes Saruman attack and then surges" . isengard $ \pid -> do
    _ <- stage Enemies.sarumanTheRoadToIsengard
    ally <- putAllyIntoPlay pid Allies.snowbournScout
    reveal [Treacheries.traitorOfManyColors, Enemies.hostOfIsengard, Locations.poisonousShaft]
    passActions pid
    chooseTarget pid ally
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    select (LocationWithCardCode $ toCardCode Locations.poisonousShaft) >>= (`shouldSatisfy` (not . null))
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  for_ [True, False] \hasCard ->
    it ("Traitor's shadow makes an additional attack " <> if hasCard then "when chosen" else "automatically with no discardable card") . isengard $ \pid -> do
      emptyHand pid
      when hasCard $ void $ putCardIntoHand pid Events.fortuneOrFate
      eid <- stage Enemies.forestSpider
      run $ EngageEnemy pid eid
      firstHero <- toTarget <$> getHeroWithCode "01012"
      otherHero <- toTarget <$> getHeroWithCode "01007"
      _ <- setEncounterDeck [Enemies.hostOfIsengard]
      run . AttachShadowCard eid =<< genEncounterCard Treacheries.traitorOfManyColors
      run $ EnemyAttack pid eid
      passActions pid
      chooseTarget pid firstHero
      passActions pid
      when hasCard $ chooseLabel "$label.traitorOfManyColors.attack" pid
      passActions pid
      chooseTarget pid otherHero
      passActions pid
      resolveShadowsInOrder pid
      passActions pid
      getDamage otherHero `shouldReturn` 2

  it "a defeated Nightmare Ent is removed instead of entering the encounter discard" . nightmare $ \pid -> do
    ent <- controlledEnt pid
    run $ DealDamage GameSource ent 100
    getsGame (any ((== ent) . toTarget) . toList . (.entities.allies)) `shouldReturn` False
    getEncounterDiscard `shouldReturn` []

  it "builds the Nightmare deck with exact removals and preserves standard set-aside cards" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    length deck `shouldBe` 41
    map copies [Enemies.snaga, Locations.ringWallOfIsengard, Locations.openPit, Treacheries.machinesOfIsengard, Treacheries.blastOfSorcery] `shouldBe` [0, 2, 0, 2, 0]
    map copies [Enemies.orthancArcher, Enemies.halfOrcOfIsengard, Locations.poisonousShaft, Locations.fortressOfIsengard, Treacheries.traitorOfManyColors, Treacheries.fireAndFoulFumes, Treacheries.dauntingPower] `shouldBe` [3, 3, 3, 3, 2, 3, 2]
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    sort aside `shouldBe` sort (map toCardCode $ replicate 4 Objectives.entOfFangorn <> [Enemies.sarumanTheRoadToIsengard, Objectives.sarumansStaffTheRoadToIsengard, Locations.orthanc, Locations.gatesOfIsengard])

  for_ [("discarded", Discard GameSource), ("returned to hand", ReturnToHand), ("shuffled into a deck", ShuffleIntoOwnersDeck)] \(description, action) ->
    it ("removes a controlled Ent from the game when " <> description) . nightmare $ \pid -> do
      ent <- controlledEnt pid
      run $ action ent
      getsGame (any ((== ent) . toTarget) . toList . (.entities.allies)) `shouldReturn` False
      map toCardCode <$> getEncounterDiscard `shouldReturn` []
      player <- getPlayer pid
      any ((== toCardCode Objectives.entOfFangorn) . toCardCode) (player.hand <> player.deck <> player.discard) `shouldBe` False

  it "removes a staged Ent when discarded in Nightmare mode" . nightmare $ \_ -> do
    run . AddToStaging =<< genEncounterCard Objectives.entOfFangorn
    oid <- getsGame (toId . fromJustNote "Ent" . headMay . toList . (.entities.objectives))
    run $ Discard GameSource (ObjectiveTarget oid)
    getsGame (null . (.entities.objectives)) `shouldReturn` True
    getEncounterDiscard `shouldReturn` []

  it "removes a staged Ent when destroyed in Nightmare mode" . nightmare $ \_ -> do
    run . AddToStaging =<< genEncounterCard Objectives.entOfFangorn
    oid <- getsGame (toId . fromJustNote "Ent" . headMay . toList . (.entities.objectives))
    run $ Destroy GameSource (ObjectiveTarget oid)
    getsGame (null . (.entities.objectives)) `shouldReturn` True
    getEncounterDiscard `shouldReturn` []

  it "keeps the normal controlled Ent discard behavior" . isengard $ \pid -> do
    ent <- controlledEnt pid
    run $ Discard GameSource ent
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Objectives.entOfFangorn]

  it "Daunting Power blocks attackers including characters entering later and expires at round end" . isengard $ \pid -> do
    threatBefore <- (.threat) <$> getPlayer pid
    reveal [Treacheries.dauntingPower]
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 4)
    Combat.readyAttackers pid `shouldReturn` []
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    getModifiers ally >>= (`shouldSatisfy` elem CannotAttack)
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ AttackWith hero
    run $ AttackWith ally
    selectCharacters AttackingHero AttackingAlly `shouldReturn` []
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False
    Combat.readyAttackers pid `shouldReturn` []
    run $ EndEffects UntilEndOfRound
    Combat.readyAttackers pid >>= (`shouldSatisfy` elem ally)

  it "Daunting Power does not raise threat when any player has an engaged enemy" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy two eid
    threatsBefore <- traverse (fmap (.threat) . getPlayer) [one, two]
    reveal [Treacheries.dauntingPower]
    traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` threatsBefore
    Combat.readyAttackers one `shouldReturn` []
    Combat.readyAttackers two `shouldReturn` []

  it "Daunting Power's shadow prevents its defender from readying until round end" . isengard $ \pid -> do
    eid <- stage Enemies.forestSpider
    hero <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.dauntingPower
    run $ ResolveShadow eid pid [hero]
    getModifiers hero >>= (`shouldSatisfy` elem CannotReady)
    run $ EndEffects UntilEndOfRound
    getModifiers hero >>= (`shouldSatisfy` notElem CannotReady)

  it "Fire and Foul Fumes damages only controlled questing characters using modified location threat" . isengard $ \pid -> do
    lid <- stageLocation Locations.poisonousShaft
    run $ PlaceLocation lid ActiveLocation
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [ThreatModifier 1] UntilEndOfRound
    hero <- toTarget <$> getHeroWithCode "01012"
    otherHero <- toTarget <$> getHeroWithCode "01007"
    run $ CommitToQuest hero
    reveal [Treacheries.fireAndFoulFumes]
    replicateM_ 3 $ chooseTarget pid hero
    getDamage hero `shouldReturn` 3
    getDamage otherHero `shouldReturn` 0

  it "Fire and Foul Fumes surges without an active location" . isengard $ \pid -> do
    reveal [Treacheries.fireAndFoulFumes, Locations.poisonousShaft]
    select (LocationWithCardCode $ toCardCode Locations.poisonousShaft) >>= (`shouldSatisfy` (not . null))
    getQuestion pid `shouldReturn` Nothing

  it "Fire and Foul Fumes' shadow damages non-questing characters too" . isengard $ \pid -> do
    lid <- stageLocation Locations.poisonousShaft
    run $ PlaceLocation lid ActiveLocation
    eid <- stage Enemies.forestSpider
    hero <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.fireAndFoulFumes
    run $ ResolveShadow eid pid []
    replicateM_ 2 $ chooseTarget pid hero
    getDamage hero `shouldReturn` 2

  it "Traitor of Many Colors surges even when Saruman is absent" . isengard $ \pid -> do
    reveal [Treacheries.traitorOfManyColors, Locations.poisonousShaft]
    select (LocationWithCardCode $ toCardCode Locations.poisonousShaft) >>= (`shouldSatisfy` (not . null))
    getQuestion pid `shouldReturn` Nothing

  it "Traitor of Many Colors' shadow offers discarding instead of another attack" . isengard $ \pid -> do
    emptyHand pid
    card <- putCardIntoHand pid Events.fortuneOrFate
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.traitorOfManyColors
    run $ ResolveShadow eid pid []
    chooseLabel "$label.traitorOfManyColors.discard" pid
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    getQuestion pid `shouldReturn` Nothing

  it "Saruman's Poisonous Shaft shadow makes only the defending player discard" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
    saruman <- stage Enemies.sarumanTheRoadToIsengard
    firstAlly <- putAllyIntoPlay one Allies.guardOfTheCitadel
    otherAlly <- putAllyIntoPlay two Allies.guardOfTheCitadel
    for_ [firstAlly, otherAlly] (\ally -> run $ DealDamage GameSource ally 1)
    run . AttachShadowCard saruman =<< genEncounterCard Locations.poisonousShaft
    run $ ResolveShadow saruman one []
    chooseTarget one firstAlly
    getsGame (any ((== firstAlly) . toTarget) . toList . (.entities.allies)) `shouldReturn` False
    getsGame (any ((== otherAlly) . toTarget) . toList . (.entities.allies)) `shouldReturn` True
    getQuestion two `shouldReturn` Nothing

  it "Saruman's Fortress shadow returns the discarded enemy without revealing it" . isengard $ \pid -> do
    saruman <- stage Enemies.sarumanTheRoadToIsengard
    enemyCard <- genEncounterCard Enemies.orthancArcher
    run $ AddToEncounterDiscard [enemyCard]
    run . AttachShadowCard saruman =<< genEncounterCard Locations.fortressOfIsengard
    run $ ResolveShadow saruman pid []
    select (EnemyWithCardCode $ toCardCode Enemies.orthancArcher) >>= (`shouldSatisfy` (not . null))
    getQuestion pid `shouldReturn` Nothing

  it "Poisonous Shaft restricts only damaged characters and updates as they heal" . isengard $ \_ -> do
    lid <- stageLocation Locations.poisonousShaft
    hero <- toTarget <$> getHeroWithCode "01012"
    otherHero <- toTarget <$> getHeroWithCode "01007"
    getModifiers hero >>= (`shouldSatisfy` notElem CannotBeReadiedByPlayerCards)
    run $ DealDamage GameSource hero 1
    getModifiers hero >>= (`shouldSatisfy` elem CannotBeReadiedByPlayerCards)
    run $ Exhaust hero
    runQueueT $ Lifted.ready hero
    void mainLoop
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    runQueueT $ Lifted.encounterReadies hero
    void mainLoop
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False
    getModifiers otherHero >>= (`shouldSatisfy` notElem CannotBeReadiedByPlayerCards)
    run $ HealDamage GameSource hero 1
    getModifiers hero >>= (`shouldSatisfy` notElem CannotBeReadiedByPlayerCards)
    run $ DealDamage GameSource hero 1
    run $ PlaceLocation lid ActiveLocation
    getModifiers hero >>= (`shouldSatisfy` notElem CannotBeReadiedByPlayerCards)

  it "Poisonous Shaft's Wizardry discards a damaged character when explored" . isengard $ \pid -> do
    lid <- stageLocation Locations.poisonousShaft
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage GameSource ally 1
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
    resolveForcedChoices pid
    chooseTarget pid ally
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Poisonous Shaft's Wizardry leaves undamaged and immune characters alone" . isengard $ \pid -> do
    lid <- stageLocation Locations.poisonousShaft
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ DealDamage GameSource hero 1
    run $ CreateEffect $ Effect GameSource hero [ImmuneToCardEffects] UntilEndOfRound
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
    resolveForcedChoices pid
    getDamage hero `shouldReturn` 1
    getQuestion pid `shouldReturn` Nothing

  it "Machines of Isengard triggers Poisonous Shaft without exploring it" . isengard $ \pid -> do
    lid <- stageLocation Locations.poisonousShaft
    run $ PlaceLocation lid ActiveLocation
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage GameSource ally 1
    _ <- setEncounterDeck [Treacheries.machinesOfIsengard]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid ally
    (.progress) <$> getLocation lid `shouldReturn` 0
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Poisonous Shaft's Wizardry makes each player discard his own damaged character" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
    lid <- stageLocation Locations.poisonousShaft
    firstAlly <- putAllyIntoPlay one Allies.guardOfTheCitadel
    otherAlly <- putAllyIntoPlay two Allies.guardOfTheCitadel
    for_ [firstAlly, otherAlly] (\ally -> run $ DealDamage GameSource ally 1)
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
    resolveForcedChoices one
    chooseTarget one firstAlly
    chooseTarget two otherAlly
    getsGame (any ((`elem` [firstAlly, otherAlly]) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Fortress of Isengard boosts all Orc enemies while staged and copies stack" . isengard $ \pid -> do
    lid <- stageLocation Locations.fortressOfIsengard
    eid <- stage Enemies.orthancArcher
    otherEnemy <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    enemyStats eid `shouldReturn` [3, 4, 3]
    enemyStats otherEnemy `shouldReturn` [2, 2, 1]
    _ <- stageLocation Locations.fortressOfIsengard
    enemyStats eid `shouldReturn` [4, 5, 4]
    run $ PlaceLocation lid ActiveLocation
    enemyStats eid `shouldReturn` [3, 4, 3]

  it "Fortress of Isengard returns the topmost discarded enemy without revealing it" . isengard $ \pid -> do
    lid <- stageLocation Locations.fortressOfIsengard
    older <- genEncounterCard Enemies.forestSpider
    enemyCard <- genEncounterCard Enemies.orthancArcher
    cover <- genEncounterCard Locations.openPit
    run $ AddToEncounterDiscard [cover, enemyCard, older]
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
    resolveForcedChoices pid
    select (EnemyWithCardCode $ toCardCode Enemies.orthancArcher) >>= (`shouldSatisfy` (not . null))
    select (EnemyWithCardCode $ toCardCode Enemies.forestSpider) `shouldReturn` []
    map (.id) <$> getEncounterDiscard >>= (`shouldSatisfy` \ids -> enemyCard.id `notElem` ids && older.id `elem` ids)
    getQuestion pid `shouldReturn` Nothing

  it "Fortress of Isengard does nothing when there is no discarded enemy" . isengard $ \pid -> do
    lid <- stageLocation Locations.fortressOfIsengard
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
    resolveForcedChoices pid
    select AnyEnemy `shouldReturn` []
    getQuestion pid `shouldReturn` Nothing

  it "blanking the Nightmare locations suppresses their modifiers and exploration Wizardry" . isengard $ \pid -> do
    shaft <- stageLocation Locations.poisonousShaft
    fortress <- stageLocation Locations.fortressOfIsengard
    eid <- stage Enemies.orthancArcher
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ DealDamage GameSource hero 1
    for_ [shaft, fortress] \lid -> run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    getModifiers hero >>= (`shouldSatisfy` notElem CannotBeReadiedByPlayerCards)
    enemyStats eid `shouldReturn` [2, 3, 2]
    enemyCard <- genEncounterCard Enemies.forestSpider
    run $ AddToEncounterDiscard [enemyCard]
    run $ PlaceTokens GameSource (LocationTarget shaft) Progress 5
    run $ PlaceTokens GameSource (LocationTarget fortress) Progress 3
    resolveForcedChoices pid
    getDamage hero `shouldReturn` 1
    select (EnemyWithCardCode $ toCardCode Enemies.forestSpider) `shouldReturn` []

  it "Half-orc's shadow discards only after the attack destroys a character" . isengard $ \pid -> do
    emptyHand pid
    card <- putCardIntoHand pid Events.fortuneOrFate
    eid <- stage Enemies.forestSpider
    defender <- putAllyIntoPlay pid Allies.snowbournScout
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.halfOrcOfIsengard
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    getsGame (any ((== defender) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Half-orc's shadow leaves the hand alone when the defender survives" . isengard $ \pid -> do
    emptyHand pid
    card <- putCardIntoHand pid Events.fortuneOrFate
    eid <- stage Enemies.forestSpider
    defender <- toTarget <$> getHeroWithCode "01012"
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.halfOrcOfIsengard
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]

  it "Orthanc Archer deals no damage for a zero-cost card" . isengard $ \pid -> do
    emptyHand pid
    _ <- putCardIntoHand pid Events.weAreNotIdle
    revealArcher
    getQuestion pid `shouldReturn` Nothing
    heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes (\hero -> getDamage hero `shouldReturn` 0)

  it "Half-orc discards one random card instead of boosting its attack" . isengard $ \pid -> do
    emptyHand pid
    card <- putCardIntoHand pid Events.fortuneOrFate
    eid <- stage Enemies.halfOrcOfIsengard
    halfOrcAttacks eid pid
    chooseLabel "$label.halfOrcOfIsengard.discard" pid
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    getEnemyAttack eid `shouldReturn` 3

  it "Half-orc's optional attack bonus expires after the attack" . isengard $ \pid -> do
    emptyHand pid
    card <- putCardIntoHand pid Events.fortuneOrFate
    eid <- stage Enemies.halfOrcOfIsengard
    halfOrcAttacks eid pid
    chooseLabel "$label.halfOrcOfIsengard.attack" pid
    getEnemyAttack eid `shouldReturn` 6
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    run $ EndEffects UntilEndOfAttack
    getEnemyAttack eid `shouldReturn` 3

  it "Half-orc automatically boosts its attack with an empty hand" . isengard $ \pid -> do
    emptyHand pid
    eid <- stage Enemies.halfOrcOfIsengard
    halfOrcAttacks eid pid
    getEnemyAttack eid `shouldReturn` 6
    getQuestion pid `shouldReturn` Nothing

  it "Half-orc cannot choose discarding when the player cannot discard" . isengard $ \pid -> do
    emptyHand pid
    card <- putCardIntoHand pid Events.fortuneOrFate
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
    eid <- stage Enemies.halfOrcOfIsengard
    halfOrcAttacks eid pid
    getEnemyAttack eid `shouldReturn` 6
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    getQuestion pid `shouldReturn` Nothing

  it "Half-orc cannot discard an encounter card protected in the player's hand" . isengard $ \pid -> do
    card <- genEncounterCard Treacheries.theNecromancersReach
    putGame . (playersL . ix pid . handL .~ [EncounterCard card]) =<< getGame
    eid <- stage Enemies.halfOrcOfIsengard
    halfOrcAttacks eid pid
    getEnemyAttack eid `shouldReturn` 6
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]

  it "blanking Half-orc suppresses its attack ability" . isengard $ \pid -> do
    emptyHand pid
    eid <- stage Enemies.halfOrcOfIsengard
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    halfOrcAttacks eid pid
    getEnemyAttack eid `shouldReturn` 3

  it "Half-orc affects the attacked player, including attacks outside its engagement" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
    emptyHand one
    emptyHand two
    kept <- putCardIntoHand one Events.fortuneOrFate
    removed <- putCardIntoHand two Events.aTestOfWill
    eid <- stage Enemies.halfOrcOfIsengard
    run $ EngageEnemy one eid
    halfOrcAttacks eid two
    chooseLabel "$label.halfOrcOfIsengard.discard" two
    map (.id) . (.hand) <$> getPlayer one `shouldReturn` [kept.id]
    map (.id) . (.discard) <$> getPlayer two >>= (`shouldSatisfy` elem removed.id)

  it "Orthanc Archer assigns the discarded card's cost among the revealer's characters" . isengard $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    card <- putCardIntoHand pid Events.fortuneOrFate
    firstHero <- toTarget <$> getHeroWithCode "01012"
    secondHero <- toTarget <$> getHeroWithCode "01007"
    _ <- setEncounterDeck [Enemies.orthancArcher]
    run RevealEncounterCard
    resolveRevealed
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    replicateM_ 3 $ chooseTarget pid firstHero
    replicateM_ 2 $ chooseTarget pid secondHero
    getDamage firstHero `shouldReturn` 3
    getDamage secondHero `shouldReturn` 2
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    eid <- getsGame (toId . fromJustNote "archer" . headMay . toList . (.entities.enemies))
    getEngagementCost eid `shouldReturn` 38
    getArcheryTotal `shouldReturn` 2

  it "Orthanc Archer does no when-revealed damage for an empty hand" . isengard $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    _ <- setEncounterDeck [Enemies.orthancArcher]
    run RevealEncounterCard
    resolveRevealed
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes (\h -> getDamage h `shouldReturn` 0)

  it "Orthanc Archer does no damage when the player cannot discard from hand" . isengard $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    card <- putCardIntoHand pid Events.fortuneOrFate
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
    _ <- setEncounterDeck [Enemies.orthancArcher]
    run RevealEncounterCard
    resolveRevealed
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes (\h -> getDamage h `shouldReturn` 0)
    -- The original random-discard message follows the same prohibition.
    run $ DiscardRandomFromHand pid 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]

  it "Orthanc Archer does not discard an encounter card that cannot leave the player's hand" . isengard $ \pid -> do
    protected <- genEncounterCard Treacheries.theNecromancersReach
    putGame . (playersL . ix pid . handL .~ [EncounterCard protected]) =<< getGame
    _ <- putCardIntoHand pid Events.aTestOfWill
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- setEncounterDeck [Enemies.orthancArcher]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid hero
    getDamage hero `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [protected.id]

  it "Orthanc Archer resolves only its own discard result and treats X as zero" . isengard $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    run . AddToStaging =<< genEncounterCard Enemies.orthancArcher
    card <- putCardIntoHand pid Events.gandalfsSearch
    _ <- setEncounterDeck [Enemies.orthancArcher]
    run RevealEncounterCard
    resolveRevealed
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    _ <- putCardIntoHand pid Events.aTestOfWill
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- setEncounterDeck [Enemies.orthancArcher]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid hero
    getDamage hero `shouldReturn` 1
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Orthanc Archer discards and assigns damage only for the revealing player" . twoPlayerTestIn theRoadToIsengard testDeck $ \one two -> do
    putGame . (playersL . ix one . handL .~ []) . (playersL . ix two . handL .~ []) =<< getGame
    untouched <- putCardIntoHand one Events.fortuneOrFate
    discarded <- putCardIntoHand two Events.aTestOfWill
    hero <- toTarget . fromJustNote "revealer's hero" . headMay <$> select (HeroControlledBy $ PlayerWithId two)
    run $ SetRevealer (Just two)
    _ <- setEncounterDeck [Enemies.orthancArcher]
    run RevealEncounterCard
    resolveRevealed
    getQuestion one >>= (`shouldSatisfy` isNothing)
    chooseTarget two hero
    getDamage hero `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer one `shouldReturn` [untouched.id]
    map (.id) . (.discard) <$> getPlayer two >>= (`shouldSatisfy` elem discarded.id)

  it "Orthanc Archer's when-revealed effect can be canceled without discarding another card" . isengard $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    kept <- putCardIntoHand pid Events.fortuneOrFate
    cancellationCard <- putCardIntoHand pid Events.aTestOfWill
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    _ <- setEncounterDeck [Enemies.orthancArcher]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget cancellationCard.id)
    resolveRevealed
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [kept.id]
    heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes (\h -> getDamage h `shouldReturn` 0)
    getArcheryTotal `shouldReturn` 2

