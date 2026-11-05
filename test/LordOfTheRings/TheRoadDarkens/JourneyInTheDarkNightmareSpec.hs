module LordOfTheRings.TheRoadDarkens.JourneyInTheDarkNightmareSpec (spec) where

import LordOfTheRings.Card (PlayerCard, toCard)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getLocationThreat, getEngagementCost, getEnemyAttack, getEnemyDefense)
import LordOfTheRings.Matcher (CardMatcher (CardWithCode), select, HeroMatcher (HeroControlledBy), PlayerMatcher (PlayerWithId), getMainQuestProgress, getArcheryTotal, getEnemy)
import LordOfTheRings.Scenario (journeyInTheDark)
import LordOfTheRings.Window (WindowType (CommittedToQuest), mkAfter)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (CannotHaveAttachments, CannotDiscardFromHand, ConsideredOutOfPlay, GainTrait, GainKeyword, CannotBeDefeated))
import LordOfTheRings.Phase (Phase (PlanningPhase, RefreshPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Scenario.Types (GameMode (NightmareMode), modeL, scenarioSetAside, setAsideL, victoryDisplayL)
import LordOfTheRings.Trait (Trait (Weapon))
import LordOfTheRings.Keyword (Keyword (Regenerate))
import LordOfTheRings.Outcome (Outcome (Won))
import TestImport

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "staged location"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.locations)
    )

stageSlabBearer :: GameT EnemyId
stageSlabBearer = do
  run . AddToStaging =<< genEncounterCard Enemies.slabBearer
  getsGame
    ( toId . fromJustNote "Slab-bearer" . find ((== toCardCode Enemies.slabBearer) . toCardCode)
        . toList . (.entities.enemies)
    )

stageBalrog :: GameT EnemyId
stageBalrog = do
  run . AddToStaging =<< genEncounterCard Enemies.theBalrog
  getsGame (toId . fromJustNote "Balrog" . find ((== toCardCode Enemies.theBalrog) . toCardCode) . toList . (.entities.enemies))

weaponOf :: CardDef -> GameT (TreacheryId, Placement)
weaponOf def = getsGame
  ( (\t -> (toId t, t.placement)) . fromJustNote "Balrog weapon"
      . find ((== toCardCode def) . toCardCode) . toList . (.entities.treacheries)
  )

revealWeapon :: CardDef -> GameT ()
revealWeapon def = do
  _ <- setEncounterDeck [def]
  run RevealEncounterCard
  resolveRevealed

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) journeyInTheDark

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

discardTestHero :: PlayerId -> GameT (HeroId, PlayerCard)
discardTestHero _ = do
  hero <- getHeroWithCode "01002"
  let hid = toId hero
  run $ Destroy GameSource (HeroTarget hid)
  case toCard hero of
    PlayerCard pc -> pure (hid, pc)
    _ -> error "test hero must be a player card"

spec :: Spec
spec = describe "Journey in the Dark Nightmare" do
  it "Darkened Mine prevents progress on itself and other staged locations, but permits active-location progress" . gameTest $ \_ -> do
    mine <- stageLocation Locations.darkenedMine
    other <- stageLocation Locations.theOldFord
    run $ PlaceTokens GameSource (toTarget mine) Progress 1
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation mine `shouldReturn` 0
    (.progress) <$> getLocation other `shouldReturn` 0
    run $ PlaceLocation other ActiveLocation
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation other `shouldReturn` 1

  it "Darkened Mine stops blocking staged locations when it becomes active" . gameTest $ \_ -> do
    mine <- stageLocation Locations.darkenedMine
    other <- stageLocation Locations.theOldFord
    run $ PlaceLocation mine ActiveLocation
    run $ PlaceTokens GameSource (toTarget mine) Progress 1
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation mine `shouldReturn` 1
    (.progress) <$> getLocation other `shouldReturn` 1

  it "Darkened Mine's shadow discards one card from the defending player's hand" . gameTest $ \pid -> do
    _ <- putCardIntoHand pid Events.aTestOfWill
    _ <- putCardIntoHand pid Events.fortuneOrFate
    handBefore <- length . (.hand) <$> getPlayer pid
    run . AddToStaging =<< genEncounterCard Enemies.forestSpider
    eid <- getsGame (toId . fromJustNote "shadow enemy" . headMay . toList . (.entities.enemies))
    run . AttachShadowCard eid =<< genEncounterCard Locations.darkenedMine
    run $ ResolveShadow eid pid []
    length . (.hand) <$> getPlayer pid `shouldReturn` (handBefore - 1)

  it "Very Weary damages a quester and reduces damaged characters' willpower even when they are not questing" . gameTest $ \_ -> do
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    quester <- toTarget <$> getHeroWithCode "01012"
    other <- toTarget <$> getHeroWithCode "01007"
    base <- getWillpower quester
    otherBase <- getWillpower other
    run $ CommitToQuest quester
    run $ PlaceTokens GameSource other Damage 1
    _ <- setEncounterDeck [Treacheries.veryWeary]
    run RevealEncounterCard
    resolveRevealed
    getDamage quester `shouldReturn` 1
    getDamage other `shouldReturn` 1
    getWillpower quester `shouldReturn` max 0 (base - 2)
    getWillpower other `shouldReturn` max 0 (otherBase - 2)

  it "Very Weary tracks newly damaged and healed characters until the phase ends" . gameTest $ \_ -> do
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    hero <- toTarget <$> getHeroWithCode "01012"
    base <- getWillpower hero
    _ <- setEncounterDeck [Treacheries.veryWeary]
    run RevealEncounterCard
    resolveRevealed
    getDamage hero `shouldReturn` 0
    getWillpower hero `shouldReturn` base
    run $ PlaceTokens GameSource hero Damage 1
    getWillpower hero `shouldReturn` max 0 (base - 2)
    run $ RemoveTokens GameSource hero Damage 1
    getWillpower hero `shouldReturn` base
    run $ PlaceTokens GameSource hero Damage 1
    run $ EndPhase PlanningPhase
    getDamage hero `shouldReturn` 1
    getWillpower hero `shouldReturn` base

  it "Very Weary's shadow damages the defender without applying its when-revealed penalty" . gameTest $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    base <- getWillpower hero
    run . AddToStaging =<< genEncounterCard Enemies.forestSpider
    eid <- getsGame (toId . fromJustNote "shadow enemy" . headMay . toList . (.entities.enemies))
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.veryWeary
    run $ ResolveShadow eid pid [hero]
    getDamage hero `shouldReturn` 1
    getWillpower hero `shouldReturn` base

  it "Slab-bearer cannot have attachments and stages an enemy when destroyed without revealing it" . gameTest $ \_ -> do
    eid <- stageSlabBearer
    getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem CannotHaveAttachments)
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ CommitToQuest hero
    _ <- setEncounterDeck [Enemies.dolGuldurOrcs]
    run $ Destroy GameSource (EnemyTarget eid)
    getsGame (map toCardCode . toList . (.entities.enemies)) `shouldReturn` [toCardCode Enemies.dolGuldurOrcs]
    getDamage hero `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.slabBearer]

  it "Slab-bearer leaves a discarded location in the discard pile" . gameTest $ \_ -> do
    eid <- stageSlabBearer
    _ <- setEncounterDeck [Locations.darkenedMine]
    run $ Destroy GameSource (EnemyTarget eid)
    getsGame (length . (.entities.locations)) `shouldReturn` 0
    sort . map toCardCode <$> getEncounterDiscard
      `shouldReturn` sort [toCardCode Enemies.slabBearer, toCardCode Locations.darkenedMine]

  it "discarding Slab-bearer does not trigger its destruction effect" . gameTest $ \_ -> do
    eid <- stageSlabBearer
    _ <- setEncounterDeck [Enemies.dolGuldurOrcs]
    run $ Discard GameSource (EnemyTarget eid)
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.slabBearer]

  it "Glowing Fissure's threat counts damaged characters rather than damage tokens" . gameTest $ \_ -> do
    lid <- stageLocation Locations.glowingFissure
    firstHero <- toTarget <$> getHeroWithCode "01012"
    secondHero <- toTarget <$> getHeroWithCode "01007"
    getLocationThreat lid `shouldReturn` 1
    run $ PlaceTokens GameSource firstHero Damage 2
    getLocationThreat lid `shouldReturn` 2
    run $ PlaceTokens GameSource secondHero Damage 1
    getLocationThreat lid `shouldReturn` 3
    run $ RemoveTokens GameSource firstHero Damage 2
    getLocationThreat lid `shouldReturn` 2

  it "Glowing Fissure damages only one character when a player commits several, and can damage a non-quester" . gameTest $ \pid -> do
    _ <- stageLocation Locations.glowingFissure
    firstHero <- toTarget <$> getHeroWithCode "01012"
    secondHero <- toTarget <$> getHeroWithCode "01007"
    other <- toTarget <$> getHeroWithCode "01002"
    run $ CommitToQuest firstHero
    run $ CommitToQuest secondHero
    run $ CheckWindows [mkAfter $ CommittedToQuest firstHero, mkAfter $ CommittedToQuest secondHero]
    chooseTarget pid other
    getDamage firstHero `shouldReturn` 0
    getDamage secondHero `shouldReturn` 0
    getDamage other `shouldReturn` 1
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Glowing Fissure's forced damage still applies while it is active" . gameTest $ \pid -> do
    lid <- stageLocation Locations.glowingFissure
    run $ PlaceLocation lid ActiveLocation
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ CommitToQuest hero
    run $ CheckWindows [mkAfter $ CommittedToQuest hero]
    chooseTarget pid hero
    getDamage hero `shouldReturn` 1

  it "Glowing Fissure makes only the committing player choose his own character" . twoPlayerTestIn journeyInTheDark testDeck $ \one two -> do
    _ <- stageLocation Locations.glowingFissure
    own <- toTarget . fromJustNote "first player's hero" . headMay <$> select (HeroControlledBy $ PlayerWithId one)
    committing <- toTarget . fromJustNote "second player's hero" . headMay <$> select (HeroControlledBy $ PlayerWithId two)
    run $ CommitToQuest committing
    run $ CheckWindows [mkAfter $ CommittedToQuest committing]
    getQuestion one >>= (`shouldSatisfy` isNothing)
    chooseTarget two committing
    getDamage own `shouldReturn` 0
    getDamage committing `shouldReturn` 1

  it "Cut Off lets its revealer damage only his own characters" . twoPlayerTestIn journeyInTheDark testDeck $ \one two -> do
    run $ SetRevealer (Just two)
    own <- map toTarget <$> select (HeroControlledBy $ PlayerWithId one)
    others <- map toTarget <$> select (HeroControlledBy $ PlayerWithId two)
    _ <- setEncounterDeck [Treacheries.cutOff]
    run RevealEncounterCard
    resolveRevealed
    getQuestion one >>= (`shouldSatisfy` isNothing)
    chooseLabel "$label.cutOff.damage" two
    for_ own (\h -> getDamage h `shouldReturn` 0)
    for_ others (\h -> getDamage h `shouldReturn` 1)

  it "Cut Off blocks quest progress for the round, including later stages, but permits location progress" . gameTest $ \pid -> do
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    q <- putQuestStage Quests.theLongDarkOfMoria
    lid <- stageLocation Locations.theOldFord
    _ <- setEncounterDeck [Treacheries.cutOff]
    run RevealEncounterCard
    resolveRevealed
    chooseLabel "$label.cutOff.noProgress" pid
    run $ PlaceTokens GameSource q Progress 2
    getMainQuestProgress `shouldReturn` 0
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
    (.progress) <$> getLocation lid `shouldReturn` 1
    run $ EndEffects UntilEndOfPhase
    run $ PlaceTokens GameSource q Progress 1
    getMainQuestProgress `shouldReturn` 0
    old <- getActiveQuest
    run $ LeaveQuestStage (toId old)
    next <- putQuestStage Quests.drumsInTheDeep
    run $ PlaceTokens GameSource next Progress 1
    getMainQuestProgress `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    run $ PlaceTokens GameSource next Progress 1
    getMainQuestProgress `shouldReturn` 1

  it "Cut Off's shadow removes quest progress after the attack destroys a character, leaving locations alone" . gameTest $ \pid -> do
    q <- putQuestStage Quests.theLongDarkOfMoria
    run $ PlaceTokens GameSource q Progress 3
    lid <- stageLocation Locations.theOldFord
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 1
    run . AddToStaging =<< genEncounterCard Enemies.goblinSniper
    eid <- getsGame (toId . fromJustNote "attacker" . headMay . toList . (.entities.enemies))
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.cutOff
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getMainQuestProgress `shouldReturn` 0
    (.progress) <$> getLocation lid `shouldReturn` 1

  it "Cut Off's shadow keeps quest progress when the defender survives" . gameTest $ \pid -> do
    q <- putQuestStage Quests.theLongDarkOfMoria
    run $ PlaceTokens GameSource q Progress 3
    run . AddToStaging =<< genEncounterCard Enemies.goblinSniper
    eid <- getsGame (toId . fromJustNote "attacker" . headMay . toList . (.entities.enemies))
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.cutOff
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getMainQuestProgress `shouldReturn` 3

  it "Zirakzigil Archer assigns the discarded card's cost among the revealer's characters" . gameTest $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    card <- putCardIntoHand pid Events.fortuneOrFate
    firstHero <- toTarget <$> getHeroWithCode "01012"
    secondHero <- toTarget <$> getHeroWithCode "01007"
    _ <- setEncounterDeck [Enemies.zirakzigilArcher]
    run RevealEncounterCard
    resolveRevealed
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    replicateM_ 3 $ chooseTarget pid firstHero
    replicateM_ 2 $ chooseTarget pid secondHero
    getDamage firstHero `shouldReturn` 3
    getDamage secondHero `shouldReturn` 2
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    eid <- getsGame (toId . fromJustNote "archer" . headMay . toList . (.entities.enemies))
    getEngagementCost eid `shouldReturn` 42
    getArcheryTotal `shouldReturn` 2

  it "Zirakzigil Archer does no when-revealed damage for an empty hand" . gameTest $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    _ <- setEncounterDeck [Enemies.zirakzigilArcher]
    run RevealEncounterCard
    resolveRevealed
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes (\h -> getDamage h `shouldReturn` 0)

  it "Zirakzigil Archer does no damage when the player cannot discard from hand" . gameTest $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    card <- putCardIntoHand pid Events.fortuneOrFate
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
    _ <- setEncounterDeck [Enemies.zirakzigilArcher]
    run RevealEncounterCard
    resolveRevealed
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes (\h -> getDamage h `shouldReturn` 0)
    -- The original random-discard message follows the same prohibition.
    run $ DiscardRandomFromHand pid 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [card.id]

  it "Zirakzigil Archer does not discard an encounter card that cannot leave the player's hand" . gameTest $ \pid -> do
    protected <- genEncounterCard Treacheries.theNecromancersReach
    putGame . (playersL . ix pid . handL .~ [EncounterCard protected]) =<< getGame
    _ <- putCardIntoHand pid Events.aTestOfWill
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- setEncounterDeck [Enemies.zirakzigilArcher]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid hero
    getDamage hero `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [protected.id]

  it "Zirakzigil Archer resolves only its own discard result and treats X as zero" . gameTest $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    run . AddToStaging =<< genEncounterCard Enemies.zirakzigilArcher
    card <- putCardIntoHand pid Events.gandalfsSearch
    _ <- setEncounterDeck [Enemies.zirakzigilArcher]
    run RevealEncounterCard
    resolveRevealed
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    _ <- putCardIntoHand pid Events.aTestOfWill
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- setEncounterDeck [Enemies.zirakzigilArcher]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid hero
    getDamage hero `shouldReturn` 1
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Zirakzigil Archer discards and assigns damage only for the revealing player" . twoPlayerTestIn journeyInTheDark testDeck $ \one two -> do
    putGame . (playersL . ix one . handL .~ []) . (playersL . ix two . handL .~ []) =<< getGame
    untouched <- putCardIntoHand one Events.fortuneOrFate
    discarded <- putCardIntoHand two Events.aTestOfWill
    hero <- toTarget . fromJustNote "revealer's hero" . headMay <$> select (HeroControlledBy $ PlayerWithId two)
    run $ SetRevealer (Just two)
    _ <- setEncounterDeck [Enemies.zirakzigilArcher]
    run RevealEncounterCard
    resolveRevealed
    getQuestion one >>= (`shouldSatisfy` isNothing)
    chooseTarget two hero
    getDamage hero `shouldReturn` 1
    map (.id) . (.hand) <$> getPlayer one `shouldReturn` [untouched.id]
    map (.id) . (.discard) <$> getPlayer two >>= (`shouldSatisfy` elem discarded.id)

  it "Zirakzigil Archer's when-revealed effect can be canceled without discarding another card" . gameTest $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    kept <- putCardIntoHand pid Events.fortuneOrFate
    cancellationCard <- putCardIntoHand pid Events.aTestOfWill
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    _ <- setEncounterDeck [Enemies.zirakzigilArcher]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget cancellationCard.id)
    resolveRevealed
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [kept.id]
    heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes (\h -> getDamage h `shouldReturn` 0)
    getArcheryTotal `shouldReturn` 2

  it "Fiery Whip and Flaming Sword attach to the Balrog and grant their traits, stats, and keywords" . gameTest $ \_ -> do
    eid <- stageBalrog
    baseDefense <- getEnemyDefense eid
    baseAttack <- getEnemyAttack eid
    revealWeapon Treacheries.fieryWhip
    revealWeapon Treacheries.flamingSword
    (whip, whipPlacement) <- weaponOf Treacheries.fieryWhip
    (sword, swordPlacement) <- weaponOf Treacheries.flamingSword
    whipPlacement `shouldBe` AttachedTo (EnemyTarget eid)
    swordPlacement `shouldBe` AttachedTo (EnemyTarget eid)
    getEnemyDefense eid `shouldReturn` (baseDefense + 1)
    getEnemyAttack eid `shouldReturn` (baseAttack + 1)
    getArcheryTotal `shouldReturn` 4
    getModifiers (TreacheryTarget whip) >>= (`shouldSatisfy` elem (GainTrait Weapon))
    getModifiers (TreacheryTarget sword) >>= (`shouldSatisfy` elem (GainTrait Weapon))
    getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem (GainKeyword $ Regenerate 4))

  it "the weapons attach to the set-aside Balrog without bringing it into play, then follow it into staging" . gameTest $ \_ -> do
    balrogCard <- genEncounterCard Enemies.theBalrog
    putGame . (scenarioL %~ overAttrs (setAsideL .~ [balrogCard])) =<< getGame
    revealWeapon Treacheries.fieryWhip
    revealWeapon Treacheries.flamingSword
    for_ [Treacheries.fieryWhip, Treacheries.flamingSword] \def -> do
      (tid, placement) <- weaponOf def
      placement `shouldBe` AttachedTo (CardIdTarget balrogCard.id)
      getModifiers (TreacheryTarget tid) >>= (`shouldSatisfy` elem ConsideredOutOfPlay)
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getsGame (map (.id) . scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [balrogCard.id]
    getArcheryTotal `shouldReturn` 0
    run $ AddSetAsideToStaging (CardWithCode $ toCardCode Enemies.theBalrog)
    eid <- getsGame (toId . fromJustNote "Balrog" . headMay . toList . (.entities.enemies))
    for_ [Treacheries.fieryWhip, Treacheries.flamingSword] \def -> do
      (tid, placement) <- weaponOf def
      placement `shouldBe` AttachedTo (EnemyTarget eid)
      getModifiers (TreacheryTarget tid) >>= (`shouldSatisfy` notElem ConsideredOutOfPlay)
    getArcheryTotal `shouldReturn` 4

  it "the weapons can attach to the Balrog in the victory display without adding archery" . gameTest $ \_ -> do
    balrogCard <- genEncounterCard Enemies.theBalrog
    putGame . (scenarioL %~ overAttrs (victoryDisplayL .~ [EncounterCard balrogCard])) =<< getGame
    revealWeapon Treacheries.fieryWhip
    revealWeapon Treacheries.flamingSword
    for_ [Treacheries.fieryWhip, Treacheries.flamingSword] \def -> do
      (_, placement) <- weaponOf def
      placement `shouldBe` AttachedTo (CardIdTarget balrogCard.id)
    getArcheryTotal `shouldReturn` 0

  it "Flaming Sword regenerates four damage during refresh" . gameTest $ \_ -> do
    eid <- stageBalrog
    revealWeapon Treacheries.flamingSword
    run $ PlaceTokens GameSource (EnemyTarget eid) Damage 6
    run $ BeginPhase RefreshPhase
    getDamage (EnemyTarget eid) `shouldReturn` 2

  for_ [Treacheries.fieryWhip, Treacheries.flamingSword] \def -> do
    it ("attaches " <> show (toCardCode def) <> " to the Balrog from a shadow card") . gameTest $ \pid -> do
      eid <- stageBalrog
      baseDefense <- getEnemyDefense eid
      baseAttack <- getEnemyAttack eid
      run . AttachShadowCard eid =<< genEncounterCard def
      run $ ResolveShadow eid pid []
      (_, placement) <- weaponOf def
      placement `shouldBe` AttachedTo (EnemyTarget eid)
      length . (.shadowCards) <$> getEnemy eid `shouldReturn` 0
      if toCardCode def == toCardCode Treacheries.fieryWhip
        then do
          getEnemyDefense eid `shouldReturn` (baseDefense + 1)
          getArcheryTotal `shouldReturn` 4
        else do
          getEnemyAttack eid `shouldReturn` (baseAttack + 1)
          getModifiers (EnemyTarget eid) >>= (`shouldSatisfy` elem (GainKeyword $ Regenerate 4))

    it ("does not attach " <> show (toCardCode def) <> " to another attacking enemy") . gameTest $ \pid -> do
      run . AddToStaging =<< genEncounterCard Enemies.goblinSniper
      eid <- getsGame (toId . fromJustNote "enemy" . headMay . toList . (.entities.enemies))
      run . AttachShadowCard eid =<< genEncounterCard def
      run $ ResolveShadow eid pid []
      getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  it "the Balrog's weapons are discarded with it and stop contributing archery" . gameTest $ \_ -> do
    eid <- stageBalrog
    revealWeapon Treacheries.fieryWhip
    revealWeapon Treacheries.flamingSword
    run $ Discard GameSource (EnemyTarget eid)
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    getArcheryTotal `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard >>= (\codes -> codes `shouldSatisfy` (\xs -> all (`elem` xs) (map toCardCode [Treacheries.fieryWhip, Treacheries.flamingSword])))

  it "discards the weapons when the Balrog enters the victory display" . gameTest $ \_ -> do
    eid <- stageBalrog
    revealWeapon Treacheries.fieryWhip
    revealWeapon Treacheries.flamingSword
    run $ AddToVictoryDisplay (EnemyTarget eid)
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    getArcheryTotal `shouldReturn` 0

  it "builds the Nightmare encounter deck with the exact removals and all nineteen new cards" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map copies [Enemies.moriaArcher, Locations.minesOfMoria, Treacheries.deepFissure, Treacheries.weCannotGetOut]
      `shouldBe` [0, 0, 0, 2]
    map copies [Enemies.slabBearer, Enemies.zirakzigilArcher, Locations.glowingFissure, Locations.darkenedMine, Treacheries.fieryWhip, Treacheries.flamingSword, Treacheries.veryWeary, Treacheries.cutOff]
      `shouldBe` [2, 4, 3, 3, 1, 1, 3, 2]
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    sort aside `shouldBe` sort (map toCardCode [Enemies.theBalrog, Locations.theGreatBridge, Locations.chamberOfMazarbul, Objectives.doomDoomDoom])

  it "keeps defeated heroes in discard through removal, recovery, resurrection, and direct deck insertion" . nightmare $ \pid -> do
    (hid, hero) <- discardTestHero pid
    let moves =
          [ RemoveFromDiscard pid hero.id
          , ReturnFromDiscardToHand pid hero.id
          , PutDiscardOnBottomOfDeck pid hero.id
          , PutDiscardOnTopOfDeck pid hero.id
          , ShuffleFromDiscardIntoDeck pid hero.id
          , PutHeroIntoPlay pid hero
          , PutHeroIntoPlayAs hid pid hero
          , PutInHand hero
          , PutOnTopOfDeck pid (PlayerCard hero)
          , ShuffleCardIntoDeck pid (PlayerCard hero)
          , RemoveFromGame (CardIdTarget hero.id)
          , AddToVictoryDisplay (CardIdTarget hero.id)
          , PlayCard pid hero.id
          , PlayCardInWindow pid hero.id []
          ]
    for_ moves \move -> do
      run move
      player <- getPlayer pid
      map (.id) player.discard `shouldSatisfy` elem hero.id
      map (.id) (player.hand <> player.deck) `shouldSatisfy` notElem hero.id
      getsGame (any ((== hero.id) . heroCardId . toAttrs) . toList . (.entities.heroes)) `shouldReturn` False

  it "shuffles non-hero discards normally while retaining the hero" . nightmare $ \pid -> do
    (_, hero) <- discardTestHero pid
    ordinary <- putCardIntoHand pid Events.aTestOfWill
    run $ DiscardFromHand pid ordinary.id
    run $ ShuffleDiscardIntoDeck pid
    player <- getPlayer pid
    map (.id) player.discard `shouldBe` [hero.id]
    map (.id) player.deck `shouldSatisfy` elem ordinary.id
    map (.id) player.deck `shouldSatisfy` notElem hero.id

  it "shuffles eligible cards from the top of a mixed discard pile without skipping past its requested cards" . nightmare $ \pid -> do
    ordinary <- putCardIntoHand pid Events.aTestOfWill
    run $ DiscardFromHand pid ordinary.id
    (_, hero) <- discardTestHero pid
    run $ ShuffleTopOfDiscardIntoDeck pid 1
    map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [hero.id, ordinary.id]
    run $ ShuffleTopOfDiscardIntoDeck pid 2
    map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [hero.id]
    map (.id) . (.deck) <$> getPlayer pid >>= (`shouldSatisfy` elem ordinary.id)

  it "filters a locked hero from cards placed underneath a quest while moving the other card" . nightmare $ \pid -> do
    q <- putQuestStage Quests.theLongDarkOfMoria
    (_, hero) <- discardTestHero pid
    ordinary <- putCardIntoHand pid Events.aTestOfWill
    run $ DiscardFromHand pid ordinary.id
    run $ PlaceUnderneath q [PlayerCard hero, ordinary]
    map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [hero.id]
    quest <- getActiveQuest
    map (.id) (questUnderneath $ toAttrs quest) `shouldBe` [ordinary.id]

  it "does not offer Fortune or Fate or The Houses of Healing for locked heroes" . nightmare $ \pid -> do
    _ <- discardTestHero pid
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 20
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource beravor Resource 20
    fortune <- putCardIntoHand pid Events.fortuneOrFate
    healing <- putCardIntoHand pid Events.theHousesOfHealing
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    player <- getPlayer pid
    canPlay player fortune `shouldReturn` False
    putGame . (phaseL .~ RefreshPhase) =<< getGame
    canPlay player healing `shouldReturn` False

  it "does not offer Landroval's response or return Landroval to hand for a locked hero" . nightmare $ \pid -> do
    landroval <- putAllyIntoPlay pid Allies.landroval
    (_, hero) <- discardTestHero pid
    offersAbilityOf pid landroval `shouldReturn` False
    getsGame (any ((== toCardCode Allies.landroval) . toCardCode) . toList . (.entities.allies)) `shouldReturn` True
    map (.id) . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem hero.id)

  it "allows hero recovery and resurrection in standard Journey in the Dark" . gameTestIn journeyInTheDark testDeck $ \pid -> do
    (_, hero) <- discardTestHero pid
    run $ ReturnFromDiscardToHand pid hero.id
    map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem hero.id)
    run $ DiscardFromHand pid hero.id
    run $ RemoveFromDiscard pid hero.id
    run $ PutHeroIntoPlay pid hero
    getsGame (any ((== hero.id) . heroCardId . toAttrs) . toList . (.entities.heroes)) `shouldReturn` True

  it "blocks stage three until the Balrog is in the victory display, even if it is absent from play" . nightmare $ \_ -> do
    q <- putQuestStage Quests.theBridgeOfKhazadDum
    getModifiers q >>= (`shouldSatisfy` elem CannotBeDefeated)
    run $ PlaceTokens GameSource q Progress 15
    getsGame (.outcome) `shouldReturn` Nothing
    eid <- stageBalrog
    run $ AddToVictoryDisplay (EnemyTarget eid)
    getsGame (.outcome) `shouldReturn` Just Won

  it "keeps the revival events playable in standard mode when a hero can return" . gameTestIn journeyInTheDark testDeck $ \pid -> do
    _ <- discardTestHero pid
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 20
    beravor <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource beravor Resource 20
    fortune <- putCardIntoHand pid Events.fortuneOrFate
    healing <- putCardIntoHand pid Events.theHousesOfHealing
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    player <- getPlayer pid
    canPlay player fortune `shouldReturn` True
    putGame . (phaseL .~ RefreshPhase) =<< getGame
    canPlay player healing `shouldReturn` True

  it "keeps a second player's defeated hero in his discard pile too" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    hid <- fromJustNote "second player's hero" . headMay <$> select (HeroControlledBy $ PlayerWithId two)
    run $ Destroy GameSource (HeroTarget hid)
    card <- fromJustNote "discarded hero" . headMay . (.discard) <$> getPlayer two
    run $ ReturnFromDiscardToHand two card.id
    run $ ShuffleDiscardIntoDeck two
    map (.id) . (.discard) <$> getPlayer two `shouldReturn` [card.id]
    length <$> select (HeroControlledBy $ PlayerWithId one) `shouldReturn` 3
    length <$> select (HeroControlledBy $ PlayerWithId two) `shouldReturn` 2

  it "does not reanimate a locked hero as an enemy" . nightmare $ \pid -> do
    (_, hero) <- discardTestHero pid
    eid <- getRandom
    run $ Reanimate hero.id eid
    map (.id) . (.discard) <$> getPlayer pid `shouldReturn` [hero.id]
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
