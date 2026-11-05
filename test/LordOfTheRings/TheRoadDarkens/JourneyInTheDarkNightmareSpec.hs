module LordOfTheRings.TheRoadDarkens.JourneyInTheDarkNightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Effect (EffectDuration (UntilEndOfRound, UntilEndOfPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Matcher (select, HeroMatcher (HeroControlledBy), PlayerMatcher (PlayerWithId), getMainQuestProgress)
import LordOfTheRings.Scenario (journeyInTheDark)
import LordOfTheRings.Window (WindowType (CommittedToQuest), mkAfter)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (CannotHaveAttachments))
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
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
