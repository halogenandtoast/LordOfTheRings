module LordOfTheRings.TreasonOfSaruman.TheRoadToIsengardNightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank, CannotDiscardFromHand))
import LordOfTheRings.Scenario (theRoadToIsengard)
import LordOfTheRings.Window (WindowType (EnemyAttacking), mkWhen)
import TestImport

isengard :: (PlayerId -> GameT ()) -> IO ()
isengard = gameTestIn theRoadToIsengard testDeck

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

spec :: Spec
spec = describe "The Road to Isengard Nightmare" do
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

