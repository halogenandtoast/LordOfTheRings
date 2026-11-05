module LordOfTheRings.TreasonOfSaruman.TheUrukHaiNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank, CannotTakeDamage, CannotAttack, CannotAttackPlayer, CannotExhaust, ImmuneToCardEffects))
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Scenario (theUrukHai)
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getPursuit)
import TestImport

uruk :: (PlayerId -> GameT ()) -> IO ()
uruk = gameTestIn theUrukHai testDeck

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

attack :: PlayerId -> EnemyId -> Target -> GameT ()
attack pid eid defender = do
  run $ EnemyAttack pid eid
  passActions pid
  chooseTarget pid defender
  passActions pid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

spec :: Spec
spec = describe "The Uruk-hai Nightmare" do
  it "Mauhur has Archery 2 and Toughness 1" . uruk $ \_ -> do
    eid <- stage Enemies.mauhur
    getArcheryTotal `shouldReturn` 2
    run $ DealDamage GameSource (EnemyTarget eid) 2
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Mauhur increases pursuit after attacking even when the defender survives" . uruk $ \pid -> do
    eid <- stage Enemies.mauhur
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    attack pid eid defender
    getDamage defender `shouldReturn` 3
    getPursuit `shouldReturn` 1

  it "Mauhur increases pursuit after every attack" . uruk $ \pid -> do
    eid <- stage Enemies.mauhur
    run $ EngageEnemy pid eid
    for_ [1 :: Int, 2] \_ -> do
      defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      attack pid eid defender
    getPursuit `shouldReturn` 2

  it "Mauhur enters the victory display when defeated" . uruk $ \_ -> do
    eid <- stage Enemies.mauhur
    run $ DealDamage GameSource (EnemyTarget eid) 5
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Enemies.mauhur]

  it "Lugdush only protects other enemies while engaged" . uruk $ \pid -> do
    lugdush <- stage Enemies.lugdush
    other <- stage Enemies.forestSpider
    run $ DealDamage GameSource (EnemyTarget other) 1
    getDamage (EnemyTarget other) `shouldReturn` 1
    run $ EngageEnemy pid lugdush
    run $ DealDamage GameSource (EnemyTarget other) 1
    getDamage (EnemyTarget other) `shouldReturn` 1
    run $ DealDamage GameSource (EnemyTarget lugdush) 3
    getDamage (EnemyTarget lugdush) `shouldReturn` 1
    run $ PlaceEnemy lugdush StagingArea
    run $ DealDamage GameSource (EnemyTarget other) 1
    getDamage (EnemyTarget other) `shouldReturn` 2

  it "Lugdush protects enemies engaged with other players too" . twoPlayerTestIn theUrukHai testDeck $ \one two -> do
    lugdush <- stage Enemies.lugdush
    other <- stage Enemies.forestSpider
    run $ EngageEnemy one lugdush
    run $ EngageEnemy two other
    run $ DealDamage GameSource (EnemyTarget other) 2
    getDamage (EnemyTarget other) `shouldReturn` 0

  it "Lugdush's protection ends when he leaves play" . uruk $ \pid -> do
    lugdush <- stage Enemies.lugdush
    other <- stage Enemies.forestSpider
    run $ EngageEnemy pid lugdush
    run $ Discard GameSource (EnemyTarget lugdush)
    run $ DealDamage GameSource (EnemyTarget other) 1
    getDamage (EnemyTarget other) `shouldReturn` 1

  it "blanking Lugdush disables his protection" . uruk $ \pid -> do
    lugdush <- stage Enemies.lugdush
    other <- stage Enemies.forestSpider
    run $ EngageEnemy pid lugdush
    run $ CreateEffect $ Effect GameSource (EnemyTarget lugdush) [Blank] UntilEndOfRound
    run $ DealDamage GameSource (EnemyTarget other) 1
    getDamage (EnemyTarget other) `shouldReturn` 1
    run $ EndEffects UntilEndOfRound
    run $ DealDamage GameSource (EnemyTarget other) 1
    getDamage (EnemyTarget other) `shouldReturn` 1

  it "Lugdush's shadow protects a unique attacker only until the end of the round" . uruk $ \pid -> do
    eid <- stage Enemies.mauhur
    run . AttachShadowCard eid =<< genEncounterCard Enemies.lugdush
    run $ ResolveShadow eid pid []
    getModifiers eid >>= (`shouldSatisfy` elem CannotTakeDamage)
    run $ DealDamage GameSource (EnemyTarget eid) 2
    getDamage (EnemyTarget eid) `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    run $ DealDamage GameSource (EnemyTarget eid) 2
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Lugdush's shadow does not protect a non-unique attacker" . uruk $ \pid -> do
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.lugdush
    run $ ResolveShadow eid pid []
    getModifiers eid >>= (`shouldSatisfy` notElem CannotTakeDamage)

  it "Orc of the White Hand raises pursuit after killing its defender" . uruk $ \pid -> do
    eid <- stage Enemies.orcOfTheWhiteHand
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.snowbournScout
    attack pid eid defender
    getPursuit `shouldReturn` 1

  it "Orc of the White Hand does not raise pursuit when its defender survives" . uruk $ \pid -> do
    eid <- stage Enemies.orcOfTheWhiteHand
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    attack pid eid defender
    getDamage defender `shouldReturn` 1
    getPursuit `shouldReturn` 0

  it "Orc of the White Hand's shadow raises pursuit after the attack kills a character" . uruk $ \pid -> do
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.orcOfTheWhiteHand
    defender <- putAllyIntoPlay pid Allies.snowbournScout
    attack pid eid defender
    getPursuit `shouldReturn` 1

  it "Orc of the White Hand's shadow leaves pursuit unchanged without a kill" . uruk $ \pid -> do
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.orcOfTheWhiteHand
    defender <- toTarget <$> getHeroWithCode "01012"
    attack pid eid defender
    getPursuit `shouldReturn` 0

  it "Orc of the White Hand has Toughness 2" . uruk $ \_ -> do
    eid <- stage Enemies.orcOfTheWhiteHand
    run $ DealDamage GameSource (EnemyTarget eid) 3
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Ugluk's Command offers raising pursuit by four instead of an attack" . uruk $ \pid -> do
    eid <- stage Enemies.ugluk
    reveal [Treacheries.ugluksCommand]
    chooseLabel "$label.ugluksCommand.pursuit" pid
    getPursuit `shouldReturn` 4
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea

  it "Ugluk's Command makes Ugluk attack from staging without engaging" . uruk $ \pid -> do
    eid <- stage Enemies.ugluk
    defender <- toTarget <$> getHeroWithCode "01012"
    reveal [Treacheries.ugluksCommand, Locations.theOldFord]
    chooseLabel "$label.ugluksCommand.attack" pid
    passActions pid
    chooseTarget pid defender
    passActions pid
    getDamage defender `shouldReturn` 3
    getPursuit `shouldReturn` 0
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea
    (.shadowCards) <$> getEnemy eid `shouldReturn` []

  it "Ugluk's Command increases pursuit when Ugluk is absent" . uruk $ \pid -> do
    reveal [Treacheries.ugluksCommand]
    getPursuit `shouldReturn` 4
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Ugluk's Command cannot choose an attack that is prohibited" . uruk $ \pid -> do
    eid <- stage Enemies.ugluk
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttack] UntilEndOfRound
    reveal [Treacheries.ugluksCommand]
    getPursuit `shouldReturn` 4
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Ugluk's Command respects the attack prohibition against its revealer" . uruk $ \pid -> do
    eid <- stage Enemies.ugluk
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttackPlayer pid] UntilEndOfRound
    reveal [Treacheries.ugluksCommand]
    getPursuit `shouldReturn` 4

  for_ [(9 :: Int, 0), (10, 1), (19, 1), (20, 2), (29, 2)] \(pursuit, bonus) ->
    it ("Ugluk's Command's shadow uses the tens digit at pursuit " <> show pursuit) . uruk $ \pid -> do
      run $ ScenarioSpecific "urukHai.setPursuit" (toJSON pursuit)
      eid <- stage Enemies.forestSpider
      baseAttack <- getEnemyAttack eid
      run . AttachShadowCard eid =<< genEncounterCard Treacheries.ugluksCommand
      run $ ResolveShadow eid pid []
      getEnemyAttack eid `shouldReturn` (baseAttack + bonus)

  it "Orc-draught allows discarding an ally instead of increasing pursuit and still surges" . uruk $ \pid -> do
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    reveal [Treacheries.orcDraught, Enemies.forestSpider]
    chooseLabel "$label.orcDraught.discard" pid
    getPursuit `shouldReturn` 0
    select (AllyControlledBy $ PlayerWithId pid) `shouldReturn` []
    selectAny (EnemyWithCardCode $ toCardCode Enemies.forestSpider) `shouldReturn` True
    map toCardCode . (.discard) <$> getPlayer pid >>= (`shouldSatisfy` elem (toCardCode Allies.guardOfTheCitadel))

  it "Orc-draught allows keeping an ally by increasing pursuit" . uruk $ \pid -> do
    _ <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    reveal [Treacheries.orcDraught, Locations.theOldFord]
    chooseLabel "$label.orcDraught.pursuit" pid
    getPursuit `shouldReturn` 1
    length <$> select (AllyControlledBy $ PlayerWithId pid) `shouldReturn` 1

  it "Orc-draught increases pursuit automatically without an ally" . uruk $ \pid -> do
    reveal [Treacheries.orcDraught, Locations.theOldFord]
    getPursuit `shouldReturn` 1
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Orc-draught cannot choose discarding an immune ally" . uruk $ \pid -> do
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [ImmuneToCardEffects] UntilEndOfRound
    reveal [Treacheries.orcDraught, Locations.theOldFord]
    getPursuit `shouldReturn` 1
    length <$> select (AllyControlledBy $ PlayerWithId pid) `shouldReturn` 1
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Orc-draught makes the revealer choose which of their allies to discard" . uruk $ \pid -> do
    kept <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    lost <- putAllyIntoPlay pid Allies.snowbournScout
    reveal [Treacheries.orcDraught, Enemies.forestSpider]
    chooseLabel "$label.orcDraught.discard" pid
    chooseTarget pid lost
    map toTarget <$> select (AllyControlledBy $ PlayerWithId pid) `shouldReturn` [kept]
    getPursuit `shouldReturn` 0

  it "Orc-draught's shadow exhausts a chosen character" . uruk $ \pid -> do
    eid <- stage Enemies.forestSpider
    defender <- getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.orcDraught
    run $ ResolveShadow eid pid []
    chooseTarget pid (toTarget defender)
    selectAny (HeroWithId (toId defender) <> ExhaustedHero) `shouldReturn` True
    getPursuit `shouldReturn` 0

  it "Orc-draught's shadow excludes characters that cannot exhaust or are immune" . uruk $ \pid -> do
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes \hid -> run $ CreateEffect $ Effect GameSource (HeroTarget hid) [CannotExhaust] UntilEndOfRound
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [ImmuneToCardEffects] UntilEndOfRound
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.orcDraught
    run $ ResolveShadow eid pid []
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    length <$> select (HeroControlledBy (PlayerWithId pid) <> ReadyHero) `shouldReturn` 3
    length <$> select (AllyControlledBy (PlayerWithId pid) <> ReadyAlly) `shouldReturn` 1
