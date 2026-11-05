module LordOfTheRings.TreasonOfSaruman.TheUrukHaiNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher (getArcheryTotal)
import LordOfTheRings.Modifier (ModifierType (Blank, CannotTakeDamage))
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
