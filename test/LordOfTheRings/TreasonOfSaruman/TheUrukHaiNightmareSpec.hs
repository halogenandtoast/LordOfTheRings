module LordOfTheRings.TreasonOfSaruman.TheUrukHaiNightmareSpec (spec) where

import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Message.Lifted (cancelWhenRevealed, cancelShadowEffects)
import LordOfTheRings.Scenario.Types (victoryDisplayL, modeL, GameMode (NightmareMode), scenarioSetAside)
import LordOfTheRings.Window (WindowType (ShadowEffectTriggered, AdvancedToStage), mkWhen)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Helpers.Window (getActionAbilities, travelableLocations)
import LordOfTheRings.Location.Types (LocationAttrs (locationCardId))
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank, CannotTakeDamage, CannotAttack, CannotAttackPlayer, CannotExhaust, ImmuneToCardEffects, ImmuneToPlayerCardEffects, WillpowerModifier))
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Scenario (theUrukHai)
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (getPursuit)
import TestImport

uruk :: (PlayerId -> GameT ()) -> IO ()
uruk = gameTestIn theUrukHai testDeck

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) theUrukHai

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

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

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged location" . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

useLocationAction :: PlayerId -> LocationId -> GameT ()
useLocationAction pid lid = do
  abilities <- getActionAbilities pid
  let ab = fromJustNote "location action" $ find ((== LocationSource lid) . abilitySource) abilities
  run $ UseAbility pid ab []

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

resolveForcedChoices :: PlayerId -> GameT ()
resolveForcedChoices pid = getQuestion pid >>= \case
  Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> chooseAbility pid >> resolveForcedChoices pid
  _ -> pure ()

cancelCommand :: PlayerId -> GameT ()
cancelCommand pid = do
  card <- putCardIntoHand pid Events.aTestOfWill
  eowyn <- toTarget <$> getHeroWithCode "01007"
  run $ PlaceTokens GameSource eowyn Resource 1
  _ <- setEncounterDeck [Treacheries.ugluksCommand]
  run RevealEncounterCard
  chooseTarget pid (CardIdTarget card.id)
  resolveForcedChoices pid
  resolveRevealed

intoVictoryDisplay :: CardDef -> GameT ()
intoVictoryDisplay def = do
  card <- genEncounterCard def
  putGame . (scenarioL %~ overAttrs (victoryDisplayL %~ (EncounterCard card :))) =<< getGame

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

  it "Wold of Rohan charges one pursuit for each progress placed by its action" . uruk $ \pid -> do
    lid <- stageLocation Locations.woldOfRohan
    useLocationAction pid lid
    getPursuit `shouldReturn` 1
    (.progress) <$> getLocation lid `shouldReturn` 1
    useLocationAction pid lid
    getPursuit `shouldReturn` 2
    (.progress) <$> getLocation lid `shouldReturn` 2

  it "Wold of Rohan's action explores it after paying for five progress" . uruk $ \pid -> do
    lid <- stageLocation Locations.woldOfRohan
    replicateM_ 5 (useLocationAction pid lid)
    getPursuit `shouldReturn` 5
    selectAny (LocationWithId lid) `shouldReturn` False
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.woldOfRohan]

  it "Wold of Rohan's action works while it is active" . uruk $ \pid -> do
    lid <- stageLocation Locations.woldOfRohan
    run $ TravelTo lid
    useLocationAction pid lid
    getPursuit `shouldReturn` 1
    (.progress) <$> getLocation lid `shouldReturn` 1

  it "either player may use Wold of Rohan's action" . twoPlayerTestIn theUrukHai testDeck $ \one two -> do
    lid <- stageLocation Locations.woldOfRohan
    useLocationAction one lid
    useLocationAction two lid
    getPursuit `shouldReturn` 2
    (.progress) <$> getLocation lid `shouldReturn` 2

  it "blanking Wold of Rohan removes its action" . uruk $ \pid -> do
    lid <- stageLocation Locations.woldOfRohan
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    getActionAbilities pid >>= (`shouldSatisfy` all ((/= LocationSource lid) . abilitySource))
    getPursuit `shouldReturn` 0

  it "Wold of Rohan's shadow discards a non-objective attachment" . uruk $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.stewardOfGondor hero
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.woldOfRohan
    run $ ResolveShadow eid pid []
    map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` [toCardCode Attachments.stewardOfGondor]
    getPursuit `shouldReturn` 0

  it "Wold of Rohan's shadow does nothing without an attachment" . uruk $ \pid -> do
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.woldOfRohan
    run $ ResolveShadow eid pid []
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getPursuit `shouldReturn` 0

  it "Wold of Rohan's shadow leaves objective attachments in play" . uruk $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    ring <- genEncounterCard Objectives.theOneRingTheRoadDarkens
    run $ PutObjectiveIntoPlayAttached ring hero
    _ <- putAttachmentOn pid Attachments.stewardOfGondor hero
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.woldOfRohan
    run $ ResolveShadow eid pid []
    getsGame (map toCardCode . toList . (.entities.objectives)) `shouldReturn` [toCardCode Objectives.theOneRingTheRoadDarkens]
    map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` [toCardCode Attachments.stewardOfGondor]

  it "Wold of Rohan's shadow lets its player choose which attachment to discard" . uruk $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    kept <- putAttachmentOn pid Attachments.stewardOfGondor hero
    lost <- putAttachmentOn pid Attachments.unexpectedCourage hero
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.woldOfRohan
    run $ ResolveShadow eid pid []
    chooseTarget pid lost
    getsGame (map toTarget . toList . (.entities.attachments)) `shouldReturn` [kept]
    map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` [toCardCode Attachments.unexpectedCourage]

  it "The Uruk's Trail searches for a non-unique Uruk-hai before becoming active" . uruk $ \pid -> do
    lid <- stageLocation Locations.theUruksTrail
    cards <- setEncounterDeck [Enemies.mauhur, Enemies.lugdush, Enemies.forestSpider, Enemies.isengardUruk]
    travelableLocations >>= (`shouldSatisfy` elem lid)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea
    chooseTarget pid (CardIdTarget $ (fromJustNote "searched enemy" $ lastMay cards).id)
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
    selectAny (EnemyWithCardCode $ toCardCode Enemies.isengardUruk) `shouldReturn` True
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    heroes <- select (HeroControlledBy $ PlayerWithId pid)
    for_ heroes (\hid -> getDamage (HeroTarget hid) `shouldReturn` 0)
    sort . map toCardCode <$> getEncounterDeck `shouldReturn` sort (map toCardCode [Enemies.mauhur, Enemies.lugdush, Enemies.forestSpider])

  it "The Uruk's Trail can find its enemy in the encounter discard pile" . uruk $ \pid -> do
    lid <- stageLocation Locations.theUruksTrail
    card <- genEncounterCard Enemies.orcOfTheWhiteHand
    run $ AddToEncounterDiscard [card]
    _ <- setEncounterDeck [Enemies.mauhur]
    travelableLocations >>= (`shouldSatisfy` elem lid)
    run $ TravelTo lid
    chooseTarget pid (CardIdTarget card.id)
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
    selectAny (EnemyWithCardCode $ toCardCode Enemies.orcOfTheWhiteHand) `shouldReturn` True
    getEncounterDiscard `shouldReturn` []

  it "The Uruk's Trail cannot be traveled to without an eligible enemy" . uruk $ \pid -> do
    lid <- stageLocation Locations.theUruksTrail
    _ <- setEncounterDeck [Enemies.mauhur, Enemies.lugdush, Enemies.forestSpider]
    travelableLocations >>= (`shouldSatisfy` notElem lid)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "The Uruk's Trail rechecks whether its travel cost can be paid" . uruk $ \_ -> do
    lid <- stageLocation Locations.theUruksTrail
    _ <- setEncounterDeck [Enemies.mauhur]
    travelableLocations >>= (`shouldSatisfy` notElem lid)
    _ <- setEncounterDeck [Enemies.urukHaiFighter]
    travelableLocations >>= (`shouldSatisfy` elem lid)

  it "The Uruk's Trail can be explored as the active location to reduce pursuit" . uruk $ \pid -> do
    lid <- stageLocation Locations.theUruksTrail
    cards <- setEncounterDeck [Enemies.urukHaiFighter]
    run $ TravelTo lid
    chooseTarget pid (CardIdTarget $ (fromJustNote "searched enemy" $ headMay cards).id)
    run $ ScenarioSpecific "urukHai.setPursuit" (toJSON (10 :: Int))
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 6
    offersAbilityOf pid (LocationTarget lid) `shouldReturn` True
    selectAny (LocationWithId lid) `shouldReturn` False
    chooseAbilityOf pid (LocationTarget lid)
    getPursuit `shouldReturn` 7

  it "The Uruk's Trail does not gain its response while explored in staging" . uruk $ \pid -> do
    lid <- stageLocation Locations.theUruksTrail
    run $ ScenarioSpecific "urukHai.setPursuit" (toJSON (10 :: Int))
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 6
    offersAbilityOf pid (LocationTarget lid) `shouldReturn` False
    getPursuit `shouldReturn` 10

  it "The Uruk's Trail's pursuit reduction is optional" . uruk $ \pid -> do
    lid <- stageLocation Locations.theUruksTrail
    run $ PlaceLocation lid ActiveLocation
    run $ ScenarioSpecific "urukHai.setPursuit" (toJSON (10 :: Int))
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 6
    chooseLabel "$label.pass" pid
    getPursuit `shouldReturn` 10

  it "The Uruk's Trail's pursuit reduction stops at zero" . uruk $ \pid -> do
    lid <- stageLocation Locations.theUruksTrail
    run $ PlaceLocation lid ActiveLocation
    run $ ScenarioSpecific "urukHai.setPursuit" (toJSON (1 :: Int))
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 6
    chooseAbilityOf pid (LocationTarget lid)
    getPursuit `shouldReturn` 0

  it "blanking The Uruk's Trail removes its travel cost and exploration response" . uruk $ \pid -> do
    lid <- stageLocation Locations.theUruksTrail
    _ <- setEncounterDeck [Enemies.mauhur]
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    travelableLocations >>= (`shouldSatisfy` elem lid)
    run $ TravelTo lid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
    run $ ScenarioSpecific "urukHai.setPursuit" (toJSON (10 :: Int))
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 6
    offersAbilityOf pid (LocationTarget lid) `shouldReturn` False
    getPursuit `shouldReturn` 10

  it "Enchanted Land raises pursuit after a when-revealed effect is canceled" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    cancelCommand pid
    getPursuit `shouldReturn` 1

  it "Enchanted Land still penalizes cancellation as the active location" . uruk $ \pid -> do
    lid <- stageLocation Locations.enchantedLand
    run $ TravelTo lid
    cancelCommand pid
    getPursuit `shouldReturn` 1

  it "each Enchanted Land raises pursuit for a cancellation" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    _ <- stageLocation Locations.enchantedLand
    cancelCommand pid
    getPursuit `shouldReturn` 2

  it "blanking Enchanted Land disables its cancellation penalty" . uruk $ \pid -> do
    lid <- stageLocation Locations.enchantedLand
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    cancelCommand pid
    getPursuit `shouldReturn` 0

  it "Enchanted Land's penalty ends when it leaves play" . uruk $ \pid -> do
    lid <- stageLocation Locations.enchantedLand
    run $ Discard GameSource (LocationTarget lid)
    cancelCommand pid
    getPursuit `shouldReturn` 0

  it "Enchanted Land raises pursuit when Hasty Stroke cancels a shadow" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    eid <- stage Enemies.forestSpider
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource eowyn Resource 1
    card <- putCardIntoHand pid Events.hastyStroke
    run . AttachShadowCard eid =<< genEncounterCard Locations.enchantedLand
    run $ ResolveShadow eid pid []
    chooseTarget pid (CardIdTarget card.id)
    getPursuit `shouldReturn` 1
    getModifiers eid >>= (`shouldSatisfy` notElem CannotTakeDamage)
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Enchanted Land raises pursuit for A Burning Brand's shadow cancellation" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    hero <- toTarget <$> getHeroWithCode "01012"
    brand <- putAttachmentOn pid Attachments.aBurningBrand hero
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.enchantedLand
    run $ ResolveShadow eid pid [hero]
    chooseAbilityOf pid brand
    getPursuit `shouldReturn` 1
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "Enchanted Land raises pursuit for Iarion's Pendant's shadow cancellation" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    hero <- toTarget <$> getHeroWithCode "01012"
    pendant <- putAttachmentOn pid Attachments.iarionsPendant hero
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.enchantedLand
    run $ ResolveShadow eid pid []
    chooseAbilityOf pid pendant
    getPursuit `shouldReturn` 1

  it "Enchanted Land counts one cancellation when a whole card with when-revealed and surge is canceled" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    intoVictoryDisplay Treacheries.orcDraught
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.theDoorIsClosed
    _ <- setEncounterDeck [Treacheries.orcDraught, Locations.woldOfRohan]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getPursuit `shouldReturn` 1
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.woldOfRohan]

  it "Enchanted Land notices whole-card cancellation even without printed when-revealed text" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    intoVictoryDisplay Treacheries.bladeTip
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.theDoorIsClosed
    _ <- setEncounterDeck [Treacheries.bladeTip, Locations.woldOfRohan]
    threatBefore <- (.threat) <$> getPlayer pid
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getPursuit `shouldReturn` 1
    (.threat) <$> getPlayer pid `shouldReturn` threatBefore
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.woldOfRohan]

  it "Enchanted Land does not trigger when no pending effect was canceled" . uruk $ \pid -> do
    lid <- stageLocation Locations.enchantedLand
    card <- genEncounterCard Locations.enchantedLand
    eid <- stage Enemies.forestSpider
    runQueueT $ cancelWhenRevealed pid (LocationTarget lid)
    runQueueT $ cancelShadowEffects [mkWhen $ ShadowEffectTriggered card.id eid]
    void mainLoop
    getPursuit `shouldReturn` 0

  it "Enchanted Land's shadow allows increasing pursuit instead of protecting the enemy" . uruk $ \pid -> do
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.enchantedLand
    run $ ResolveShadow eid pid []
    chooseLabel "$label.enchantedLand.pursuit" pid
    getPursuit `shouldReturn` 1
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Enchanted Land's shadow protection lasts after discarding the shadow until round end" . uruk $ \pid -> do
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.enchantedLand
    run $ ResolveShadow eid pid []
    chooseLabel "$label.enchantedLand.protect" pid
    getPursuit `shouldReturn` 0
    run $ DiscardShadowCards eid
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Enchanted Land's shadow does not offer protection to an immune enemy" . uruk $ \pid -> do
    eid <- stage Enemies.forestSpider
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [ImmuneToCardEffects] UntilEndOfRound
    run . AttachShadowCard eid =<< genEncounterCard Locations.enchantedLand
    run $ ResolveShadow eid pid []
    getPursuit `shouldReturn` 1
    getQuestion pid >>= (`shouldSatisfy` isNothing)

  it "builds the Nightmare deck with exact removals and sets Mauhur aside" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map copies [Enemies.urukHaiFighter, Enemies.snaga, Locations.eastemnet, Locations.plainsOfRohan, Locations.theWold, Treacheries.unseenBarrier]
      `shouldBe` [0, 2, 0, 2, 0, 0]
    map copies [Enemies.mauhur, Enemies.lugdush, Enemies.orcOfTheWhiteHand, Locations.woldOfRohan, Locations.enchantedLand, Locations.theUruksTrail, Treacheries.ugluksCommand, Treacheries.orcDraught]
      `shouldBe` [0, 1, 4, 2, 3, 3, 3, 2]
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    sort aside `shouldBe` sort (map toCardCode [Enemies.ugluk, Objectives.toIsengard, Locations.eastemnet, Enemies.mauhur])

  it "keeps the standard encounter deck unchanged in normal mode" . uruk $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map copies [Enemies.urukHaiFighter, Enemies.snaga, Locations.eastemnet, Locations.plainsOfRohan, Locations.theWold, Treacheries.unseenBarrier]
      `shouldBe` [3, 3, 2, 4, 3, 2]
    copies Enemies.mauhur `shouldBe` 0
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    sort aside `shouldBe` sort (map toCardCode [Enemies.ugluk, Objectives.toIsengard, Locations.eastemnet])

  it "Nightmare Captives ignore player card effects while remaining affected by game effects" . nightmare $ \_ -> do
    captive <- toTarget <$> getHeroWithCode "01012"
    other <- toId <$> getHeroWithCode "01007"
    guardCard <- genEncounterCard Objectives.toIsengard
    run $ GuardCharacter captive guardCard.id
    getModifiers captive >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
    run $ CreateEffect $ Effect (HeroSource other) captive [WillpowerModifier 3] UntilEndOfRound
    getModifiers captive >>= (`shouldSatisfy` notElem (WillpowerModifier 3))
    run $ CreateEffect $ Effect GameSource captive [WillpowerModifier 2] UntilEndOfRound
    getModifiers captive >>= (`shouldSatisfy` elem (WillpowerModifier 2))

  it "Nightmare Captives lose immunity after being rescued" . nightmare $ \_ -> do
    captive <- toTarget <$> getHeroWithCode "01012"
    guardCard <- genEncounterCard Objectives.toIsengard
    run $ GuardCharacter captive guardCard.id
    getModifiers captive >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
    run $ RescueCharacter captive
    getModifiers captive >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)

  it "normal Captives do not gain Nightmare immunity" . uruk $ \_ -> do
    captive <- toTarget <$> getHeroWithCode "01012"
    guardCard <- genEncounterCard Objectives.toIsengard
    run $ GuardCharacter captive guardCard.id
    getModifiers captive >>= (`shouldSatisfy` notElem ImmuneToPlayerCardEffects)

  it "adds Mauhur before resolving stage 2A and only takes him from set aside once" . nightmare $ \pid -> do
    run $ DoStep 1 Setup
    selectAny (EnemyWithCardCode $ toCardCode Enemies.mauhur) `shouldReturn` False
    q <- getActiveQuest
    run $ CompleteQuest (toId q)
    selectAny (EnemyWithCardCode (toCardCode Enemies.mauhur) <> EnemyInStagingArea) `shouldReturn` True
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.rescueTheCaptives
    getQuestion pid >>= (`shouldSatisfy` isJust)
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    aside `shouldSatisfy` notElem (toCardCode Enemies.mauhur)
    -- No second copy is created if another advancement window is observed.
    clearQueue
    putGame . (questionL .~ mempty) =<< getGame
    run $ CheckWindows [mkWhen AdvancedToStage]
    length <$> select (EnemyWithCardCode $ toCardCode Enemies.mauhur) `shouldReturn` 1

  it "does not add Mauhur when advancing in normal mode" . uruk $ \_ -> do
    run $ DoStep 1 Setup
    q <- getActiveQuest
    run $ CompleteQuest (toId q)
    selectAny (EnemyWithCardCode $ toCardCode Enemies.mauhur) `shouldReturn` False

  it "Nightmare setup guards a Captive normally and leaves Mauhur set aside" . nightmare $ \pid -> do
    flipFirstQuest pid
    captive <- toTarget <$> getHeroWithCode "01002"
    chooseTarget pid captive
    getModifiers captive >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)
    getPursuit `shouldReturn` 10
    selectAny (EnemyWithCardCode $ toCardCode Enemies.mauhur) `shouldReturn` False
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    aside `shouldBe` [toCardCode Enemies.mauhur]

  it "a canceled Enchanted Land does not trigger its own cancellation penalty" . uruk $ \pid -> do
    intoVictoryDisplay Locations.enchantedLand
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.theDoorIsClosed
    _ <- setEncounterDeck [Locations.enchantedLand]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getPursuit `shouldReturn` 0
    selectAny (LocationWithCardCode $ toCardCode Locations.enchantedLand) `shouldReturn` False

  it "only Enchanted Lands remaining in play penalize cancellation of another copy" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    intoVictoryDisplay Locations.enchantedLand
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.theDoorIsClosed
    _ <- setEncounterDeck [Locations.enchantedLand]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getPursuit `shouldReturn` 1
    length <$> select (LocationWithCardCode $ toCardCode Locations.enchantedLand) `shouldReturn` 1

  it "whole-card cancellation removes its pending Hide test" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    intoVictoryDisplay Enemies.blackRider
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.theDoorIsClosed
    _ <- setEncounterDeck [Enemies.blackRider, Locations.woldOfRohan]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getPursuit `shouldReturn` 1
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.woldOfRohan]

  it "whole-card cancellation does not offer responses to the canceled card being revealed" . uruk $ \pid -> do
    _ <- stageLocation Locations.enchantedLand
    merry <- genPlayerCard Heroes.merrySpirit (Just pid)
    run $ PutHeroIntoPlay pid merry
    intoVictoryDisplay Enemies.orcOfTheWhiteHand
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 1
    card <- putCardIntoHand pid Events.theDoorIsClosed
    _ <- setEncounterDeck [Enemies.orcOfTheWhiteHand]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getPursuit `shouldReturn` 1
