module LordOfTheRings.TheLandOfShadow.ThePassageOfTheMarshesNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Ability (isForcedAbility)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationCardId, locationTokens))
import LordOfTheRings.Matcher
import LordOfTheRings.Message.Lifted qualified as Lifted
import LordOfTheRings.Modifier (ModifierType (Blank, CannotReduceThreat, CannotHaveAttachments, CannotTakeDamage, HitPointModifier, CancelNextDamage, DamageReducedTo, DamageRedirectedTo, CannotBeTraveledTo, CannotDiscardFromHand, CannotSpendResources, ImmuneToCardEffects))
import LordOfTheRings.Scenario (thePassageOfTheMarshes)
import LordOfTheRings.Scenario.Types (modeL, GameMode (NightmareMode))
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Window (WindowType (EnemyAttacking), mkWhen)
import TestImport

marshes :: (PlayerId -> GameT ()) -> IO ()
marshes = gameTestIn thePassageOfTheMarshes testDeck

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn (overAttrs (modeL .~ NightmareMode) thePassageOfTheMarshes) testDeck

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

activeLocation :: GameT LocationId
activeLocation = activeLocationWith Locations.stagnantPools

activeLocationWith :: CardDef -> GameT LocationId
activeLocationWith def = do
  lid <- stageLocation def
  run $ PlaceLocation lid ActiveLocation
  pure lid

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged location" . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))

emptyHand :: PlayerId -> GameT ()
emptyHand pid = putGame . (playersL . ix pid . handL .~ []) =<< getGame

putSmeagol :: PlayerId -> GameT AllyId
putSmeagol pid = do
  run . PutObjectiveAllyIntoPlayUnder pid =<< genEncounterCard Objectives.smeagolTheLandOfShadow
  fromJustNote "Sméagol" . headMay <$> select (AllyWithCardCode $ toCardCode Objectives.smeagolTheLandOfShadow)

resolveForcedChoices :: PlayerId -> GameT ()
resolveForcedChoices pid = getQuestion pid >>= \case
  Just q | any isForcedChoice (questionChoices q) ->
    chooseOptionMatching "resolve a Forced ability" pid isForcedChoice >> resolveForcedChoices pid
  _ -> pure ()
 where
  isForcedChoice = \case
    AbilityLabel _ ability _ _ -> isForcedAbility ability
    _ -> False

mireTokens :: LocationId -> GameT Int
mireTokens lid = countTokens Resource . locationTokens . toAttrs <$> getLocation lid

attack :: PlayerId -> EnemyId -> Target -> GameT ()
attack pid eid defender = do
  run $ EnemyAttack pid eid
  passActions pid
  chooseTarget pid defender
  passActions pid
  resolveShadowsInOrder pid
  passActions pid

spec :: Spec
spec = describe "The Passage of the Marshes Nightmare" do
  it "Gollum's Rage stays a shadow when it is the last encounter card and surge resets the deck" . marshes $ \pid -> do
    eid <- stage Enemies.gollumTheLandOfShadow
    locationCard <- genEncounterCard Locations.festeringMire
    run $ AddToEncounterDiscard [locationCard]
    putGame . (phaseL .~ QuestPhase) =<< getGame
    _ <- setEncounterDeck [Treacheries.gollumsRage]
    run RevealEncounterCard
    resolveRevealed
    map toCardCode . (.shadowCards) <$> getEnemy eid `shouldReturn` [toCardCode Treacheries.gollumsRage]
    length <$> select (LocationWithCardCode $ toCardCode Locations.festeringMire) `shouldReturn` 1
    map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` notElem (toCardCode Treacheries.gollumsRage))
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Treacheries.gollumsRage))
    getQuestion pid `shouldReturn` Nothing

  it "canceling Gollum's Rage's When Revealed effect prevents the shadow deal but preserves surge" . marshes $ \pid -> do
    eid <- stage Enemies.gollumTheLandOfShadow
    emptyHand pid
    card <- putCardIntoHand pid Events.aTestOfWill
    hero <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource hero Resource 1
    _ <- setEncounterDeck [Treacheries.gollumsRage, Locations.festeringMire]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    (.shadowCards) <$> getEnemy eid `shouldReturn` []
    length <$> select (LocationWithCardCode $ toCardCode Locations.festeringMire) `shouldReturn` 1

  it "two Gollum's Rage shadows each schedule one additional attack without reusing old shadows" . marshes $ \pid -> do
    eid <- stage Enemies.gollumTheLandOfShadow
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    firstHero <- toTarget <$> getHeroWithCode "01012"
    otherHero <- toTarget <$> getHeroWithCode "01007"
    replicateM_ 2 $ run . AttachShadowCard eid =<< genEncounterCard Treacheries.gollumsRage
    _ <- setEncounterDeck [Enemies.hostOfIsengard, Enemies.hostOfIsengard]
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    chooseTarget pid firstHero
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    chooseTarget pid otherHero
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    getDamage firstHero `shouldReturn` 0
    getDamage otherHero `shouldReturn` 1
    getEncounterDeck `shouldReturn` []
    getQuestion pid `shouldReturn` Nothing

  it "builds the Nightmare deck with exact replacements" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    length deck `shouldBe` 40
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map copies [Enemies.gropingDead, Enemies.deadThings, Locations.softMires, Locations.hauntedMere, Treacheries.bitterReek] `shouldBe` [0, 0, 1, 0, 0]
    map copies [Locations.festeringMire, Locations.candleLitPool, Locations.greatReedThicket, Enemies.paleFaces, Enemies.rottingThing, Treacheries.rotAndVapors, Treacheries.gollumsRage] `shouldBe` [3, 3, 2, 3, 3, 3, 2]
    map copies [Enemies.morgulWraith, Treacheries.weHatesThem, Treacheries.callOfTheRing] `shouldBe` [2, 2, 2]

  it "Nightmare exploration of a staged location raises every player's threat by its Mire value" . twoPlayerTestIn (overAttrs (modeL .~ NightmareMode) thePassageOfTheMarshes) testDeck $ \one two -> do
    lid <- stageLocation Locations.candleLitPool
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 1
    threatsBefore <- traverse (fmap (.threat) . getPlayer) [one, two]
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
    resolveForcedChoices one
    traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` map (+ 3) threatsBefore

  it "Nightmare does not raise threat for exploring the active location" . nightmare $ \pid -> do
    lid <- activeLocationWith Locations.festeringMire
    threatBefore <- (.threat) <$> getPlayer pid
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 6
    resolveForcedChoices pid
    (.threat) <$> getPlayer pid `shouldReturn` threatBefore

  it "Nightmare does not count a Mire discard as exploration" . nightmare $ \pid -> do
    _ <- setEncounterDeck []
    lid <- stageLocation Locations.candleLitPool
    threatBefore <- (.threat) <$> getPlayer pid
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 3
    resolveForcedChoices pid
    (.threat) <$> getPlayer pid `shouldReturn` threatBefore

  it "normal mode keeps staged exploration free of the Nightmare threat penalty" . marshes $ \pid -> do
    lid <- stageLocation Locations.candleLitPool
    threatBefore <- (.threat) <$> getPlayer pid
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 5
    resolveForcedChoices pid
    (.threat) <$> getPlayer pid `shouldReturn` threatBefore

  it "Rot and Vapors raises threat only for questing characters you control" . marshes $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CommitToQuest hero
    run $ CommitToQuest ally
    threatBefore <- (.threat) <$> getPlayer pid
    reveal [Treacheries.rotAndVapors]
    chooseLabel "$label.rotAndVapors.threat" pid
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 2)
    getDamage hero `shouldReturn` 0
    getDamage ally `shouldReturn` 0

  it "Rot and Vapors allows the threat option with no questing characters" . marshes $ \pid -> do
    threatBefore <- (.threat) <$> getPlayer pid
    reveal [Treacheries.rotAndVapors]
    chooseLabel "$label.rotAndVapors.threat" pid
    (.threat) <$> getPlayer pid `shouldReturn` threatBefore
    characters <- selectCharacters AnyHero AnyAlly
    for_ characters $ \character -> getDamage character `shouldReturn` 0

  it "Rot and Vapors damages ready characters and Sméagol, while respecting immunity" . marshes $ \pid -> do
    aid <- putSmeagol pid
    hero <- toTarget <$> getHeroWithCode "01012"
    protected <- toTarget <$> getHeroWithCode "01007"
    run $ CreateEffect $ Effect GameSource protected [ImmuneToCardEffects] UntilEndOfRound
    reveal [Treacheries.rotAndVapors]
    chooseLabel "$label.rotAndVapors.damage" pid
    getDamage hero `shouldReturn` 1
    getDamage protected `shouldReturn` 0
    getDamage (AllyTarget aid) `shouldReturn` 1

  it "each player chooses Rot and Vapors' effect independently" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
    firstHero <- toTarget . fromJustNote "first hero" . headMay <$> select (HeroControlledBy $ PlayerWithId one)
    run $ CommitToQuest firstHero
    threatsBefore <- traverse (fmap (.threat) . getPlayer) [one, two]
    reveal [Treacheries.rotAndVapors]
    chooseLabel "$label.rotAndVapors.threat" one
    chooseLabel "$label.rotAndVapors.damage" two
    traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` zipWith (+) threatsBefore [1, 0]
    firstHeroes <- select (HeroControlledBy $ PlayerWithId one)
    otherHeroes <- select (HeroControlledBy $ PlayerWithId two)
    for_ firstHeroes $ \hid -> getDamage (HeroTarget hid) `shouldReturn` 0
    for_ otherHeroes $ \hid -> getDamage (HeroTarget hid) `shouldReturn` 1

  it "Rot and Vapors' shadow adds attack and mire only if the attack destroys a character" . marshes $ \pid -> do
    lid <- activeLocationWith Locations.theOldFord
    eid <- stage Enemies.forestSpider
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.rotAndVapors
    attack pid eid ally
    mireTokens lid `shouldReturn` 1
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Rot and Vapors' shadow leaves mire unchanged when its defender survives" . marshes $ \pid -> do
    lid <- activeLocationWith Locations.theOldFord
    eid <- stage Enemies.forestSpider
    hero <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.rotAndVapors
    attack pid eid hero
    mireTokens lid `shouldReturn` 0
    getDamage hero `shouldReturn` 1

  it "Gollum's Rage becomes a facedown shadow card and still surges" . marshes $ \pid -> do
    eid <- stage Enemies.gollumTheLandOfShadow
    cards <- setEncounterDeck [Treacheries.gollumsRage, Locations.festeringMire]
    run RevealEncounterCard
    resolveRevealed
    map (.id) . (.shadowCards) <$> getEnemy eid `shouldReturn` [(.id) $ fromJustNote "Rage" $ headMay cards]
    getEnemyAttack eid `shouldReturn` 2
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Treacheries.gollumsRage))
    length <$> select (LocationWithCardCode $ toCardCode Locations.festeringMire) `shouldReturn` 1
    getEncounterDeck `shouldReturn` []
    getQuestion pid `shouldReturn` Nothing

  it "Gollum's Rage surges and is discarded while Sméagol is faceup" . marshes $ \pid -> do
    aid <- putSmeagol pid
    reveal [Treacheries.gollumsRage, Locations.festeringMire]
    select (AllyWithId aid) `shouldReturn` [aid]
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Treacheries.gollumsRage))
    length <$> select (LocationWithCardCode $ toCardCode Locations.festeringMire) `shouldReturn` 1

  it "Gollum's Rage grants Gollum one extra attack with a fresh shadow after the boosted attack" . marshes $ \pid -> do
    eid <- stage Enemies.gollumTheLandOfShadow
    firstHero <- toTarget <$> getHeroWithCode "01012"
    otherHero <- toTarget <$> getHeroWithCode "01007"
    reveal [Treacheries.gollumsRage, Locations.festeringMire, Enemies.hostOfIsengard]
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid firstHero
    passActions pid
    chooseTarget pid otherHero
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    getDamage firstHero `shouldReturn` 1
    getDamage otherHero `shouldReturn` 1
    getEncounterDeck `shouldReturn` []
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Treacheries.gollumsRage))

  it "Gollum's Rage has no shadow effect for another enemy" . marshes $ \pid -> do
    eid <- stage Enemies.forestSpider
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- setEncounterDeck [Enemies.hostOfIsengard]
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.gollumsRage
    attack pid eid hero
    getDamage hero `shouldReturn` 0
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.hostOfIsengard]

  it "Festering Mire makes each player discard one random card to travel" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
    for_ [one, two] emptyHand
    _ <- putCardIntoHand one Events.fortuneOrFate
    _ <- putCardIntoHand two Events.fortuneOrFate
    _ <- putCardIntoHand two Events.weAreNotIdle
    lid <- stageLocation Locations.festeringMire
    run $ TravelTo lid
    length . (.hand) <$> getPlayer one `shouldReturn` 0
    length . (.hand) <$> getPlayer two `shouldReturn` 1
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "Festering Mire bars travel if any player has no discardable card" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
    for_ [one, two] emptyHand
    kept <- putCardIntoHand one Events.fortuneOrFate
    protected <- genEncounterCard Treacheries.theNecromancersReach
    putGame . (playersL . ix two . handL .~ [EncounterCard protected]) =<< getGame
    lid <- stageLocation Locations.festeringMire
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea
    map (.id) . (.hand) <$> getPlayer one `shouldReturn` [kept.id]

  it "Festering Mire bars travel when a player cannot discard from hand" . marshes $ \pid -> do
    _ <- putCardIntoHand pid Events.fortuneOrFate
    run $ CreateEffect $ Effect GameSource (PlayerTarget pid) [CannotDiscardFromHand] UntilEndOfRound
    lid <- stageLocation Locations.festeringMire
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  it "Festering Mire discards each player's hand when discarded by Mire" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
    for_ [one, two] \pid -> do
      emptyHand pid
      void $ putCardIntoHand pid Events.fortuneOrFate
      void $ putCardIntoHand pid Events.weAreNotIdle
    lid <- stageLocation Locations.festeringMire
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 2
    resolveForcedChoices one
    for_ [one, two] (\pid -> (.hand) <$> getPlayer pid `shouldReturn` [])
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.festeringMire))
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` []

  it "Festering Mire spares protected encounter cards when discarding the hand" . marshes $ \pid -> do
    emptyHand pid
    protected <- genEncounterCard Treacheries.theNecromancersReach
    putGame . (playersL . ix pid . handL .~ [EncounterCard protected]) =<< getGame
    _ <- putCardIntoHand pid Events.fortuneOrFate
    lid <- stageLocation Locations.festeringMire
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 2
    resolveForcedChoices pid
    map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [protected.id]

  it "Candle-lit Pool's travel cost can be split between different players' heroes" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
    firstHero <- fromJustNote "first hero" . headMay <$> select (HeroControlledBy $ PlayerWithId one)
    otherHero <- fromJustNote "other hero" . headMay <$> select (HeroControlledBy $ PlayerWithId two)
    for_ [firstHero, otherHero] (\hid -> run $ PlaceTokens GameSource (HeroTarget hid) Resource 1)
    lid <- stageLocation Locations.candleLitPool
    getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea
    chooseTarget one (HeroTarget firstHero)
    chooseTarget one (HeroTarget otherHero)
    (.resources) <$> getHero firstHero `shouldReturn` 0
    (.resources) <$> getHero otherHero `shouldReturn` 0
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "Candle-lit Pool bars travel when fewer than two resources can be spent" . marshes $ \_ -> do
    hero <- toId <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource (HeroTarget hero) Resource 2
    run $ CreateEffect $ Effect GameSource (HeroTarget hero) [CannotSpendResources] UntilEndOfRound
    lid <- stageLocation Locations.candleLitPool
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run $ TravelTo lid
    (.placement) <$> getLocation lid `shouldReturn` StagingArea
    (.resources) <$> getHero hero `shouldReturn` 2

  it "Candle-lit Pool puts a searched enemy into play without revealing it" . marshes $ \pid -> do
    deck <- setEncounterDeck [Enemies.cursedDead]
    otherCard <- genEncounterCard Enemies.cursedDead
    run $ AddToEncounterDiscard [otherCard]
    lid <- stageLocation Locations.candleLitPool
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 3
    resolveForcedChoices pid
    chooseTarget pid (CardIdTarget $ (fromJustNote "deck card" $ headMay deck).id)
    enemies <- select (EnemyWithCardCode $ toCardCode Enemies.cursedDead)
    length enemies `shouldBe` 1
    (.placement) <$> getEnemy (fromJustNote "enemy" $ headMay enemies) `shouldReturn` EngagedWith pid
    map (.id) <$> getEncounterDiscard >>= (`shouldSatisfy` elem otherCard.id)

  it "Candle-lit Pool reads the remaining search cards separately for each player" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
    deck <- setEncounterDeck [Enemies.paleFaces, Enemies.rottingThing]
    lid <- stageLocation Locations.candleLitPool
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 3
    resolveForcedChoices one
    chooseTarget one (CardIdTarget $ (fromJustNote "first card" $ headMay deck).id)
    length <$> select (EnemyWithCardCode (toCardCode Enemies.paleFaces) <> EnemyEngagedWith (PlayerWithId one)) `shouldReturn` 1
    length <$> select (EnemyWithCardCode (toCardCode Enemies.rottingThing) <> EnemyEngagedWith (PlayerWithId two)) `shouldReturn` 1
    length <$> select AnyEnemy `shouldReturn` 2
    getQuestion two `shouldReturn` Nothing

  it "Candle-lit Pool has no search choice when no Undead enemy can be found" . marshes $ \pid -> do
    _ <- setEncounterDeck [Enemies.forestSpider]
    lid <- stageLocation Locations.candleLitPool
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 3
    resolveForcedChoices pid
    select AnyEnemy `shouldReturn` []
    getQuestion pid `shouldReturn` Nothing

  it "Great Reed Thicket requires and exhausts a ready Sméagol to travel" . marshes $ \pid -> do
    lid <- stageLocation Locations.greatReedThicket
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    aid <- putSmeagol pid
    getModifiers lid >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    run $ Exhaust (AllyTarget aid)
    getModifiers lid >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    run $ Ready (AllyTarget aid)
    run $ TravelTo lid
    allyExhausted . toAttrs <$> getAlly aid `shouldReturn` True
    (.placement) <$> getLocation lid `shouldReturn` ActiveLocation

  it "Great Reed Thicket flips Sméagol when discarded by Mire" . marshes $ \pid -> do
    aid <- putSmeagol pid
    lid <- stageLocation Locations.greatReedThicket
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 2
    resolveForcedChoices pid
    select (AllyWithId aid) `shouldReturn` []
    length <$> select (EnemyWithCardCode (toCardCode Enemies.gollumTheLandOfShadow) <> EnemyEngagedWith (PlayerWithId pid)) `shouldReturn` 1

  it "Great Reed Thicket makes Gollum attack if his enemy side is already faceup" . marshes $ \pid -> do
    _ <- stage Enemies.gollumTheLandOfShadow
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- setEncounterDeck [Enemies.hostOfIsengard]
    lid <- stageLocation Locations.greatReedThicket
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 2
    resolveForcedChoices pid
    passActions pid
    chooseTarget pid ally
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False
    getEncounterDeck `shouldReturn` []

  for_ [(Locations.festeringMire, 6), (Locations.candleLitPool, 5), (Locations.greatReedThicket, 3)] \(def, progress) -> do
    it ("exploring " <> show (toCardCode def) <> " awards victory without its Mire effect") . marshes $ \pid -> do
      aid <- putSmeagol pid
      emptyHand pid
      kept <- putCardIntoHand pid Events.fortuneOrFate
      _ <- setEncounterDeck [Enemies.paleFaces]
      lid <- stageLocation def
      run $ PlaceTokens GameSource (LocationTarget lid) Progress progress
      resolveForcedChoices pid
      getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode def]
      map (.id) . (.hand) <$> getPlayer pid `shouldReturn` [kept.id]
      select (AllyWithId aid) `shouldReturn` [aid]
      select AnyEnemy `shouldReturn` []

    it ("blanking " <> show (toCardCode def) <> " removes its travel cost") . marshes $ \pid -> do
      emptyHand pid
      lid <- stageLocation def
      run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
      run $ TravelTo lid
      (.placement) <$> getLocation lid `shouldReturn` ActiveLocation
      getQuestion pid `shouldReturn` Nothing

  it "Rotting Thing counts redirected attack damage against the character that actually takes it" . marshes $ \pid -> do
    lid <- activeLocationWith Locations.theOldFord
    eid <- stage Enemies.rottingThing
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    recipient <- putAllyIntoPlay pid Allies.vassalOfTheWindlord
    run $ CreateEffect $ Effect GameSource defender [DamageRedirectedTo recipient] UntilEndOfRound
    attack pid eid defender
    mireTokens lid `shouldReturn` 5
    getsGame (any ((== defender) . toTarget) . toList . (.entities.allies)) `shouldReturn` True
    getsGame (any ((== recipient) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Rotting Thing counts only excess damage actually dealt after partial prevention" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [CancelNextDamage 1] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 3

  it "Rotting Thing adds no mire when damage is reduced to exactly the defender's hit points" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [DamageReducedTo 2] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 0
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  for_ [("Pale Faces", Enemies.paleFaces), ("Rotting Thing", Enemies.rottingThing)] \(title, def) -> do
    it (title <> " prevents only its engaged player from reducing threat") . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
      for_ [one, two] (\pid -> run $ RaiseThreat pid 10)
      eid <- stage def
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem CannotReduceThreat)
      run $ EngageEnemy one eid
      threatsBefore <- traverse (fmap (.threat) . getPlayer) [one, two]
      runQueueT $ Lifted.lowerThreatBy GameSource one 3
      runQueueT $ Lifted.lowerThreatBy GameSource two 3
      void mainLoop
      traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` zipWith (-) threatsBefore [0, 3]
      run $ PlaceEnemy eid StagingArea
      getModifiers (PlayerTarget one) >>= (`shouldSatisfy` notElem CannotReduceThreat)

    it ("blanking " <> title <> " removes its threat restriction") . marshes $ \pid -> do
      eid <- stage def
      run $ EngageEnemy pid eid
      run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
      getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` notElem CannotReduceThreat)

  it "Pale Faces raises the attacked player's threat by 4 on every attack" . marshes $ \pid -> do
    eid <- stage Enemies.paleFaces
    threatBefore <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
    run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 8)

  it "Pale Faces targets the attacked player instead of its engaged player" . twoPlayerTestIn thePassageOfTheMarshes testDeck $ \one two -> do
    eid <- stage Enemies.paleFaces
    run $ EngageEnemy one eid
    threatsBefore <- traverse (fmap (.threat) . getPlayer) [one, two]
    run $ CheckWindows [mkWhen $ EnemyAttacking eid two]
    traverse (fmap (.threat) . getPlayer) [one, two] `shouldReturn` zipWith (+) threatsBefore [0, 4]

  it "blanked Pale Faces does not raise threat when attacking" . marshes $ \pid -> do
    eid <- stage Enemies.paleFaces
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    threatBefore <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen $ EnemyAttacking eid pid]
    (.threat) <$> getPlayer pid `shouldReturn` threatBefore

  it "Pale Faces' shadow uses the active location's Mire value, not its mire tokens" . marshes $ \pid -> do
    lid <- activeLocation
    run $ PlaceTokens GameSource (LocationTarget lid) Resource 3
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.paleFaces
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` 7
    run $ EndEffects UntilEndOfAttack
    getEnemyAttack eid `shouldReturn` 2

  it "Pale Faces' shadow adds no attack without an active location" . marshes $ \pid -> do
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.paleFaces
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` 2

  it "Rotting Thing places one mire token for each point of excess attack damage" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    attack pid eid ally
    mireTokens lid `shouldReturn` 4
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Rotting Thing counts a wounded defender's remaining hit points" . marshes $ \pid -> do
    lid <- activeLocationWith Locations.theOldFord
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage GameSource ally 1
    attack pid eid ally
    mireTokens lid `shouldReturn` 5

  it "Rotting Thing can trigger the active location's Mire discard and Forced effect" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ DealDamage GameSource ally 1
    threatBefore <- (.threat) <$> getPlayer pid
    attack pid eid ally
    select (LocationWithId lid) `shouldReturn` []
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Locations.stagnantPools))
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 5)

  it "Rotting Thing adds no mire for an exact lethal attack using modified hit points" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [HitPointModifier 4] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 0
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Rotting Thing adds no mire when its damage cannot destroy the defender" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource ally [CannotTakeDamage] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 0
    getsGame (any ((== ally) . toTarget) . toList . (.entities.allies)) `shouldReturn` True

  it "blanked Rotting Thing does not place mire after destroying a defender" . marshes $ \pid -> do
    lid <- activeLocation
    eid <- stage Enemies.rottingThing
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    attack pid eid ally
    mireTokens lid `shouldReturn` 0

  it "Rotting Thing cannot receive attachments until its text is blanked" . marshes $ \pid -> do
    eid <- stage Enemies.rottingThing
    run $ EngageEnemy pid eid
    getModifiers eid >>= (`shouldSatisfy` elem CannotHaveAttachments)
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 3
    card <- putCardIntoHand pid Attachments.forestSnare
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    player <- getPlayer pid
    canPlay player card `shouldReturn` False
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    getModifiers eid >>= (`shouldSatisfy` notElem CannotHaveAttachments)
    canPlay player card `shouldReturn` True
