module LordOfTheRings.TheLandOfShadow.JourneyToTheCrossRoadsNightmareSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getEngagementCost, getLocationThreat)
import LordOfTheRings.Matcher
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import LordOfTheRings.Ability (isForcedAbility)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationUnderneath))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Card (toCard)
import LordOfTheRings.Modifier (ModifierType (Blank, AttackModifier, DoesNotReadyDuringRefresh, CannotBeOptionallyEngagedBy))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveUnderneath))
import LordOfTheRings.Scenario (journeyToTheCrossRoads)
import LordOfTheRings.Scenario.Types (modeL, GameMode (NightmareMode), ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Hero.CardDefs qualified as Heroes
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (canPlaceUnderTheBlackGate)
import TestImport

crossRoads :: (PlayerId -> GameT ()) -> IO ()
crossRoads = gameTestIn journeyToTheCrossRoads testDeck

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) journeyToTheCrossRoads

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "location" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.locations))

resolveForcedChoices :: PlayerId -> GameT ()
resolveForcedChoices pid = getQuestion pid >>= \case
  Just q | any isForcedChoice (questionChoices q) ->
    chooseOptionMatching "Forced ability" pid isForcedChoice >> resolveForcedChoices pid
  _ -> pure ()
 where
  isForcedChoice = \case
    AbilityLabel _ ability _ _ -> isForcedAbility ability
    _ -> False

spec :: Spec
spec = describe "Journey to the Cross-roads Nightmare" do
  it "Southron Warrior contributes Archery 2 until blanked" . crossRoads $ \_ -> do
    eid <- stage Enemies.southronWarrior
    getArcheryTotal `shouldReturn` 2
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    getArcheryTotal `shouldReturn` 0

  it "Southron Warrior's undefended shadow adds one attack for this attack" . crossRoads $ \pid -> do
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.southronWarrior
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` 3
    run $ EndEffects UntilEndOfAttack
    getEnemyAttack eid `shouldReturn` 2

  it "Southron Rearguard starts at engagement cost 38" . crossRoads $ \_ -> do
    eid <- stage Enemies.southronRearguard
    getEngagementCost eid `shouldReturn` 38

  it "The Black Gate accepts the eligible enemy in a mixed batch but leaves the Rearguard in play" . crossRoads $ \_ -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    gate <- getsGame (fromJustNote "gate" . headMay . toList . (.entities.objectives))
    rear <- stage Enemies.southronRearguard
    other <- stage Enemies.manOfHarad
    canPlaceUnderTheBlackGate rear `shouldReturn` False
    canPlaceUnderTheBlackGate other `shouldReturn` True
    cards <- traverse (fmap toCard . getEnemy) [rear, other]
    run $ PlaceUnderneath (toTarget gate) cards
    selectAny (EnemyWithId rear) `shouldReturn` True
    selectAny (EnemyWithId other) `shouldReturn` False
    underneath <- getsGame (objectiveUnderneath . toAttrs . fromJustNote "gate" . headMay . toList . (.entities.objectives))
    map toCardCode underneath `shouldBe` [toCardCode Enemies.manOfHarad]

  it "blanking the Rearguard allows it under The Black Gate" . crossRoads $ \_ -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    eid <- stage Enemies.southronRearguard
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    canPlaceUnderTheBlackGate eid `shouldReturn` True

  it "Southron Warrior discards an ally belonging to the engaged player" . twoPlayerTestIn journeyToTheCrossRoads testDeck $ \p1 p2 -> do
    own <- putAllyIntoPlay p1 Allies.guardOfTheCitadel
    victim <- putAllyIntoPlay p2 Allies.guardOfTheCitadel
    eid <- stage Enemies.southronWarrior
    run $ EngageEnemy p2 eid
    resolveForcedChoices p1
    resolveForcedChoices p2
    for_ [p1, p2] \pid -> do
      q <- getQuestion pid
      when (maybe False (any (\case TargetLabel t _ -> t == victim; _ -> False) . questionChoices) q) $ chooseTarget pid victim
    getsGame (any ((== own) . toTarget) . toList . (.entities.allies)) `shouldReturn` True
    getsGame (any ((== victim) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Southron Warrior's shadow gives two attack for a damaged defender" . crossRoads $ \pid -> do
    defender <- toTarget <$> getHeroWithCode "01012"
    run $ DealDamage GameSource defender 1
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Enemies.southronWarrior
    run $ ResolveShadow eid pid [defender]
    getEnemyAttack eid `shouldReturn` 4

  it "the Rearguard counts progress on stage 1B, ignoring its A side" . crossRoads $ \_ -> do
    run . AddToStaging =<< genEncounterCard Locations.theCrossRoadsTheLandOfShadow
    q <- putQuestStage Quests.ambushTheMenOfHarad
    eid <- stage Enemies.southronRearguard
    run $ PlaceTokens GameSource q Progress 5
    getEngagementCost eid `shouldReturn` 38
    putGame . (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) =<< getGame
    run $ SetRevealer Nothing
    getEngagementCost eid `shouldReturn` 33
    run $ RemoveTokens GameSource q Progress 2
    getEngagementCost eid `shouldReturn` 35

  it "the Rearguard's shadow returns the attacker to staging" . crossRoads $ \pid -> do
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.southronRearguard
    run $ ResolveShadow eid pid []
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea

  it "the Rearguard cannot receive Forest Snare until blanked" . crossRoads $ \pid -> do
    eid <- stage Enemies.southronRearguard
    run $ EngageEnemy pid eid
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 3
    card <- putCardIntoHand pid Attachments.forestSnare
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    player <- getPlayer pid
    canPlay player card `shouldReturn` False
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    canPlay player card `shouldReturn` True

  it "The Black Gate's planning effect leaves the Rearguard behind" . crossRoads $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    rear <- stage Enemies.southronRearguard
    other <- stage Enemies.manOfHarad
    run $ CheckWindows [mkWhen $ PhaseEnds PlanningPhase]
    resolveForcedChoices pid
    selectAny (EnemyWithId rear) `shouldReturn` True
    selectAny (EnemyWithId other) `shouldReturn` False

  it "the Rearguard may be placed underneath The Cross-roads" . crossRoads $ \_ -> do
    card <- genEncounterCard Locations.theCrossRoadsTheLandOfShadow
    run $ AddToStaging card
    lid <- getsGame (toId . fromJustNote "Cross-roads" . headMay . toList . (.entities.locations))
    eid <- stage Enemies.southronRearguard
    enemyCard <- toCard <$> getEnemy eid
    run $ PlaceUnderneath (LocationTarget lid) [enemyCard]
    selectAny (EnemyWithId eid) `shouldReturn` False
    locationUnderneath . toAttrs <$> getLocation lid `shouldReturn` [enemyCard]


  it "Fierce Southrons cannot put the Rearguard under the Gate when it is the lowest-threat Harad" . crossRoads $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    _ <- stage Enemies.southronRearguard
    _ <- setEncounterDeck [Treacheries.fierceSouthrons, Enemies.hostOfIsengard]
    run RevealEncounterCard
    resolveRevealed
    passActions pid
    asked <- getQuestion pid
    fmap (\case QuestionLabel label _ _ _ -> label; _ -> "") asked
      `shouldSatisfy` maybe False ("$label.declareDefender" `isPrefixOf`)

  it "Arrows Thick in the Air forces the additional attack when the Rearguard cannot go under the Gate" . crossRoads $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    eid <- stage Enemies.southronRearguard
    run $ EngageEnemy pid eid
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [AttackModifier (-4)] UntilEndOfRound
    defender <- toTarget <$> getHeroWithCode "01012"
    otherDefender <- toTarget <$> getHeroWithCode "01007"
    _ <- setEncounterDeck [Enemies.hostOfIsengard]
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.arrowsThickInTheAir
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    chooseTarget pid otherDefender
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    getDamage defender `shouldReturn` 0
    getDamage otherDefender `shouldReturn` 1
    selectAny (EnemyWithId eid) `shouldReturn` True
    underneath <- getsGame (objectiveUnderneath . toAttrs . fromJustNote "gate" . headMay . toList . (.entities.objectives))
    underneath `shouldBe` []


  it "Wooded Glade blanks an attachment only while active and restores its bonus when it leaves" . crossRoads $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    att <- putAttachmentOn pid Attachments.celebriansStone hero
    getWillpower hero `shouldReturn` 4
    lid <- stageLocation Locations.woodedGlade
    getWillpower hero `shouldReturn` 4
    run $ PlaceLocation lid ActiveLocation
    getModifiers att >>= (`shouldSatisfy` elem Blank)
    getWillpower hero `shouldReturn` 2
    run $ Discard GameSource (LocationTarget lid)
    getWillpower hero `shouldReturn` 4

  it "Wooded Glade also blanks an attachment that enters after it becomes active" . crossRoads $ \pid -> do
    lid <- stageLocation Locations.woodedGlade
    run $ PlaceLocation lid ActiveLocation
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.celebriansStone hero
    getWillpower hero `shouldReturn` 2
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    getWillpower hero `shouldReturn` 4
    run $ EndEffects UntilEndOfRound
    getWillpower hero `shouldReturn` 2

  it "Wooded Glade blanks an attached encounter Condition" . crossRoads $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- setEncounterDeck [Treacheries.caughtInAWeb]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid hero
    getModifiers hero >>= (`shouldSatisfy` elem DoesNotReadyDuringRefresh)
    lid <- stageLocation Locations.woodedGlade
    run $ PlaceLocation lid ActiveLocation
    getModifiers hero >>= (`shouldSatisfy` notElem DoesNotReadyDuringRefresh)

  it "Wooded Glade preserves objective attachments" . crossRoads $ \_ -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    card <- genEncounterCard Objectives.theOneRingTheLandOfShadow
    run $ PutObjectiveIntoPlayAttached card hero
    ring <- getsGame (toTarget . fromJustNote "ring" . headMay . toList . (.entities.objectives))
    lid <- stageLocation Locations.woodedGlade
    run $ PlaceLocation lid ActiveLocation
    getModifiers ring >>= (`shouldSatisfy` notElem Blank)

  it "Wooded Glade's shadow discards an attachment controlled by the attacked player" . twoPlayerTestIn journeyToTheCrossRoads testDeck $ \p1 p2 -> do
    hero1 <- toTarget . fromJustNote "hero" . headMay <$> (traverse getHero =<< select (HeroControlledBy $ PlayerWithId p1))
    hero2 <- toTarget . fromJustNote "hero" . headMay <$> (traverse getHero =<< select (HeroControlledBy $ PlayerWithId p2))
    own <- putAttachmentOn p1 Attachments.celebriansStone hero1
    victim <- putAttachmentOn p2 Attachments.unexpectedCourage hero2
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.woodedGlade
    run $ ResolveShadow eid p2 []
    for_ [p1, p2] \pid -> do
      q <- getQuestion pid
      when (maybe False (any (\case TargetLabel t _ -> t == victim; _ -> False) . questionChoices) q) $ chooseTarget pid victim
    getAttachmentsOn hero1 `shouldReturn` [own]
    getAttachmentsOn hero2 `shouldReturn` []

  it "Dense Fern-brake protects staged enemies from player effects but allows encounter damage" . crossRoads $ \_ -> do
    lid <- stageLocation Locations.denseFernBrake
    eid <- stage Enemies.forestSpider
    hero <- toId <$> getHeroWithCode "01012"
    run $ PlaceLocation lid ActiveLocation
    run $ DealDamage (HeroSource hero) (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 0
    run $ DealDamage (LocationSource lid) (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Dense Fern-brake's immunity follows staging and ends when its text is blanked" . crossRoads $ \pid -> do
    lid <- stageLocation Locations.denseFernBrake
    run $ PlaceLocation lid ActiveLocation
    eid <- stage Enemies.forestSpider
    hero <- toId <$> getHeroWithCode "01012"
    run $ DealDamage (HeroSource hero) (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 0
    run $ EngageEnemy pid eid
    run $ DealDamage (HeroSource hero) (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1
    run $ PlaceEnemy eid StagingArea
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    run $ DealDamage (HeroSource hero) (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 2

  it "Dense Fern-brake's shadow exhausts a character controlled by the attacked player" . crossRoads $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.denseFernBrake
    run $ ResolveShadow eid pid []
    chooseTarget pid hero
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True

  it "Shadowy Thicket gains two threat per card under The Black Gate" . crossRoads $ \_ -> do
    lid <- stageLocation Locations.shadowyThicket
    getLocationThreat lid `shouldReturn` 2
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    gate <- getsGame (toTarget . fromJustNote "gate" . headMay . toList . (.entities.objectives))
    cards <- traverse genEncounterCard [Enemies.manOfHarad, Enemies.captainOfHarad]
    run $ PlaceUnderneath gate (map toCard cards)
    getLocationThreat lid `shouldReturn` 6
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    getLocationThreat lid `shouldReturn` 2

  it "Shadowy Thicket cannot be traveled to without a card under The Cross-roads" . crossRoads $ \_ -> do
    lid <- stageLocation Locations.shadowyThicket
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` StagingArea

  it "Shadowy Thicket reveals the top card under The Cross-roads as its travel cost" . crossRoads $ \_ -> do
    cr <- stageLocation Locations.theCrossRoadsTheLandOfShadow
    card <- genEncounterCard Enemies.manOfHarad
    run $ PlaceUnderneath (LocationTarget cr) [toCard card]
    lid <- stageLocation Locations.shadowyThicket
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` ActiveLocation
    locationUnderneath . toAttrs <$> getLocation cr `shouldReturn` []
    selectAny (EnemyWithCardCode $ toCardCode Enemies.manOfHarad) `shouldReturn` True

  it "blanked Shadowy Thicket needs no travel cost" . crossRoads $ \_ -> do
    lid <- stageLocation Locations.shadowyThicket
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [Blank] UntilEndOfRound
    run $ TravelTo lid
    (.placement) . toAttrs <$> getLocation lid `shouldReturn` ActiveLocation


  it "Wooded Glade's shadow leaves an objective attachment in play" . crossRoads $ \pid -> do
    hero <- toTarget <$> getHeroWithCode "01012"
    card <- genEncounterCard Objectives.theOneRingTheLandOfShadow
    run $ PutObjectiveIntoPlayAttached card hero
    eid <- stage Enemies.forestSpider
    run . AttachShadowCard eid =<< genEncounterCard Locations.woodedGlade
    run $ ResolveShadow eid pid []
    getAttachmentsOn hero >>= (\xs -> length xs `shouldBe` 1)

  it "Wooded Glade stays blank when Thror's Key blanked it before travel" . crossRoads $ \pid -> do
    lid <- stageLocation Locations.woodedGlade
    _ <- putAttachmentOn pid Attachments.throrsKey (LocationTarget lid)
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.celebriansStone hero
    run $ TravelTo lid
    getWillpower hero `shouldReturn` 4
    getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem Blank)


  it "Dense Fern-brake does not protect enemies from player combat damage" . crossRoads $ \pid -> do
    lid <- stageLocation Locations.denseFernBrake
    run $ PlaceLocation lid ActiveLocation
    eid <- stage Enemies.forestSpider
    run $ DealDamage (PlayerAttackSource pid) (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "removing Thror's Key restores Wooded Glade's attachment blanking" . crossRoads $ \pid -> do
    lid <- stageLocation Locations.woodedGlade
    key <- putAttachmentOn pid Attachments.throrsKey (LocationTarget lid)
    hero <- toTarget <$> getHeroWithCode "01012"
    _ <- putAttachmentOn pid Attachments.celebriansStone hero
    run $ TravelTo lid
    getWillpower hero `shouldReturn` 4
    run $ Discard GameSource key
    getWillpower hero `shouldReturn` 2


  it "Blind Wrath modifies only Harad enemies currently in staging, including later entrants" . crossRoads $ \pid -> do
    staged <- stage Enemies.manOfHarad
    engaged <- stage Enemies.southronRearguard
    run $ EngageEnemy pid engaged
    spider <- stage Enemies.forestSpider
    cost <- getEngagementCost staged
    threat <- getEnemyThreat staged
    otherCost <- getEngagementCost engaged
    reveal [Treacheries.blindWrath]
    getEngagementCost staged `shouldReturn` (cost - 30)
    getEnemyThreat staged `shouldReturn` (threat + 2)
    getModifiers staged >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))
    getEngagementCost engaged `shouldReturn` otherCost
    getEngagementCost spider `shouldReturn` 25
    late <- stage Enemies.southronWarrior
    getEngagementCost late `shouldReturn` 16
    run $ PlaceEnemy engaged StagingArea
    getEngagementCost engaged `shouldReturn` (otherCost - 30)
    run $ EngageEnemy pid staged
    resolveForcedChoices pid
    getEngagementCost staged `shouldReturn` cost
    run $ EndEffects UntilEndOfRound
    getEngagementCost late `shouldReturn` 46
    getModifiers late >>= (`shouldSatisfy` notElem (CannotBeOptionallyEngagedBy AnyPlayer))

  it "Blind Wrath's shadow increases attack without returning the enemy if its defender survives" . crossRoads $ \pid -> do
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    defender <- toTarget <$> getHeroWithCode "01012"
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.blindWrath
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    getDamage defender `shouldReturn` 2
    (.placement) <$> getEnemy eid `shouldReturn` EngagedWith pid

  it "Blind Wrath returns the attacker to staging after its attack destroys an ally" . crossRoads $ \pid -> do
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    defender <- putAllyIntoPlay pid Allies.vassalOfTheWindlord
    run . AttachShadowCard eid =<< genEncounterCard Treacheries.blindWrath
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid defender
    passActions pid
    resolveShadowsInOrder pid
    passActions pid
    (.placement) <$> getEnemy eid `shouldReturn` StagingArea
    getsGame (any ((== defender) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Ware! Ware! goes under The Black Gate and still surges without a duplicate in the discard pile" . crossRoads $ \_ -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    reveal [Treacheries.wareWare, Locations.woodedGlade]
    under <- getsGame (concatMap (objectiveUnderneath . toAttrs) . toList . (.entities.objectives))
    map toCardCode under `shouldBe` [toCardCode Treacheries.wareWare]
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Treacheries.wareWare))
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True

  it "Ware! Ware! puts the Cross-roads enemy into play without its When Revealed effect" . crossRoads $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    cr <- stageLocation Locations.theCrossRoadsTheLandOfShadow
    card <- genEncounterCard Enemies.cursedDead
    discarded <- genEncounterCard Enemies.cursedDead
    run $ AddToEncounterDiscard [discarded]
    run $ PlaceUnderneath (LocationTarget cr) [toCard card]
    reveal [Treacheries.wareWare, Locations.woodedGlade]
    chooseLabel "$label.wareWare.engage" pid
    resolveRevealed
    enemies <- traverse getEnemy =<< select (EnemyWithCardCode $ toCardCode Enemies.cursedDead)
    map (.placement) enemies `shouldBe` [EngagedWith pid]
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` elem (toCardCode Enemies.cursedDead))
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True
    locationUnderneath . toAttrs <$> getLocation cr `shouldReturn` []

  it "Ware! Ware! triggers Southron Warrior's engagement effect when putting it into play" . crossRoads $ \pid -> do
    cr <- stageLocation Locations.theCrossRoadsTheLandOfShadow
    card <- genEncounterCard Enemies.southronWarrior
    run $ PlaceUnderneath (LocationTarget cr) [toCard card]
    victim <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    reveal [Treacheries.wareWare, Locations.woodedGlade]
    resolveForcedChoices pid
    for_ [pid] \p -> do
      asked <- getQuestion p
      when (maybe False (any (\case TargetLabel t _ -> t == victim; _ -> False) . questionChoices) asked) $ chooseTarget p victim
    resolveRevealed
    getsGame (any ((== victim) . toTarget) . toList . (.entities.allies)) `shouldReturn` False

  it "Ware! Ware!'s shadow engages a staged enemy with the attacked player and deals it a shadow" . twoPlayerTestIn journeyToTheCrossRoads testDeck $ \_ p2 -> do
    attacker <- stage Enemies.forestSpider
    chosen <- stage Enemies.manOfHarad
    run $ EngageEnemy p2 attacker
    _ <- setEncounterDeck [Enemies.hostOfIsengard]
    run . AttachShadowCard attacker =<< genEncounterCard Treacheries.wareWare
    run $ ResolveShadow attacker p2 []
    resolveForcedChoices p2
    (.placement) <$> getEnemy chosen `shouldReturn` EngagedWith p2
    map toCardCode . (.shadowCards) <$> getEnemy chosen `shouldReturn` [toCardCode Enemies.hostOfIsengard]

  it "builds the Nightmare deck with the exact removals and all Southron Warriors set aside" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    deck <- getEncounterDeck
    length deck `shouldBe` 33
    let copies def = length $ filter ((== toCardCode def) . toCardCode) deck
    map copies [Enemies.captainOfHarad, Locations.hennethAnnun, Locations.southwardRoad, Locations.hillsOfIthilien, Treacheries.theDarkLordsSummons, Enemies.southronWarrior]
      `shouldBe` [2, 0, 0, 0, 0, 0]
    map copies [Enemies.southronRearguard, Locations.woodedGlade, Locations.denseFernBrake, Locations.shadowyThicket, Treacheries.blindWrath, Treacheries.wareWare]
      `shouldBe` [2, 3, 2, 3, 3, 2]
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    length (filter ((== toCardCode Enemies.southronWarrior) . toCardCode) aside) `shouldBe` 4

  it "puts one Southron Warrior under The Cross-roads and leaves three aside in solo Nightmare setup" . nightmare $ \pid -> do
    run Setup
    chooseRingBearer pid (toCardCode Heroes.frodoBagginsTheLandOfShadow)
    chooseLabel "$label.keepHand" pid
    chooseOptionMatching "quest setup" pid (const True)
    cr <- fromJustNote "Cross-roads" . headMay <$> select (LocationWithCardCode $ toCardCode Locations.theCrossRoadsTheLandOfShadow)
    under <- locationUnderneath . toAttrs <$> getLocation cr
    length under `shouldBe` 10
    length (filter ((== toCardCode Enemies.southronWarrior) . toCardCode) under) `shouldBe` 1
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    map toCardCode aside `shouldBe` replicate 3 (toCardCode Enemies.southronWarrior)

  it "puts two Warriors under The Cross-roads and leaves two aside in two-player Nightmare setup" . twoPlayerTestIn nightmareScenario testDeck $ \p1 p2 -> do
    run Setup
    lead <- getLead
    let other = if lead == p1 then p2 else p1
    chooseRingBearer lead (toCardCode Heroes.frodoBagginsTheLandOfShadow)
    replicateM_ 2 $ for_ [p1, p2] \pid -> do
      asked <- getQuestion pid
      when (maybe False (\case QuestionLabel label _ _ _ -> label == "$label.keepStartingHand"; _ -> False) asked) $
        chooseLabel "$label.keepHand" pid
    chooseOptionMatching "quest setup" lead (const True)
    chooseOptionMatching "second player's location" other (const True)
    cr <- fromJustNote "Cross-roads" . headMay <$> select (LocationWithCardCode $ toCardCode Locations.theCrossRoadsTheLandOfShadow)
    under <- locationUnderneath . toAttrs <$> getLocation cr
    length under `shouldBe` 11
    length (filter ((== toCardCode Enemies.southronWarrior) . toCardCode) under) `shouldBe` 2
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    map toCardCode aside `shouldBe` replicate 2 (toCardCode Enemies.southronWarrior)

  it "Ware! Ware! remains under the Gate when its surge resets an empty encounter deck" . crossRoads $ \_ -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    card <- genEncounterCard Locations.woodedGlade
    run $ AddToEncounterDiscard [card]
    putGame . (phaseL .~ QuestPhase) =<< getGame
    reveal [Treacheries.wareWare]
    under <- getsGame (concatMap (objectiveUnderneath . toAttrs) . toList . (.entities.objectives))
    map toCardCode under `shouldBe` [toCardCode Treacheries.wareWare]
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True
    map toCardCode <$> getEncounterDeck >>= (`shouldSatisfy` notElem (toCardCode Treacheries.wareWare))

  it "canceling Ware! Ware! prevents either option while preserving surge" . crossRoads $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    card <- putCardIntoHand pid Events.aTestOfWill
    hero <- toTarget <$> getHeroWithCode "01007"
    run $ PlaceTokens GameSource hero Resource 1
    _ <- setEncounterDeck [Treacheries.wareWare, Locations.woodedGlade]
    run RevealEncounterCard
    chooseTarget pid (CardIdTarget card.id)
    resolveRevealed
    under <- getsGame (concatMap (objectiveUnderneath . toAttrs) . toList . (.entities.objectives))
    under `shouldBe` []
    selectAny (LocationWithCardCode $ toCardCode Locations.woodedGlade) `shouldReturn` True

  it "Ware! Ware! counts towards The Black Gate's loss threshold" . crossRoads $ \_ -> do
    run . AddToStaging =<< genEncounterCard Objectives.theBlackGate
    gate <- getsGame (toTarget . fromJustNote "gate" . headMay . toList . (.entities.objectives))
    cards <- replicateM 3 $ genEncounterCard Enemies.manOfHarad
    run $ PlaceUnderneath gate (map toCard cards)
    reveal [Treacheries.wareWare, Locations.woodedGlade]
    getsGame (.outcome) `shouldReturn` Just Lost
