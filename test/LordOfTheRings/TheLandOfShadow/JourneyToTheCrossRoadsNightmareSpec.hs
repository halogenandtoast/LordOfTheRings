module LordOfTheRings.TheLandOfShadow.JourneyToTheCrossRoadsNightmareSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound, UntilEndOfAttack))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost)
import LordOfTheRings.Matcher
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Phase (Phase (PlanningPhase))
import LordOfTheRings.Window (WindowType (PhaseEnds), mkWhen)
import LordOfTheRings.Ability (isForcedAbility)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationUnderneath))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Card (toCard)
import LordOfTheRings.Modifier (ModifierType (Blank, AttackModifier))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Objective.Types (ObjectiveAttrs (objectiveUnderneath))
import LordOfTheRings.Scenario (journeyToTheCrossRoads)
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (canPlaceUnderTheBlackGate)
import TestImport

crossRoads :: (PlayerId -> GameT ()) -> IO ()
crossRoads = gameTestIn journeyToTheCrossRoads testDeck

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

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
