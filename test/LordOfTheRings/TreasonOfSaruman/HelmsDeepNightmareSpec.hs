module LordOfTheRings.TreasonOfSaruman.HelmsDeepNightmareSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEngagementCost)
import LordOfTheRings.Location.Types (LocationAttrs (locationCardId))
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Entities (questsL)
import LordOfTheRings.Effect (Effect (..), EffectDuration (UntilEndOfRound))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Modifier (ModifierType (Blank, AdditionalAttackAfterNextAttack, CannotAttack, ImmuneToCardEffects, CannotBeTraveledTo, SkipPhase))
import LordOfTheRings.Phase (Phase (PlanningPhase, CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (helmsDeep)
import LordOfTheRings.Window (WindowType (RoundEnds, EnemyAttacking), mkWhen, mkAfter)
import TestImport

helms :: (PlayerId -> GameT ()) -> IO ()
helms = gameTestIn helmsDeep testDeck

stage :: CardDef -> GameT EnemyId
stage def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (toId . fromJustNote "staged enemy" . find ((== card.id) . (.cardId) . toAttrs) . toList . (.entities.enemies))

resolveForcedChoices :: PlayerId -> GameT ()
resolveForcedChoices pid = getQuestion pid >>= \case
  Just q | any (\case AbilityLabel {} -> True; _ -> False) (questionChoices q) -> chooseAbility pid >> resolveForcedChoices pid
  _ -> pure ()

activeLocation :: CardDef -> GameT LocationId
activeLocation def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  lid <- getsGame (toId . fromJustNote "active location" . find ((== card.id) . locationCardId . toAttrs) . toList . (.entities.locations))
  run $ PlaceLocation lid ActiveLocation
  pure lid

blankShadow :: PlayerId -> EnemyId -> GameT ()
blankShadow pid eid = do
  run . AttachShadowCard eid =<< genEncounterCard Enemies.hostOfIsengard
  run $ ResolveShadow eid pid []

spec :: Spec
spec = describe "Helm's Deep Nightmare" do
  it "Glittering Caves permits travel at stage 4B and skips combat while active" . helms $ \_ -> do
    _ <- putQuestStage Quests.theWallIsBreached
    lid <- activeLocation Locations.glitteringCaves
    getModifiers (LocationTarget lid) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    putGame . (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) =<< getGame
    run $ SetRevealer Nothing
    getModifiers (LocationTarget lid) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    getModifiers GameTarget >>= (`shouldSatisfy` elem (SkipPhase CombatPhase))
    run $ PlaceLocation lid StagingArea
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (SkipPhase CombatPhase))

  it "Helm's Dike lowers enemy engagement cost only while active" . helms $ \_ -> do
    eid <- stage Enemies.hostOfIsengard
    lid <- activeLocation Locations.helmsDike
    getEngagementCost eid `shouldReturn` 38
    run $ PlaceLocation lid StagingArea
    getEngagementCost eid `shouldReturn` 48

  it "Helm's Dike shuffles the discard before searching and stages the enemy" . helms $ \pid -> do
    _ <- setEncounterDeck []
    enemyCard <- genEncounterCard Enemies.isengardUruk
    run $ AddToEncounterDiscard [enemyCard]
    lid <- activeLocation Locations.helmsDike
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 2
    resolveForcedChoices pid
    select (EnemyWithCardCode $ toCardCode Enemies.isengardUruk) >>= (`shouldSatisfy` (not . null))
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.helmsDike]
    getQuestion pid `shouldReturn` Nothing

  it "Hornburg Wall grants stacking Toughness to staged and engaged enemies" . helms $ \pid -> do
    _ <- activeLocation Locations.hornburgWall
    wallCard <- genEncounterCard Locations.hornburgWall
    run $ AddToStaging wallCard
    host <- stage Enemies.hostOfIsengard
    berserker <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid berserker
    run $ DealDamage GameSource (EnemyTarget host) 5
    run $ DealDamage GameSource (EnemyTarget berserker) 3
    getDamage (EnemyTarget host) `shouldReturn` 1
    getDamage (EnemyTarget berserker) `shouldReturn` 1

  it "exploring Hornburg Wall exhausts allies" . helms $ \pid -> do
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    lid <- activeLocation Locations.hornburgWall
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 3
    case ally of
      AllyTarget aid -> allyExhausted . toAttrs <$> getAlly aid `shouldReturn` True
      _ -> error "expected ally"

  it "Glittering Caves leaves at round end and raises threat" . helms $ \pid -> do
    _ <- activeLocation Locations.glitteringCaves
    threatBefore <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen RoundEnds]
    resolveForcedChoices pid
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 2)
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.glitteringCaves]

  it "exploring Glittering Caves awards victory without raising threat" . helms $ \pid -> do
    lid <- activeLocation Locations.glitteringCaves
    threatBefore <- (.threat) <$> getPlayer pid
    run $ PlaceTokens GameSource (LocationTarget lid) Progress 2
    (.threat) <$> getPlayer pid `shouldReturn` threatBefore
    getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Locations.glitteringCaves]

  it "Host of Isengard has Toughness 2" . helms $ \_ -> do
    eid <- stage Enemies.hostOfIsengard
    run $ DealDamage GameSource (EnemyTarget eid) 3
    getDamage (EnemyTarget eid) `shouldReturn` 1

  it "Host of Isengard's round-end progress bypasses the active location" . helms $ \_ -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    card <- genEncounterCard Locations.deepingWall
    run $ AddToStaging card
    lid <- getsGame (toId . fromJustNote "Deeping Wall" . find ((== toCardCode Locations.deepingWall) . toCardCode) . toList . (.entities.locations))
    run $ PlaceLocation lid ActiveLocation
    _ <- stage Enemies.hostOfIsengard
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 1
    (.progress) <$> getLocation lid `shouldReturn` 0

  it "Host of Isengard places progress at the end of each round" . helms $ \_ -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    _ <- stage Enemies.hostOfIsengard
    run $ CheckWindows [mkWhen RoundEnds]
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "Host of Isengard does not place progress while engaged" . helms $ \pid -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    eid <- stage Enemies.hostOfIsengard
    run $ EngageEnemy pid eid
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 0
    run $ PlaceEnemy eid StagingArea
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 1

  it "each staged Host of Isengard places progress" . helms $ \pid -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    _ <- stage Enemies.hostOfIsengard
    _ <- stage Enemies.hostOfIsengard
    run $ CheckWindows [mkWhen RoundEnds]
    resolveForcedChoices pid
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "blanking Host of Isengard removes its round-end ability" . helms $ \_ -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    eid <- stage Enemies.hostOfIsengard
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 0
    run $ EndEffects UntilEndOfRound
    run $ CheckWindows [mkWhen RoundEnds]
    (.progress) <$> getActiveQuest `shouldReturn` 1

  it "Host of Isengard cannot receive attachments" . helms $ \pid -> do
    eid <- stage Enemies.hostOfIsengard
    run $ EngageEnemy pid eid
    hero <- toTarget <$> getHeroWithCode "01012"
    run $ PlaceTokens GameSource hero Resource 3
    card <- putCardIntoHand pid Attachments.forestSnare
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    player <- getPlayer pid
    canPlay player card `shouldReturn` False
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    canPlay player card `shouldReturn` True

  it "Host of Isengard adds its progress to stage 2B's ordinary round-end progress" . helms $ \pid -> do
    _ <- putQuestStage Quests.assaultOnHelmsDeep
    putGame . (entitiesL . questsL . traverse %~ overAttrs (\a -> a {questFlipped = True})) =<< getGame
    _ <- stage Enemies.hostOfIsengard
    run $ CheckWindows [mkWhen RoundEnds]
    resolveForcedChoices pid
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "Bloodied Berserker checks a blank shadow as it turns faceup and can place progress" . helms $ \pid -> do
    lid <- activeLocation Locations.deepingWall
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.hostOfIsengard
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    (.progress) <$> getLocation lid `shouldReturn` 0
    run $ ResolveShadow eid pid []
    chooseLabel "$label.bloodiedBerserker.progress" pid
    (.progress) <$> getLocation lid `shouldReturn` 2
    getModifiers eid >>= (`shouldSatisfy` notElem (AdditionalAttackAfterNextAttack pid))

  it "Bloodied Berserker cannot choose progress without an active location" . helms $ \pid -> do
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    blankShadow pid eid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getModifiers eid >>= (`shouldSatisfy` elem (AdditionalAttackAfterNextAttack pid))
    _ <- setEncounterDeck [Enemies.bloodiedBerserker]
    run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    passActions pid
    getQuestion pid >>= (`shouldSatisfy` \case Just (QuestionLabel label _ _ _) -> "$label.declareDefender" `isPrefixOf` label; _ -> False)

  it "Bloodied Berserker's additional attack waits for the current attack to finish" . helms $ \pid -> do
    _ <- activeLocation Locations.deepingWall
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    hero <- toTarget <$> getHeroWithCode "01012"
    ally <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run . AttachShadowCard eid =<< genEncounterCard Enemies.hostOfIsengard
    _ <- setEncounterDeck [Enemies.bloodiedBerserker]
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid hero
    passActions pid
    getDamage hero `shouldReturn` 0
    chooseLabel "$label.bloodiedBerserker.attack" pid
    passActions pid
    getDamage hero `shouldReturn` 1
    chooseTarget pid ally
    passActions pid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getEncounterDeck `shouldReturn` []
    sort . map toCardCode <$> getEncounterDiscard `shouldReturn` sort (map toCardCode [Enemies.hostOfIsengard, Enemies.bloodiedBerserker])
    getModifiers eid >>= (`shouldSatisfy` notElem (AdditionalAttackAfterNextAttack pid))

  it "Bloodied Berserker ignores a dealt shadow that is discarded before turning faceup" . helms $ \pid -> do
    lid <- activeLocation Locations.deepingWall
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.hostOfIsengard
    run $ DiscardShadowCards eid
    run $ ResolveShadow eid pid []
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    (.progress) <$> getLocation lid `shouldReturn` 0
    getModifiers eid >>= (`shouldSatisfy` notElem (AdditionalAttackAfterNextAttack pid))

  it "Bloodied Berserker does not trigger for a shadow with printed text, and its own shadow gives +1 attack" . helms $ \pid -> do
    lid <- activeLocation Locations.deepingWall
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    run . AttachShadowCard eid =<< genEncounterCard Enemies.bloodiedBerserker
    run $ ResolveShadow eid pid []
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    (.progress) <$> getLocation lid `shouldReturn` 0
    getEnemyAttack eid `shouldReturn` 5

  it "blanking Bloodied Berserker disables its blank-shadow ability" . helms $ \pid -> do
    lid <- activeLocation Locations.deepingWall
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [Blank] UntilEndOfRound
    blankShadow pid eid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    (.progress) <$> getLocation lid `shouldReturn` 0
    getModifiers eid >>= (`shouldSatisfy` notElem (AdditionalAttackAfterNextAttack pid))

  it "Bloodied Berserker does not trigger for another enemy's blank shadow" . helms $ \pid -> do
    lid <- activeLocation Locations.deepingWall
    _ <- stage Enemies.bloodiedBerserker
    eid <- stage Enemies.forestSpider
    run $ EngageEnemy pid eid
    blankShadow pid eid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    (.progress) <$> getLocation lid `shouldReturn` 0

  it "Bloodied Berserker triggers once for each blank shadow and can explore the active location" . helms $ \pid -> do
    lid <- activeLocation Locations.deepingWall
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    replicateM_ 2 $ run . AttachShadowCard eid =<< genEncounterCard Enemies.hostOfIsengard
    threatBefore <- (.threat) <$> getPlayer pid
    run $ ResolveShadow eid pid []
    resolveShadowsInOrder pid
    chooseLabel "$label.bloodiedBerserker.progress" pid
    (.progress) <$> getLocation lid `shouldReturn` 2
    resolveShadowsInOrder pid
    chooseLabel "$label.bloodiedBerserker.progress" pid
    selectAny (LocationWithId lid) `shouldReturn` False
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 4)

  it "Bloodied Berserker cannot place progress on an immune active location" . helms $ \pid -> do
    lid <- activeLocation Locations.deepingWall
    run $ CreateEffect $ Effect GameSource (LocationTarget lid) [ImmuneToCardEffects] UntilEndOfRound
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    blankShadow pid eid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getModifiers eid >>= (`shouldSatisfy` elem (AdditionalAttackAfterNextAttack pid))
    (.progress) <$> getLocation lid `shouldReturn` 0

  it "Bloodied Berserker chooses progress when it cannot attack again" . helms $ \pid -> do
    lid <- activeLocation Locations.deepingWall
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttack] UntilEndOfRound
    blankShadow pid eid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    (.progress) <$> getLocation lid `shouldReturn` 2
    getModifiers eid >>= (`shouldSatisfy` notElem (AdditionalAttackAfterNextAttack pid))

  it "Bloodied Berserker does not treat a gained shadow effect as blank" . helms $ \pid -> do
    _ <- activeLocation Locations.mountainsOfAngmar
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    threatBefore <- (.threat) <$> getPlayer pid
    blankShadow pid eid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    (.threat) <$> getPlayer pid `shouldReturn` (threatBefore + 2)
    getModifiers eid >>= (`shouldSatisfy` notElem (AdditionalAttackAfterNextAttack pid))

  it "Bloodied Berserker cannot make its scheduled attack after leaving play" . helms $ \pid -> do
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    blankShadow pid eid
    run $ Discard GameSource (EnemyTarget eid)
    deck <- setEncounterDeck [Enemies.bloodiedBerserker]
    run $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    passActions pid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getEncounterDeck `shouldReturn` deck

  it "Bloodied Berserker cannot make its scheduled attack if later prevented from attacking" . helms $ \pid -> do
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    blankShadow pid eid
    deck <- setEncounterDeck [Enemies.bloodiedBerserker]
    runOnly $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    run $ CreateEffect $ Effect GameSource (EnemyTarget eid) [CannotAttack] UntilEndOfRound
    passActions pid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getEncounterDeck `shouldReturn` deck

  it "Bloodied Berserker's extra attack targets the attacked player rather than its engaged player" . twoPlayerTestIn helmsDeep testDeck $ \one two -> do
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy one eid
    blankShadow two eid
    getModifiers eid >>= (`shouldSatisfy` elem (AdditionalAttackAfterNextAttack two))
    getModifiers eid >>= (`shouldSatisfy` notElem (AdditionalAttackAfterNextAttack one))

  it "Bloodied Berserker rechecks its presence after the attack-completion window" . helms $ \pid -> do
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy pid eid
    blankShadow pid eid
    deck <- setEncounterDeck [Enemies.bloodiedBerserker]
    runOnly $ CheckWindows [mkAfter $ EnemyAttacking eid pid]
    run $ Discard GameSource (EnemyTarget eid)
    passActions pid
    getQuestion pid >>= (`shouldSatisfy` isNothing)
    getEncounterDeck `shouldReturn` deck

  it "Bloodied Berserker cannot make its extra attack against an eliminated player" . twoPlayerTestIn helmsDeep testDeck $ \one two -> do
    eid <- stage Enemies.bloodiedBerserker
    run $ EngageEnemy one eid
    blankShadow one eid
    run $ EliminatePlayer one
    deck <- setEncounterDeck [Enemies.bloodiedBerserker]
    run $ CheckWindows [mkAfter $ EnemyAttacking eid one]
    passActions two
    getQuestion one >>= (`shouldSatisfy` isNothing)
    getQuestion two >>= (`shouldSatisfy` isNothing)
    getEncounterDeck `shouldReturn` deck
