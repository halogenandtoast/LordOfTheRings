module LordOfTheRings.Haradrim.TheMumakilSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Enemy.Types (tokensL)
import LordOfTheRings.Entities (enemiesL)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat, getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theMumakil)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..), setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkAfter, mkWhen)
import TestImport

mumakilTest :: (PlayerId -> GameT ()) -> IO ()
mumakilTest = gameTestIn theMumakil testDeck

-- | Setup done: the player picks the location he adds to the staging area.
setup :: PlayerId -> GameT ()
setup pid = do
  flipFirstQuest pid
  chooseOptionMatching "add a location" pid (const True)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . lastMay . filter ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stage :: CardDef -> GameT EnemyId
stage def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  eid <- stage def
  run $ EngageEnemy pid eid
  pure eid

placementOfEnemy :: EnemyId -> GameT (Maybe Placement)
placementOfEnemy eid = getsGame (fmap (.placement) . lookup eid . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

activate :: CardDef -> GameT LocationId
activate def = do
  run . AddToStaging =<< genEncounterCard def
  lid <- locationOf def
  run $ PlaceLocation lid ActiveLocation
  pure lid

-- | The Capture objective put into play attached to the enemy.
attachCapture :: CardDef -> EnemyId -> GameT ObjectiveId
attachCapture def eid = do
  card <- genEncounterCard def
  run $ PutObjectiveIntoPlayAttached card (toTarget eid)
  getsGame (fromJustNote "no objective" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

-- | Damage put straight on the enemy, past what it may take in a round.
wound :: EnemyId -> Int -> GameT ()
wound eid n = putGame . (entitiesL . enemiesL . ix eid %~ overAttrs (tokensL %~ addTokens Damage n)) =<< getGame

setAsideCards :: [CardDef] -> GameT ()
setAsideCards defs = do
  cards <- traverse genEncounterCard defs
  putGame . (scenarioL %~ overAttrs (setAsideL .~ cards)) =<< getGame

setAsideCodes :: GameT [CardCode]
setAsideCodes = getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . scenarioVictoryDisplay . toAttrs . (.scenario))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

setDeck :: PlayerId -> [CardDef] -> GameT ()
setDeck pid defs = do
  cards <- traverse (\d -> PlayerCard <$> genPlayerCard d (Just pid)) defs
  putGame . (playersL . ix pid . deckL .~ cards) =<< getGame

-- | The enemy attacks the player, who takes it undefended on Beravor.
attackUndefended :: PlayerId -> EnemyId -> GameT ()
attackUndefended pid eid = do
  _ <- setEncounterDeck []
  run $ EnemyAttack pid eid
  passActions pid
  chooseLabel "$label.takeAttackUndefended" pid
  passActions pid
  chooseTarget pid =<< hero "01012"
  passActions pid

activeQuestCode :: GameT CardCode
activeQuestCode = toCardCode <$> getActiveQuest

spec :: Spec
spec = describe "The Mûmakil" do
  it "is a scenario the game can be dealt" . mumakilTest $ \_ ->
    isJust (lookupScenario "the-mumakil") `shouldBe` True

  -- \* Setup and stage 1

  it "sets aside each Wild Mûmak and Capture objective, and each player adds a location to the staging area" . mumakilTest $ \pid -> do
    setup pid
    aside <- setAsideCodes
    length (filter (== toCardCode Enemies.wildMumak) aside) `shouldBe` 4
    sort (filter (/= toCardCode Enemies.wildMumak) aside)
      `shouldBe` sort (map toCardCode [Objectives.horseHairLasso, Objectives.poisonedSpear, Objectives.pitTrap, Objectives.nooseOfVines])
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Enemies.wildMumak)
    -- 22 cards of The Mûmakil and 10 of Jungle Forest, less the location staged
    length deck `shouldBe` 31
    staged <- getsGame (filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
    length staged `shouldBe` 1
    activeQuestCode `shouldReturn` toCardCode Quests.welcomeToTheJungle

  it "shuffles a set-aside Wild Mûmak into the encounter deck after the active location is explored" . mumakilTest $ \pid -> do
    setup pid
    lid <- activate Locations.jungleTrail
    run $ PlaceTokens GameSource (toTarget lid) Progress 4
    length . filter (== toCardCode Enemies.wildMumak) <$> setAsideCodes `shouldReturn` 3
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` elem (toCardCode Enemies.wildMumak)

  it "cannot advance until there are as many Wild Mûmakil in play as players, then attaches a Capture objective to one" . mumakilTest $ \pid -> do
    setup pid
    passActions pid
    q <- getActiveQuest
    run $ PlaceTokens GameSource (toTarget q) Progress 12
    activeQuestCode `shouldReturn` toCardCode Quests.welcomeToTheJungle
    mumak <- stage Enemies.wildMumak
    activeQuestCode `shouldReturn` toCardCode Quests.captureTheOliphaunts
    resolveWhenRevealed pid
    attached <- getsGame (map (.placement) . toList . (.entities.objectives))
    attached `shouldBe` [AttachedTo (EnemyTarget mumak)]
    length <$> setAsideCodes `shouldReturn` 7

  it "cannot advance while there are more Wild Mûmakil in play than players" . mumakilTest $ \pid -> do
    setup pid
    _ <- stage Enemies.wildMumak
    _ <- stage Enemies.wildMumak
    q <- getActiveQuest
    getModifiers (toTarget q) >>= (`shouldSatisfy` elem CannotBeDefeated)

  -- \* Stage 2

  it "cannot be defeated while a Capture objective is in play, and the players win once none is" . mumakilTest $ \pid -> do
    mumak <- stage Enemies.wildMumak
    setAsideCards [Objectives.pitTrap]
    q <- putQuestStage Quests.captureTheOliphaunts
    run $ Flip q
    resolveWhenRevealed pid
    oid <- getsGame (fromJustNote "no objective" . fmap toId . headMay . toList . (.entities.objectives))
    run $ PlaceTokens GameSource q Progress 8
    getsGame (.outcome) `shouldReturn` Nothing
    run $ AddToVictoryDisplay (ObjectiveTarget oid)
    run $ AddToVictoryDisplay (EnemyTarget mumak)
    getsGame (.outcome) `shouldReturn` Just Won

  it "reveals an additional encounter card at the beginning of the quest phase while there is no active location" . mumakilTest $ \pid -> do
    q <- putQuestStage Quests.captureTheOliphaunts
    run $ Flip q
    resolveWhenRevealed pid
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    getModifiers GameTarget >>= (`shouldSatisfy` elem (AdditionalStagingReveals 1))

  it "reveals no more with an active location" . mumakilTest $ \pid -> do
    q <- putQuestStage Quests.captureTheOliphaunts
    run $ Flip q
    resolveWhenRevealed pid
    _ <- activate Locations.jungleTrail
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    getModifiers GameTarget >>= (`shouldSatisfy` notElem (AdditionalStagingReveals 1))

  -- \* Wild Mûmak and the Capture objectives

  it "has Wild Mûmak take no more than 3 damage a round, immune to player card effects" . mumakilTest $ \_ -> do
    mumak <- stage Enemies.wildMumak
    run $ DealDamage GameSource (toTarget mumak) 2
    run $ DealDamage GameSource (toTarget mumak) 2
    getDamage (toTarget mumak) `shouldReturn` 3
    getModifiers mumak >>= (`shouldSatisfy` elem ImmuneToPlayerCardEffects)

  it "captures with Horse-hair Lasso when the engaged player's top card costs at least the Mûmak's hit points left" . mumakilTest $ \pid -> do
    mumak <- engaged pid Enemies.wildMumak
    wound mumak 6
    lasso <- attachCapture Objectives.horseHairLasso mumak
    setDeck pid [Allies.guardOfTheCitadel]
    run $ DealDamage GameSource (toTarget mumak) 1
    chooseAbilityOf pid (ObjectiveTarget lasso)
    victoryDisplay >>= (`shouldSatisfy` (\vd -> all (`elem` vd) [toCardCode Objectives.horseHairLasso, toCardCode Enemies.wildMumak]))
    placementOfEnemy mumak `shouldReturn` Nothing

  it "captures with Poisoned Spear when the encounter card discarded has threat at least the Mûmak's hit points left" . mumakilTest $ \pid -> do
    mumak <- engaged pid Enemies.wildMumak
    wound mumak 7
    spear <- attachCapture Objectives.poisonedSpear mumak
    _ <- setEncounterDeck [Locations.jungleTrail]
    run $ CheckWindows [mkAfter QuestedSuccessfully]
    chooseAbilityOf pid (ObjectiveTarget spear)
    placementOfEnemy mumak `shouldReturn` Nothing
    victoryDisplay >>= (`shouldSatisfy` elem (toCardCode Objectives.poisonedSpear))

  it "keeps Poisoned Spear on the Mûmak when the threat falls short" . mumakilTest $ \pid -> do
    mumak <- engaged pid Enemies.wildMumak
    spear <- attachCapture Objectives.poisonedSpear mumak
    _ <- setEncounterDeck [Locations.jungleTrail]
    run $ CheckWindows [mkAfter QuestedSuccessfully]
    chooseAbilityOf pid (ObjectiveTarget spear)
    placementOfEnemy mumak `shouldReturn` Just (EngagedWith pid)

  it "captures with Pit Trap after the players travel to a location whose printed threat is at least the Mûmak's hit points left" . mumakilTest $ \pid -> do
    mumak <- engaged pid Enemies.wildMumak
    wound mumak 7
    trap <- attachCapture Objectives.pitTrap mumak
    lid <- activate Locations.denseJungle
    run $ CheckWindows [mkAfter $ Traveled lid]
    chooseAbilityOf pid (ObjectiveTarget trap)
    placementOfEnemy mumak `shouldReturn` Nothing

  it "captures with Noose of Vines after a character with more hit points left defends the Mûmak's attack" . mumakilTest $ \pid -> do
    mumak <- engaged pid Enemies.wildMumak
    wound mumak 7
    noose <- attachCapture Objectives.nooseOfVines mumak
    beravor <- hero "01012"
    run $ CheckWindows [mkAfter $ DefendedAttack beravor mumak]
    chooseAbilityOf pid (ObjectiveTarget noose)
    placementOfEnemy mumak `shouldReturn` Nothing

  it "offers Noose of Vines nothing after an attack by another enemy" . mumakilTest $ \pid -> do
    mumak <- engaged pid Enemies.wildMumak
    noose <- attachCapture Objectives.nooseOfVines mumak
    spider <- engaged pid Enemies.forestSpider
    beravor <- hero "01012"
    run $ CheckWindows [mkAfter $ DefendedAttack beravor spider]
    offersAbilityOf pid (ObjectiveTarget noose) `shouldReturn` False

  -- \* Encounter cards

  it "gives Territorial Ape the active location's threat as threat and attack" . mumakilTest $ \_ -> do
    ape <- stage Enemies.territorialApe
    _ <- activate Locations.jungleTrail
    -- Jungle Trail's 4 threat
    getEnemyThreat ape `shouldReturn` 6
    getEnemyAttack ape `shouldReturn` 6

  it "attaches Strangling Python to the character it damages, which cannot ready" . mumakilTest $ \pid -> do
    python <- engaged pid Enemies.stranglingPython
    attackUndefended pid python
    beravor <- hero "01012"
    placementOfEnemy python `shouldReturn` Just (AttachedTo beravor)
    getModifiers beravor >>= (`shouldSatisfy` elem CannotReady)

  it "blanks the text box of the character Giant Centipede damages" . mumakilTest $ \pid -> do
    centipede <- engaged pid Enemies.giantCentipede
    attackUndefended pid centipede
    (getModifiers =<< hero "01012") >>= (`shouldSatisfy` elem Blank)

  it "adds a Wild Mûmak to the staging area as Mûmak Trail is explored while active" . mumakilTest $ \pid -> do
    lid <- activate Locations.mumakTrail
    _ <- setEncounterDeck [Enemies.wildMumak]
    run $ PlaceTokens GameSource (toTarget lid) Progress 3
    chooseAbilityOf pid lid
    chooseOptionMatching "add the Wild Mûmak" pid (const True)
    mumak <- enemyOf Enemies.wildMumak
    placementOfEnemy mumak `shouldReturn` Just StagingArea

  it "has Stampeding Oliphaunt's Wild Mûmak in the staging area attack the first player" . mumakilTest $ \pid -> do
    mumak <- stage Enemies.wildMumak
    reveal [Treacheries.stampedingOliphaunt]
    passActions pid
    Just (QuestionLabel _ source _ _) <- getQuestion pid
    source `shouldBe` Just (toTarget mumak)

  it "has Stampeding Oliphaunt add a Mûmak Trail with no Wild Mûmak in the staging area" . mumakilTest $ \pid -> do
    threat0 <- (.threat) <$> getPlayer pid
    reveal [Treacheries.stampedingOliphaunt, Locations.mumakTrail]
    chooseOptionMatching "add the Mûmak Trail" pid (const True)
    lid <- locationOf Locations.mumakTrail
    getsGame (fmap (.placement) . lookup lid . (.entities.locations)) `shouldReturn` Just StagingArea
    -- Doomed 1
    (.threat) <$> getPlayer pid `shouldReturn` (threat0 + 1)

  it "has The Savage South discard the top 5 cards of the deck and each card in play sharing a title with them" . mumakilTest $ \pid -> do
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    setDeck pid (Allies.guardOfTheCitadel : replicate 5 Allies.gandalf)
    reveal [Treacheries.theSavageSouth]
    getsGame (map toTarget . toList . (.entities.allies)) >>= (`shouldSatisfy` notElem guard')
    length . (.deck) <$> getPlayer pid `shouldReturn` 1

  it "removes the hero Terrible Fever attaches to from the quest, dealing it 1 damage at the end of the round" . mumakilTest $ \pid -> do
    reveal [Treacheries.terribleFever]
    beravor <- hero "01012"
    chooseTarget pid beravor
    run $ CheckWindows [mkWhen RoundEnds]
    getDamage beravor `shouldReturn` 1

  it "has Biting Insects discard a random card from hand and deal its cost in damage" . mumakilTest $ \pid -> do
    putGame . (playersL . ix pid . handL .~ []) =<< getGame
    _ <- putCardIntoHand pid Allies.guardOfTheCitadel
    reveal [Treacheries.bitingInsects]
    beravor <- hero "01012"
    chooseTarget pid beravor
    chooseTarget pid beravor
    length . (.hand) <$> getPlayer pid `shouldReturn` 0
    getDamage beravor `shouldReturn` 2

  it "gives each location +1 threat while Guardians of the Jungle is in play, and engages a Territorial Ape as it becomes the current quest" . mumakilTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.jungleTrail
    lid <- locationOf Locations.jungleTrail
    run . AddToStaging =<< genEncounterCard Quests.guardiansOfTheJungle
    getLocationThreat lid `shouldReturn` 5
    _ <- setEncounterDeck [Enemies.territorialApe]
    q <- getsGame (fromJustNote "no side quest" . find ((== toCardCode Quests.guardiansOfTheJungle) . toCardCode) . toList . (.entities.quests))
    run $ SetCurrentQuest (toTarget q)
    chooseOptionMatching "engage the Territorial Ape" pid (const True)
    ape <- enemyOf Enemies.territorialApe
    placementOfEnemy ape `shouldReturn` Just (EngagedWith pid)
