module LordOfTheRings.HeirsOfNumenor.IntoIthilienSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getCommittedWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getEnemyThreat)
import LordOfTheRings.Keyword (Keyword (Siege))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (currentQuestHas, getArcheryTotal)
import LordOfTheRings.Modifier (ModifierType (EngagementCostModifier, NoEngagementChecks))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (intoIthilien, lookupScenario)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (QuestedUnsuccessfully, RoundEnds), mkAfter, mkWhen)
import TestImport

ithilienTest :: (PlayerId -> GameT ()) -> IO ()
ithilienTest = gameTestIn intoIthilien testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
      locations = [l.placement | l <- toList g.entities.locations, toCardCode l == code]
      enemies = [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
      objectives = [o.placement | o <- toList g.entities.objectives, toCardCode o == code]
      allies = [a.placement | a <- toList g.entities.allies, toCardCode a == code]
  pure $ headMay (locations <> enemies <> objectives <> allies)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

objectiveOf :: CardDef -> GameT ObjectiveId
objectiveOf def = getsGame (fromJustNote "no such objective" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

stageCode :: GameT CardCode
stageCode = toCardCode <$> getActiveQuest

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | Celador in the staging area, as the setup adds him.
addCelador :: GameT ObjectiveId
addCelador = do
  run . AddToStaging =<< genEncounterCard Objectives.celador
  objectiveOf Objectives.celador

-- | Complete the active stage, resolving the next one's When Revealed when it asks.
completeStage :: PlayerId -> GameT ()
completeStage pid = do
  quest <- getActiveQuest
  run $ PlaceTokens GameSource (toTarget quest) Progress quest.questPoints
  getQuestion pid >>= \case
    Just (QuestionLabel l _ _ _) | "$label.resolveWhenRevealed" `isPrefixOf` l -> resolveWhenRevealed pid
    _ -> pure ()

beravor :: GameT Target
beravor = toTarget <$> getHeroWithCode "01012"

eowyn :: GameT Target
eowyn = toTarget <$> getHeroWithCode "01007"

spec :: Spec
spec = describe "Into Ithilien" do
  it "is a scenario the game can be dealt" . ithilienTest $ \_ ->
    isJust (lookupScenario "into-ithilien") `shouldBe` True

  it "adds Celador and a Southron Company per player to the staging area, and makes Ithilien Road active" . ithilienTest $ \pid -> do
    flipFirstQuest pid
    placementOf Objectives.celador `shouldReturn` Just StagingArea
    placementOf Locations.ithilienRoad `shouldReturn` Just ActiveLocation
    companies <- getsGame (filter ((== toCardCode Enemies.southronCompany) . toCardCode) . toList . (.entities.enemies))
    map (.placement) companies `shouldBe` [StagingArea]
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Objectives.celador)

  it "quests with Celador from the staging area, by his attack under battle" . ithilienTest $ \_ -> do
    _ <- putQuestStage Quests.ambushInIthilien
    _ <- addCelador
    getCommittedWillpower `shouldReturn` 2
    run . CommitToQuest =<< beravor
    -- Beravor's 2 attack
    getCommittedWillpower `shouldReturn` 4

  it "gives Southron Company +2 threat and attack under battle" . ithilienTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Enemies.southronCompany
    company <- enemyOf Enemies.southronCompany
    getEnemyThreat company `shouldReturn` 1
    _ <- putQuestStage Quests.ambushInIthilien
    getEnemyThreat company `shouldReturn` 3
    getEnemyAttack company `shouldReturn` 5

  it "bypasses stage 2 when stage 1 is completed with Celador in the staging area, and gives him to the first player" . ithilienTest $ \pid -> do
    flipFirstQuest pid
    completeStage pid
    stageCode `shouldReturn` toCardCode Quests.theHiddenWay
    placementOf Objectives.celador `shouldReturn` Just (InPlayArea pid)
    -- enemies make no engagement checks
    getModifiers GameTarget >>= (`shouldSatisfy` elem NoEngagementChecks)
    completeStage pid
    stageCode `shouldReturn` toCardCode Quests.approachingCairAndros
    completeStage pid
    getsGame (.outcome) `shouldReturn` Just Won

  it "goes through stage 2, bypassing stage 3, once Celador has left play" . ithilienTest $ \pid -> do
    flipFirstQuest pid
    celador <- objectiveOf Objectives.celador
    run $ DealDamage GameSource (toTarget celador) 3
    placementOf Objectives.celador `shouldReturn` Nothing
    -- removed from the game, not discarded
    map toCardCode <$> getEncounterDiscard >>= (`shouldSatisfy` notElem (toCardCode Objectives.celador))
    completeStage pid
    stageCode `shouldReturn` toCardCode Quests.southronCounterAttack
    currentQuestHas Siege `shouldReturn` True
    -- Archery X, one player
    getArcheryTotal `shouldReturn` 1
    completeStage pid
    stageCode `shouldReturn` toCardCode Quests.approachingCairAndros

  it "damages Celador when the players quest unsuccessfully" . ithilienTest $ \_ -> do
    _ <- putQuestStage Quests.ambushInIthilien
    celador <- addCelador
    run $ CheckWindows [mkAfter QuestedUnsuccessfully]
    getDamage (toTarget celador) `shouldReturn` 1

  it "keeps the damage on Celador as the first player takes control of him" . ithilienTest $ \pid -> do
    celador <- addCelador
    run $ DealDamage GameSource (toTarget celador) 2
    quest <- putQuestStage Quests.theHiddenWay
    run $ Flip quest
    resolveWhenRevealed pid
    ally <- getsGame (fromJustNote "no Celador" . find ((== toCardCode Objectives.celador) . toCardCode) . toList . (.entities.allies))
    ally.damage `shouldBe` 2

  it "deals 1 damage to each committed character with Blocking Wargs, Celador too, and surges" . ithilienTest $ \_ -> do
    _ <- putQuestStage Quests.ambushInIthilien
    celador <- addCelador
    b <- beravor
    run $ CommitToQuest b
    reveal [Treacheries.blockingWargs, Locations.secludedGlade]
    getDamage b `shouldReturn` 1
    getDamage (toTarget celador) `shouldReturn` 1
    placementOf Locations.secludedGlade `shouldReturn` Just StagingArea

  it "makes each enemy's engagement cost 0 while Ithilien Road is active" . ithilienTest $ \_ -> do
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.ithilienRoad
    run . AddToStaging =<< genEncounterCard Enemies.mumak
    mumak <- enemyOf Enemies.mumak
    getModifiers (toTarget mumak) >>= (`shouldSatisfy` elem (EngagementCostModifier (-38)))

  it "lets Mumak take no more than 3 damage a round" . ithilienTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Enemies.mumak
    mumak <- toTarget <$> enemyOf Enemies.mumak
    run $ DealDamage GameSource mumak 2
    run $ DealDamage GameSource mumak 2
    getDamage mumak `shouldReturn` 3
    run $ DealDamage GameSource mumak 1
    getDamage mumak `shouldReturn` 3

  it "gives Southron Mercenaries archery for each player" . ithilienTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Enemies.southronMercenaries
    getArcheryTotal `shouldReturn` 1

  it "has Haradrim Elite attack the first player as it enters play" . ithilienTest $ \pid -> do
    _ <- setEncounterDeck [Locations.secludedGlade]
    run . AddToStaging =<< genEncounterCard Enemies.haradrimElite
    passActions pid
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldSatisfy` ("$label.declareDefender" `isPrefixOf`)

  it "has Haradrim Elite attack the first player as it enters play engaged" . ithilienTest $ \pid -> do
    _ <- setEncounterDeck [Locations.secludedGlade]
    run . PutEnemyIntoPlayEngaged pid =<< genEncounterCard Enemies.haradrimElite
    passActions pid
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldSatisfy` ("$label.declareDefender" `isPrefixOf`)

  it "has each player remove 1 character from the quest with Lost Company" . ithilienTest $ \pid -> do
    b <- beravor
    e <- eowyn
    run $ CommitToQuest b
    run $ CommitToQuest e
    reveal [Treacheries.lostCompany]
    chooseTarget pid b
    heroCommitted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False
    heroCommitted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True

  it "deals 2 damage to a questing hero with Forest Bat and removes it from the quest" . ithilienTest $ \_ -> do
    b <- beravor
    run $ CommitToQuest b
    reveal [Enemies.forestBat]
    getDamage b `shouldReturn` 2
    heroCommitted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False

  it "raises threat by the questing characters with Watcher in the Woods, surging under battle" . ithilienTest $ \pid -> do
    _ <- putQuestStage Quests.ambushInIthilien
    run . CommitToQuest =<< beravor
    run . CommitToQuest =<< eowyn
    threat <- (.threat) <$> getPlayer pid
    reveal [Treacheries.watcherInTheWoods, Locations.secludedGlade]
    ((.threat) <$> getPlayer pid) `shouldReturn` threat + 2
    placementOf Locations.secludedGlade `shouldReturn` Just StagingArea

  it "surges with Watcher in the Woods once its threat brings Approaching Cair Andros siege (ruling)" . ithilienTest $ \pid -> do
    _ <- putQuestStage Quests.approachingCairAndros
    run . CommitToQuest =<< beravor
    threat <- (.threat) <$> getPlayer pid
    run $ RaiseThreat pid (36 - threat)
    reveal [Treacheries.watcherInTheWoods, Locations.secludedGlade]
    placementOf Locations.secludedGlade `shouldReturn` Just StagingArea

  it "gives Morgul Spider +1 attack for each character not committed" . ithilienTest $ \_ -> do
    run . CommitToQuest =<< beravor
    reveal [Enemies.morgulSpider]
    spider <- enemyOf Enemies.morgulSpider
    -- Éowyn and the third hero stay back
    getEnemyAttack spider `shouldReturn` 6

  it "adds Ithilien Guardian to the staging area, committed, and surges" . ithilienTest $ \_ -> do
    _ <- putQuestStage Quests.theHiddenWay
    reveal [Objectives.ithilienGuardian, Locations.secludedGlade]
    placementOf Objectives.ithilienGuardian `shouldReturn` Just StagingArea
    placementOf Locations.secludedGlade `shouldReturn` Just StagingArea
    getCommittedWillpower `shouldReturn` 2

  it "does not surge with Ithilien Guardian's When Revealed canceled" . ithilienTest $ \pid -> do
    _ <- putQuestStage Quests.theHiddenWay
    tow <- putCardIntoHand pid Events.aTestOfWill
    spirit <- eowyn
    run $ PlaceTokens GameSource spirit Resource 1
    reveal [Objectives.ithilienGuardian, Locations.secludedGlade]
    chooseTarget pid (CardIdTarget tow.id)
    placementOf Locations.secludedGlade `shouldReturn` Nothing

  it "exhausts a Ranger as Overgrown Trail's cost to place 3 progress on it" . ithilienTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Locations.overgrownTrail
    trail <- getsGame (fromJustNote "no trail" . headMay . map toId . toList . (.entities.locations))
    -- Beravor is a Ranger too, so the cost asks which
    tracker <- putAllyIntoPlay pid Allies.ithilienTracker
    run $ ActionWindow pid
    chooseAbilityOf pid (LocationTarget trail)
    chooseTarget pid tracker
    getsGame (fmap (.progress) . lookup trail . (.entities.locations)) `shouldReturn` Just 3
    getsGame (any (\a -> toTarget a == tracker && allyExhausted (toAttrs a)) . toList . (.entities.allies)) `shouldReturn` True

  it "adds a Harad enemy to the staging area with Southron Support" . ithilienTest $ \pid -> do
    elite <- genEncounterCard Enemies.haradrimElite
    _ <- setEncounterDeck [Treacheries.southronSupport, Enemies.forestBat]
    run $ AddToEncounterDiscard [elite]
    run RevealEncounterCard
    resolveRevealed
    chooseTarget pid elite
    placementOf Enemies.haradrimElite `shouldReturn` Just StagingArea

  it "raises each player's threat by 2 at the end of the round, and gains siege at 37 threat" . ithilienTest $ \pid -> do
    _ <- putQuestStage Quests.approachingCairAndros
    currentQuestHas Siege `shouldReturn` False
    threat <- (.threat) <$> getPlayer pid
    run $ CheckWindows [mkWhen RoundEnds]
    ((.threat) <$> getPlayer pid) `shouldReturn` threat + 2
    run $ RaiseThreat pid (37 - threat - 2)
    currentQuestHas Siege `shouldReturn` True
