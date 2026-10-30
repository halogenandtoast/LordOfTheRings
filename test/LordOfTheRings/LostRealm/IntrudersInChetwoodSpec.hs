module LordOfTheRings.LostRealm.IntrudersInChetwoodSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getCurrentQuest, getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (CannotReduceThreat, CannotTakeDamage, NoEngagementChecks, ProgressPerRoundLimit, ThreatModifier))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (QuestPhase, RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (intrudersInChetwood, lookupScenario)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseBegins, PhaseEnds), mkAfter, mkWhen)
import TestImport

chetwoodTest :: (PlayerId -> GameT ()) -> IO ()
chetwoodTest = gameTestIn intrudersInChetwood testDeck

-- | Setup done: the player picks the location he adds to the staging area.
setup :: PlayerId -> GameT ()
setup pid = do
  flipFirstQuest pid
  chooseOptionMatching "add a location" pid (const True)

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [a.placement | a <- toList g.entities.allies, toCardCode a == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

questOf :: CardDef -> GameT Target
questOf def = getsGame (fromJustNote "no such quest" . fmap toTarget . find ((== toCardCode def) . toCardCode) . toList . (.entities.quests))

iarion :: GameT AllyId
iarion = getsGame (fromJustNote "no Iarion" . fmap toId . find ((== toCardCode Objectives.iarion) . toCardCode) . toList . (.entities.allies))

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

handSize :: PlayerId -> GameT Int
handSize pid = length . (.hand) <$> getPlayer pid

spec :: Spec
spec = describe "Intruders in Chetwood" do
  it "is a scenario the game can be dealt" . chetwoodTest $ \_ ->
    isJust (lookupScenario "intruders-in-chetwood") `shouldBe` True

  it "gives Iârion to the first player and stages an Orc War Party and a location" . chetwoodTest $ \pid -> do
    setup pid
    placementOf Objectives.iarion `shouldReturn` Just (InPlayArea pid)
    placementOf Enemies.orcWarParty `shouldReturn` Just StagingArea
    locations <- getsGame (filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
    length locations `shouldBe` 1
    deck <- map toCardCode <$> getEncounterDeck
    deck `shouldSatisfy` notElem (toCardCode Objectives.iarion)
    length (filter (== toCardCode Enemies.orcWarParty) deck) `shouldBe` 2

  it "gives Iârion X willpower, X the number of quest cards in play" . chetwoodTest $ \pid -> do
    setup pid
    i <- iarion
    getWillpower (AllyTarget i) `shouldReturn` 1
    staged Quests.orcRearguard
    getWillpower (AllyTarget i) `shouldReturn` 2

  it "loses the game as Iârion leaves play" . chetwoodTest $ \pid -> do
    setup pid
    i <- iarion
    run $ Discard (AllyTarget i)
    outcome `shouldReturn` Just Lost

  it "keeps the enemies from engagement checks, and raises threat for each staged enemy at the end of the refresh phase" . chetwoodTest $ \pid -> do
    setup pid
    party <- enemyOf Enemies.orcWarParty
    getModifiers (EnemyTarget party) >>= (`shouldSatisfy` elem NoEngagementChecks)
    threat <- threatOf pid
    run $ CheckWindows [mkWhen $ PhaseEnds RefreshPhase]
    threatOf pid `shouldReturn` threat + 1

  it "cannot be won while an Orc War Party is in play, and is won once the last one leaves" . chetwoodTest $ \pid -> do
    setup pid
    q <- questOf Quests.stopTheWarParty
    run $ PlaceTokens GameSource q Progress 30
    outcome `shouldReturn` Nothing
    party <- enemyOf Enemies.orcWarParty
    getModifiers (EnemyTarget party) >>= (`shouldSatisfy` elem CannotTakeDamage)
    run $ Discard (EnemyTarget party)
    outcome `shouldReturn` Just Won

  it "is not won while Iârion is underneath Rescue Iârion, and is once he is rescued" . chetwoodTest $ \pid -> do
    setup pid
    run . Discard . EnemyTarget =<< enemyOf Enemies.orcWarParty
    reveal [Quests.rescueIarion]
    i <- iarion
    rescue <- questOf Quests.rescueIarion
    placementOf Objectives.iarion `shouldReturn` Just (AttachedTo rescue)
    getTimeCounters rescue `shouldReturn` 4
    q <- questOf Quests.stopTheWarParty
    run $ PlaceTokens GameSource q Progress 30
    outcome `shouldReturn` Nothing
    run $ PlaceTokens GameSource rescue Progress 6
    placementOf Objectives.iarion `shouldReturn` Just (InPlayArea pid)
    getsGame (fmap (allyExhausted . toAttrs) . find ((== i) . toId) . toList . (.entities.allies)) `shouldReturn` Just True
    outcome `shouldReturn` Just Won

  it "loses Iârion, and the game, as Rescue Iârion's last time counter is removed" . chetwoodTest $ \pid -> do
    setup pid
    reveal [Quests.rescueIarion]
    rescue <- questOf Quests.rescueIarion
    run $ RemoveTokens GameSource rescue TimeCounter 4
    outcome `shouldReturn` Just Lost

  it "bars threat reduction while Outlying Homestead is staged" . gameTest $ \pid -> do
    staged Locations.outlyingHomestead
    getModifiers (PlayerTarget pid) >>= (`shouldSatisfy` elem CannotReduceThreat)

  it "gives Shrouded Hills a threat for each quest card, and surge with only one" . gameTest $ \_ -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    reveal [Locations.shroudedHills, Locations.ruggedCountry]
    hills <- locationOf Locations.shroudedHills
    getLocationThreat hills `shouldReturn` 1
    placementOf Locations.ruggedCountry `shouldReturn` Just StagingArea

  it "raises Rugged Country's threat by 2 while the current quest is a side quest" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    staged Locations.ruggedCountry
    staged Quests.orcRearguard
    rugged <- locationOf Locations.ruggedCountry
    getLocationThreat rugged `shouldReturn` 2
    run $ BeginPhase QuestPhase
    chooseTarget pid =<< questOf Quests.orcRearguard
    getLocationThreat rugged `shouldReturn` 4

  it "keeps the current quest to 3 progress a round while Orc Rearguard is in play" . gameTest $ \_ -> do
    main <- putQuestStage Quests.fliesAndSpiders
    staged Quests.orcRearguard
    getModifiers main >>= (`shouldSatisfy` elem (ProgressPerRoundLimit 3))

  it "shuffles Sudden Assault and the discard pile back when no attack is made" . gameTest $ \_ -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.ruggedCountry
    reveal [Treacheries.suddenAssault]
    getEncounterDiscard `shouldReturn` []
    deck <- map toCardCode <$> getEncounterDeck
    sort deck `shouldBe` sort [toCardCode Treacheries.suddenAssault, toCardCode Locations.ruggedCountry]

  it "gives Surprising Speed surge and doomed 1 when no enemy is engaged" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    threat <- threatOf pid
    reveal [Treacheries.surprisingSpeed, Locations.ruggedCountry]
    threatOf pid `shouldReturn` threat + 1
    placementOf Locations.ruggedCountry `shouldReturn` Just StagingArea

  it "returns an engaged enemy to the staging area with Surprising Speed" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    staged Enemies.angmarCaptain
    run . EngageEnemy pid =<< enemyOf Enemies.angmarCaptain
    reveal [Treacheries.surprisingSpeed]
    placementOf Enemies.angmarCaptain `shouldReturn` Just StagingArea

  it "returns an engaged enemy as the quest phase begins while Borders of Bree-land is staged" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    staged Locations.bordersOfBreeLand
    staged Enemies.angmarCaptain
    run . EngageEnemy pid =<< enemyOf Enemies.angmarCaptain
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    placementOf Enemies.angmarCaptain `shouldReturn` Just StagingArea

  it "makes each Orc stronger and engages one with each player as Orc Ambush is revealed" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.angmarOrc
    reveal [Quests.orcAmbush]
    chooseOptionMatching "pick the Orc" pid (const True)
    placementOf Enemies.angmarOrc `shouldReturn` Just (EngagedWith pid)
    orc <- enemyOf Enemies.angmarOrc
    getModifiers (EnemyTarget orc) >>= (`shouldSatisfy` elem (ThreatModifier 1))

  it "puts each hand underneath Lost in the Wilderness, back once it is defeated" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    _ <- putCardIntoHand pid Treacheries.suddenAssault
    n <- handSize pid
    reveal [Quests.lostInTheWilderness]
    handSize pid `shouldReturn` 0
    lost <- questOf Quests.lostInTheWilderness
    run $ PlaceTokens GameSource lost Progress 4
    handSize pid `shouldReturn` n

  it "reveals an encounter card for each quest card with Weight of Responsibility" . gameTest $ \_ -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    staged Quests.orcRearguard
    reveal [Treacheries.weightOfResponsibility, Locations.ruggedCountry, Locations.chetwoodForest]
    placementOf Locations.ruggedCountry `shouldReturn` Just StagingArea
    placementOf Locations.chetwoodForest `shouldReturn` Just StagingArea

  it "reveals an encounter side quest with Pressing Needs" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    run . AddToEncounterDiscard . pure =<< genEncounterCard Quests.orcRearguard
    threat <- threatOf pid
    reveal [Treacheries.pressingNeeds]
    chooseOptionMatching "pick the side quest" pid (const True)
    void $ questOf Quests.orcRearguard
    threatOf pid `shouldReturn` threat + 2

  it "makes Pressing Needs choose another current quest" . gameTest $ \pid -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    staged Quests.orcRearguard
    reveal [Treacheries.pressingNeeds]
    rearguard <- questOf Quests.orcRearguard
    chooseTarget pid rearguard
    getCurrentQuest `shouldReturn` Just rearguard

  it "reveals another card with Angmar Orc when no ally is in play" . gameTest $ \_ -> do
    _ <- putQuestStage Quests.fliesAndSpiders
    reveal [Enemies.angmarOrc, Locations.ruggedCountry]
    placementOf Locations.ruggedCountry `shouldReturn` Just StagingArea
