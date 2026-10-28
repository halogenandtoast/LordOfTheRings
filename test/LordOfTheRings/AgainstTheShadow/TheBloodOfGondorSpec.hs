module LordOfTheRings.AgainstTheShadow.TheBloodOfGondorSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationQuestPoints, getStagingThreat)
import LordOfTheRings.Keyword (Keyword (Battle, Siege))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (currentQuestHas, getArcheryTotal)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (CombatPhase, QuestPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theBloodOfGondor)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (PhaseBegins, RoundEnds), mkAfter)
import TestImport

bloodTest :: (PlayerId -> GameT ()) -> IO ()
bloodTest = gameTestIn theBloodOfGondor testDeck

hiddenOf :: PlayerId -> GameT [CardCode]
hiddenOf pid = getsGame (map toCardCode . findWithDefault [] pid . (.hiddenCards) . toAttrs . (.scenario))

-- | The player takes the cards as hidden cards, in order.
hide :: PlayerId -> [CardDef] -> GameT ()
hide pid defs = do
  _ <- setEncounterDeck defs
  replicateM_ (length defs) $ run (TakeHiddenCard pid)

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [a.placement | a <- toList g.entities.allies, toCardCode a == code]

allyOf :: CardDef -> GameT AllyId
allyOf def = getsGame (fromJustNote "no such ally" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.allies))

stage :: CardDef -> GameT ()
stage def = run . AddToStaging =<< genEncounterCard def

engaged :: PlayerId -> CardDef -> GameT EnemyId
engaged pid def = do
  stage def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  pure eid

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

discardCodes :: GameT [CardCode]
discardCodes = map toCardCode <$> getEncounterDiscard

spec :: Spec
spec = describe "The Blood of Gondor" do
  it "is a scenario the game can be dealt" . bloodTest $ \_ ->
    isJust (lookupScenario "the-blood-of-gondor") `shouldBe` True

  it "stages The Cross-roads and Black Númenorean, gives the first player Faramir and Lord Alcaron, and reveals a card at setup" . bloodTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    let setUp = [Locations.theCrossroads, Enemies.blackNumenorean, Objectives.faramirObjective, Objectives.lordAlcaron]
    _ <- setEncounterDeck (setUp <> [Locations.southernRoad])
    chooseOptionMatching "flip the quest" pid (const True)
    placementOf Locations.theCrossroads `shouldReturn` Just StagingArea
    placementOf Enemies.blackNumenorean `shouldReturn` Just StagingArea
    placementOf Locations.southernRoad `shouldReturn` Just StagingArea
    placementOf Objectives.faramirObjective `shouldReturn` Just (InPlayArea pid)
    placementOf Objectives.lordAlcaron `shouldReturn` Just (InPlayArea pid)
    getsGame (map toCardCode . (.questDeck) . toAttrs . (.scenario)) `shouldReturn` [toCardCode Quests.captured]

  -- \* Hidden cards

  it "takes the top encounter card facedown, shuffling the discard pile in when the deck is empty" . bloodTest $ \pid -> do
    hide pid [Enemies.evilCrow]
    hiddenOf pid `shouldReturn` [toCardCode Enemies.evilCrow]
    getEncounterDeck `shouldReturn` []
    run . AddToEncounterDiscard . pure =<< genEncounterCard Locations.northernRoad
    run $ TakeHiddenCard pid
    hiddenOf pid `shouldReturn` map toCardCode [Enemies.evilCrow, Locations.northernRoad]
    getEncounterDiscard `shouldReturn` []

  it "has each player take a hidden card as the quest phase begins at The Ambush" . bloodTest $ \pid -> do
    _ <- putQuestStage Quests.theAmbush
    _ <- setEncounterDeck [Locations.southernRoad]
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    hiddenOf pid `shouldReturn` [toCardCode Locations.southernRoad]

  it "turns hidden cards faceup at The Ambush's combat phase: an enemy engages, anything else is discarded" . bloodTest $ \pid -> do
    _ <- putQuestStage Quests.theAmbush
    hide pid [Locations.southernRoad, Enemies.blackNumenorean]
    run $ CheckWindows [mkAfter $ PhaseBegins CombatPhase]
    chooseLabel "$label.theAmbush.turnFaceup" pid
    hiddenOf pid `shouldReturn` []
    placementOf Enemies.blackNumenorean `shouldReturn` Just (EngagedWith pid)
    discardCodes `shouldReturn` [toCardCode Locations.southernRoad]

  it "may take another hidden card at The Ambush's combat phase instead, and must with none to turn" . bloodTest $ \pid -> do
    _ <- putQuestStage Quests.theAmbush
    _ <- setEncounterDeck [Locations.southernRoad, Locations.northernRoad]
    run $ CheckWindows [mkAfter $ PhaseBegins CombatPhase]
    hiddenOf pid `shouldReturn` [toCardCode Locations.southernRoad]
    run $ CheckWindows [mkAfter $ PhaseBegins CombatPhase]
    chooseLabel "$label.theAmbush.take" pid
    hiddenOf pid `shouldReturn` map toCardCode [Locations.southernRoad, Locations.northernRoad]

  it "discards an eliminated player's hidden cards" . bloodTest $ \pid -> do
    hide pid [Locations.southernRoad]
    run $ EliminatePlayer pid
    hiddenOf pid `shouldReturn` []
    discardCodes `shouldReturn` [toCardCode Locations.southernRoad]

  -- \* Enemies

  it "gives Black Númenorean a threat for each hidden card, and raises each player's threat by theirs at the end of the round" . bloodTest $ \pid -> do
    stage Enemies.blackNumenorean
    hide pid [Locations.southernRoad, Locations.northernRoad]
    getStagingThreat `shouldReturn` 2
    threat <- threatOf pid
    run $ CheckWindows [mkAfter RoundEnds]
    threatOf pid `shouldReturn` threat + 2

  it "discards a non-objective ally of the player Orc Ambusher engages" . bloodTest $ \pid -> do
    stage Objectives.faramirObjective
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- engaged pid Enemies.orcAmbusher
    chooseTarget pid guard'
    getsGame (any ((== guard') . toTarget) . toList . (.entities.allies)) `shouldReturn` False
    placementOf Objectives.faramirObjective `shouldReturn` Just (InPlayArea pid)

  it "gives the attacking enemy +3 attack undefended with Orc Ambusher's shadow" . bloodTest $ \pid -> do
    eid <- engaged pid Enemies.blackNumenorean
    _ <- setEncounterDeck [Enemies.orcAmbusher]
    run $ DealShadowCard eid
    run $ ResolveShadow eid pid []
    getEnemyAttack eid `shouldReturn` 8

  it "has the player Evil Crow engages take a hidden card" . bloodTest $ \pid -> do
    stage Enemies.evilCrow
    _ <- setEncounterDeck [Locations.southernRoad]
    run . EngageEnemy pid =<< enemyOf Enemies.evilCrow
    hiddenOf pid `shouldReturn` [toCardCode Locations.southernRoad]

  it "turns the hidden cards of the player whose character Brutal Uruk destroys faceup, and they attack" . bloodTest $ \pid -> do
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    uruk <- engaged pid Enemies.brutalUruk
    hide pid [Enemies.blackNumenorean]
    _ <- setEncounterDeck [Locations.southernRoad]
    run $ EnemyAttack pid uruk
    passActions pid
    chooseTarget pid guard'
    passActions pid
    hiddenOf pid `shouldReturn` []
    numenorean <- enemyOf Enemies.blackNumenorean
    placementOf Enemies.blackNumenorean `shouldReturn` Just (EngagedWith pid)
    getsGame (fmap (length . (.shadowCards)) . lookup numenorean . (.entities.enemies)) `shouldReturn` Just 1
    getQuestion pid >>= (`shouldSatisfy` isJust)

  it "leaves the hidden cards alone when Brutal Uruk's attack destroys no one" . bloodTest $ \pid -> do
    beravor <- toTarget <$> getHeroWithCode "01012"
    uruk <- engaged pid Enemies.brutalUruk
    hide pid [Enemies.blackNumenorean]
    run $ EnemyAttack pid uruk
    passActions pid
    chooseTarget pid beravor
    passActions pid
    hiddenOf pid `shouldReturn` [toCardCode Enemies.blackNumenorean]

  -- \* Locations

  it "gives the current quest siege in place of battle, and has a threat for each player, while The Cross-roads is in play" . bloodTest $ \_ -> do
    _ <- putQuestStage Quests.captured
    currentQuestHas Battle `shouldReturn` True
    stage Locations.theCrossroads
    currentQuestHas Battle `shouldReturn` False
    currentQuestHas Siege `shouldReturn` True
    getStagingThreat `shouldReturn` 1

  it "has the first player take hidden cards to travel to a road" . bloodTest $ \pid -> do
    stage Locations.easternRoad
    lid <- getsGame (fromJustNote "no road" . fmap toId . headMay . toList . (.entities.locations))
    _ <- setEncounterDeck [Locations.southernRoad, Locations.northernRoad]
    run $ TravelTo lid
    length <$> hiddenOf pid `shouldReturn` 2

  it "gives Western and Eastern Road +5 quest points in the staging area" . bloodTest $ \_ -> do
    stage Locations.westernRoad
    lid <- getsGame (fromJustNote "no road" . fmap toId . headMay . toList . (.entities.locations))
    getLocationQuestPoints lid `shouldReturn` 9
    run $ PlaceLocation lid ActiveLocation
    getLocationQuestPoints lid `shouldReturn` 4

  it "has archery for each player at The Dark Woods, and has the first player discard a hidden card once it is explored" . bloodTest $ \pid -> do
    stage Locations.theDarkWoods
    getArcheryTotal `shouldReturn` 1
    lid <- getsGame (fromJustNote "no woods" . fmap toId . headMay . toList . (.entities.locations))
    run $ PlaceLocation lid ActiveLocation
    hide pid [Locations.southernRoad]
    run $ PlaceTokens GameSource (toTarget lid) Progress 2
    hiddenOf pid `shouldReturn` []
    discardCodes >>= (`shouldSatisfy` elem (toCardCode Locations.southernRoad))

  -- \* Treacheries

  it "turns the first player's hidden cards faceup with Lying in Wait, resolving their When Revealed effects" . bloodTest $ \pid -> do
    hide pid [Treacheries.mordorLooms, Locations.northernRoad, Enemies.evilCrow]
    _ <- setEncounterDeck [Treacheries.lyingInWait, Locations.southernRoad, Locations.westernRoad]
    run RevealEncounterCard
    resolveRevealed
    placementOf Locations.northernRoad `shouldReturn` Just StagingArea
    placementOf Enemies.evilCrow `shouldReturn` Just (EngagedWith pid)
    -- Mordor Looms' has the player take one, as does Evil Crow engaging him: no surge
    hiddenOf pid `shouldReturn` map toCardCode [Locations.southernRoad, Locations.westernRoad]

  it "has each player take a hidden card with Mordor Looms, and surges" . bloodTest $ \pid -> do
    _ <- setEncounterDeck [Treacheries.mordorLooms, Locations.southernRoad, Locations.northernRoad]
    run RevealEncounterCard
    resolveRevealed
    hiddenOf pid `shouldReturn` [toCardCode Locations.southernRoad]
    placementOf Locations.northernRoad `shouldReturn` Just StagingArea

  it "turns the hidden cards faceup of the player whose character an attack with Mordor Looms' shadow destroys" . bloodTest $ \pid -> do
    guard' <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    eid <- engaged pid Enemies.blackNumenorean
    hide pid [Locations.southernRoad]
    _ <- setEncounterDeck [Treacheries.mordorLooms]
    run $ DealShadowCard eid
    run $ EnemyAttack pid eid
    passActions pid
    chooseTarget pid guard'
    passActions pid
    hiddenOf pid `shouldReturn` []
    discardCodes >>= (`shouldSatisfy` elem (toCardCode Locations.southernRoad))

  it "has engaged Orcs add their threat for the phase with Conflict at the Crossroads" . bloodTest $ \pid -> do
    _ <- engaged pid Enemies.brutalUruk
    _ <- setEncounterDeck [Treacheries.conflictAtTheCrossroads, Locations.southernRoad]
    run RevealEncounterCard
    resolveRevealed
    getStagingThreat `shouldReturn` 3
    placementOf Locations.southernRoad `shouldReturn` Nothing

  it "surges with Conflict at the Crossroads when no Orc is engaged" . bloodTest $ \_ -> do
    _ <- setEncounterDeck [Treacheries.conflictAtTheCrossroads, Locations.southernRoad]
    run RevealEncounterCard
    resolveRevealed
    placementOf Locations.southernRoad `shouldReturn` Just StagingArea

  -- \* Faramir and Lord Alcaron

  it "turns a hidden enemy faceup and damages it with Faramir's Combat Action" . bloodTest $ \pid -> do
    stage Objectives.faramirObjective
    faramir <- allyOf Objectives.faramirObjective
    hide pid [Enemies.blackNumenorean]
    putGame . (phaseL .~ CombatPhase) =<< getGame
    run $ ActionWindow pid
    chooseAbilityOf pid faramir
    numenorean <- enemyOf Enemies.blackNumenorean
    placementOf Enemies.blackNumenorean `shouldReturn` Just (EngagedWith pid)
    getDamage (toTarget numenorean) `shouldReturn` 3
    hiddenOf pid `shouldReturn` []

  it "returns an engaged enemy to the staging area with Lord Alcaron's Action" . bloodTest $ \pid -> do
    stage Objectives.lordAlcaron
    alcaron <- allyOf Objectives.lordAlcaron
    _ <- engaged pid Enemies.blackNumenorean
    run $ ActionWindow pid
    chooseAbilityOf pid alcaron
    placementOf Enemies.blackNumenorean `shouldReturn` Just StagingArea

  it "loses the game when Faramir leaves play" . bloodTest $ \_ -> do
    stage Objectives.faramirObjective
    faramir <- allyOf Objectives.faramirObjective
    run $ DealDamage GameSource (toTarget faramir) 4
    getsGame (.outcome) `shouldReturn` Just Lost

  -- \* Captured!

  it "puts Faramir and Lord Alcaron under Captured! and turns every hidden card faceup" . bloodTest $ \pid -> do
    stage Objectives.faramirObjective
    stage Objectives.lordAlcaron
    hide pid [Locations.southernRoad]
    quest <- putQuestStage Quests.captured
    run $ Flip quest
    resolveWhenRevealed pid
    placementOf Objectives.faramirObjective `shouldReturn` Just (AttachedTo quest)
    placementOf Objectives.lordAlcaron `shouldReturn` Just (AttachedTo quest)
    hiddenOf pid `shouldReturn` []
    -- in play, but no one's: archery cannot go to them
    captives <- traverse (fmap toTarget . allyOf) [Objectives.faramirObjective, Objectives.lordAlcaron]
    stage Locations.theDarkWoods
    run ResolveArchery
    choices <- maybe [] questionChoices <$> getQuestion pid
    [t | TargetLabel t _ <- choices] `shouldSatisfy` \ts -> notNull ts && all (`notElem` captives) ts

  it "turns a player's hidden cards faceup once he has 5 at Captured!" . bloodTest $ \pid -> do
    _ <- putQuestStage Quests.captured
    hide pid (replicate 4 Locations.southernRoad)
    hiddenOf pid >>= (`shouldSatisfy` ((== 4) . length))
    _ <- setEncounterDeck [Locations.northernRoad]
    run $ CheckWindows [mkAfter $ PhaseBegins QuestPhase]
    hiddenOf pid `shouldReturn` []
    length <$> getEncounterDiscard `shouldReturn` 5

  it "is won once Captured! is defeated" . bloodTest $ \_ -> do
    quest <- putQuestStage Quests.captured
    run $ PlaceTokens GameSource quest Progress 15
    getsGame (.outcome) `shouldReturn` Just Won
