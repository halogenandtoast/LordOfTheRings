module LordOfTheRings.Dwarrowdelf.TheWatcherInTheWaterSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character (getAttack, getDefense)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (PlayerMatcher (AnyPlayer))
import LordOfTheRings.Modifier (ModifierType (CannotBeOptionallyEngagedBy, CannotExhaust))
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theWatcherInTheWater)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (RoundEnds))
import TestImport

watcherTest :: (PlayerId -> GameT ()) -> IO ()
watcherTest = gameTestIn theWatcherInTheWater testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
      locations = [l.placement | l <- toList g.entities.locations, toCardCode l == code]
      enemies = [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
      treacheries = [t.placement | t <- toList g.entities.treacheries, toCardCode t == code]
  pure $ headMay (locations <> enemies <> treacheries)

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

setAsideCodes :: GameT [CardCode]
setAsideCodes = getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))

victoryDisplay :: GameT [CardCode]
victoryDisplay = getsGame (map toCardCode . (.victoryDisplay) . toAttrs . (.scenario))

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

-- | The enemy engaged with the player, the encounter deck then holding the cards given.
engaged :: PlayerId -> CardDef -> [CardDef] -> GameT EnemyId
engaged pid def deck = do
  run . AddToStaging =<< genEncounterCard def
  eid <- enemyOf def
  run $ EngageEnemy pid eid
  _ <- setEncounterDeck deck
  pure eid

-- | Beravor attacks the enemy alone.
beravorAttacks :: PlayerId -> EnemyId -> GameT ()
beravorAttacks pid eid = do
  run $ DeclareAttackers pid eid
  chooseTarget pid =<< getHeroWithCode "01012"
  chooseLabel "$label.doneAttacking" pid
  passActions pid

spec :: Spec
spec = describe "The Watcher in the Water" do
  it "is a scenario the game can be dealt" . watcherTest $ \_ ->
    isJust (lookupScenario "the-watcher-in-the-water") `shouldBe` True

  it "sets The Watcher and Doors of Durin aside at setup" . watcherTest $ \pid -> do
    flipFirstQuest pid
    aside <- setAsideCodes
    aside `shouldSatisfy` elem (toCardCode Enemies.theWatcher)
    aside `shouldSatisfy` elem (toCardCode Locations.doorsOfDurin)

  it "reveals until the staging area holds twice the players in threat" . watcherTest $ \_ -> do
    quest <- putQuestStage Quests.toTheWestDoor
    _ <- setEncounterDeck [Enemies.theWatcher, Locations.makeshiftPassage, Locations.doorsOfDurin, Locations.perilousSwamp, Locations.stairFalls]
    run $ Flip quest
    placementOf Locations.makeshiftPassage `shouldReturn` Just StagingArea
    placementOf Locations.perilousSwamp `shouldReturn` Just StagingArea
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Locations.stairFalls]
    aside <- setAsideCodes
    sort aside `shouldBe` sort [toCardCode Enemies.theWatcher, toCardCode Locations.doorsOfDurin]

  it "brings The Watcher and Doors of Durin in at The Seething Lake, and shuffles the tentacles back" . watcherTest $ \pid -> do
    quest <- putQuestStage Quests.theSeethingLake
    _ <- setEncounterDeck []
    run . SetAside =<< genEncounterCard Enemies.theWatcher
    run . SetAside =<< genEncounterCard Locations.doorsOfDurin
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.makeshiftPassage
    run . AddToEncounterDiscard . pure =<< genEncounterCard Enemies.graspingTentacle
    run $ Flip quest
    resolveWhenRevealed pid
    placementOf Enemies.theWatcher `shouldReturn` Just StagingArea
    placementOf Locations.doorsOfDurin `shouldReturn` Just ActiveLocation
    placementOf Locations.makeshiftPassage `shouldReturn` Just StagingArea
    setAsideCodes `shouldReturn` []
    getEncounterDiscard `shouldReturn` []
    map toCardCode <$> getEncounterDeck `shouldReturn` [toCardCode Enemies.graspingTentacle]

  it "places progress for Doors of Durin on the quest" . watcherTest $ \_ -> do
    _ <- putQuestStage Quests.theSeethingLake
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.doorsOfDurin
    doors <- locationOf Locations.doorsOfDurin
    run $ PlaceTokens GameSource (toTarget doors) Progress 2
    run $ PlaceProgressOnQuest 1
    (.progress) <$> getActiveQuest `shouldReturn` 3
    getsGame (fmap (.progress) . lookup doors . (.entities.locations)) `shouldReturn` Just 0

  it "opens the Doors of Durin when a discarded card shares the encounter card's first letter, and the players win" . watcherTest $ \pid -> do
    quest <- putQuestStage Quests.theSeethingLake
    run $ PlaceTokens GameSource quest Progress 5
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.doorsOfDurin
    doors <- locationOf Locations.doorsOfDurin
    card <- putCardIntoHand pid Events.sneakAttack
    _ <- setEncounterDeck [Locations.stairFalls]
    run $ ActionWindow pid
    chooseAbilityOf pid (LocationTarget doors)
    chooseTarget pid (CardIdTarget card.id)
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.stairFalls]
    victoryDisplay >>= (`shouldSatisfy` elem (toCardCode Locations.doorsOfDurin))
    getsGame (.outcome) `shouldReturn` Just Won

  it "keeps the Doors of Durin shut when no first letter matches" . watcherTest $ \pid -> do
    _ <- putQuestStage Quests.theSeethingLake
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.doorsOfDurin
    doors <- locationOf Locations.doorsOfDurin
    _ <- putCardIntoHand pid Events.sneakAttack
    _ <- setEncounterDeck [Locations.perilousSwamp]
    run $ ActionWindow pid
    chooseAbilityOf pid (LocationTarget doors)
    chooseLabel "$label.doneDiscarding" pid
    placementOf Locations.doorsOfDurin `shouldReturn` Just ActiveLocation

  it "attaches Grasping Tentacle to its attacker, whose attack and defense fall to 0" . watcherTest $ \pid -> do
    eid <- engaged pid Enemies.graspingTentacle [Enemies.strikingTentacle]
    beravorAttacks pid eid
    beravor <- toTarget <$> getHeroWithCode "01012"
    placementOf Enemies.graspingTentacle `shouldReturn` Just (AttachedTo beravor)
    getDamage (toTarget eid) `shouldReturn` 0
    getAttack beravor `shouldReturn` 0
    getDefense beravor `shouldReturn` 0
    -- it is no enemy in play any more
    run . AddToStaging =<< genEncounterCard Enemies.theWatcher
    watcher <- enemyOf Enemies.theWatcher
    getModifiers (toTarget watcher) >>= (`shouldSatisfy` notElem (CannotBeOptionallyEngagedBy AnyPlayer))

  it "is attacked as usual when the discarded card neither has a shadow nor is a Tentacle" . watcherTest $ \pid -> do
    eid <- engaged pid Enemies.graspingTentacle [Locations.makeshiftPassage]
    beravorAttacks pid eid
    placementOf Enemies.graspingTentacle `shouldReturn` Just (EngagedWith pid)
    getDamage (toTarget eid) `shouldReturn` 2

  it "deals the damage of an attack on Thrashing Tentacle to an attacking player's character" . watcherTest $ \pid -> do
    eid <- engaged pid Enemies.thrashingTentacle [Locations.stairFalls]
    beravorAttacks pid eid
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    getDamage eowyn `shouldReturn` 2
    getDamage (toTarget eid) `shouldReturn` 0

  it "makes Striking Tentacle's attack undefended" . watcherTest $ \pid -> do
    eid <- engaged pid Enemies.strikingTentacle [Locations.stairFalls]
    run $ EnemyAttack pid eid
    passActions pid
    Just (QuestionLabel prompt _ _ _) <- getQuestion pid
    prompt `shouldBe` "$label.undefendedDamage"

  it "cannot be optionally engaged while another Tentacle enemy is in play" . watcherTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Enemies.theWatcher
    watcher <- enemyOf Enemies.theWatcher
    getModifiers (toTarget watcher) >>= (`shouldSatisfy` notElem (CannotBeOptionallyEngagedBy AnyPlayer))
    run . AddToStaging =<< genEncounterCard Enemies.strikingTentacle
    getModifiers (toTarget watcher) >>= (`shouldSatisfy` elem (CannotBeOptionallyEngagedBy AnyPlayer))

  it "has each player deal 3 damage when The Watcher is in the staging area at the end of combat" . watcherTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Enemies.theWatcher
    run $ EndPhase CombatPhase
    beravor <- toTarget <$> getHeroWithCode "01012"
    chooseTarget pid beravor
    getDamage beravor `shouldReturn` 3

  it "takes at most 1 progress on Perilous Swamp each round, losing the rest" . watcherTest $ \_ -> do
    _ <- putQuestStage Quests.toTheWestDoor
    run . PutIntoPlayAsActiveLocation =<< genEncounterCard Locations.perilousSwamp
    swamp <- locationOf Locations.perilousSwamp
    run $ PlaceProgressOnQuest 5
    run $ PlaceProgressOnQuest 3
    getsGame (fmap (.progress) . lookup swamp . (.entities.locations)) `shouldReturn` Just 1
    (.progress) <$> getActiveQuest `shouldReturn` 0

  it "places 2 progress on the quest after traveling to Makeshift Passage" . watcherTest $ \_ -> do
    _ <- putQuestStage Quests.toTheWestDoor
    run . AddToStaging =<< genEncounterCard Locations.makeshiftPassage
    run . TravelTo =<< locationOf Locations.makeshiftPassage
    (.progress) <$> getActiveQuest `shouldReturn` 2

  it "brings a discarded Tentacle enemy in with Stagnant Creek and raises threat by 5" . watcherTest $ \pid -> do
    threat <- (.threat) <$> getPlayer pid
    reveal [Locations.stagnantCreek, Enemies.graspingTentacle]
    placementOf Enemies.graspingTentacle `shouldReturn` Just StagingArea
    ((.threat) <$> getPlayer pid) `shouldReturn` threat + 5

  it "engages the staging area's enemies with Ill Purpose, then raises threat by the staging threat" . watcherTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Enemies.thrashingTentacle
    run . AddToStaging =<< genEncounterCard Locations.perilousSwamp
    threat <- (.threat) <$> getPlayer pid
    reveal [Treacheries.illPurpose]
    placementOf Enemies.thrashingTentacle `shouldReturn` Just (EngagedWith pid)
    ((.threat) <$> getPlayer pid) `shouldReturn` threat + 4

  it "keeps a hero Wrapped! from exhausting, and discards the hero at the end of the round" . watcherTest $ \pid -> do
    reveal [Treacheries.wrapped]
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    placementOf Treacheries.wrapped `shouldReturn` Just (AttachedTo eowyn)
    getModifiers eowyn >>= (`shouldSatisfy` elem CannotExhaust)
    run $ CheckWindows [Window When RoundEnds]
    getsGame (any ((== "01007") . toCardCode) . toList . (.entities.heroes)) `shouldReturn` False

  it "discards Wrapped! as another hero exhausts in combat" . watcherTest $ \pid -> do
    reveal [Treacheries.wrapped]
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    wrappedId <- getsGame (fromJustNote "no Wrapped!" . fmap toTarget . headMay . toList . (.entities.treacheries))
    putGame . (phaseL .~ CombatPhase) =<< getGame
    run $ ActionWindow pid
    chooseAbilityOf pid wrappedId
    beravor <- getHeroWithCode "01012"
    chooseTarget pid beravor
    placementOf Treacheries.wrapped `shouldReturn` Nothing
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
