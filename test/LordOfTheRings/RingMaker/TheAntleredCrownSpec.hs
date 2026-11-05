module LordOfTheRings.RingMaker.TheAntleredCrownSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyAttack, getLocationQuestPoints, getStagingThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getTimeCounters)
import LordOfTheRings.Modifier (ModifierType (DoesNotExhaustToDefend, ImmuneToPlayerCardEffects))
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theAntleredCrown)
import LordOfTheRings.Scenario.Scenarios.TheAntleredCrown.Helpers (Meta (..), getMeta)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside), metaL, setAsideL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (When), Window (..), WindowType (RoundEnds))
import TestImport

crownTest :: (PlayerId -> GameT ()) -> IO ()
crownTest = gameTestIn theAntleredCrown testDeck

-- | The Raven deck, top first, made of these cards, with an empty discard pile.
setRavenDeck :: [CardDef] -> GameT [EncounterCard]
setRavenDeck defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (metaL .~ toJSON (Meta cards [] (map (.id) cards)))
  pure cards

raven :: GameT Meta
raven = getsGame (getMeta . toAttrs . (.scenario))

codes :: [EncounterCard] -> [CardCode]
codes = map toCardCode

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

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

activeAt :: CardDef -> GameT LocationId
activeAt def = do
  run . PutIntoPlayAsActiveLocation =<< genEncounterCard def
  locationOf def

counters :: Targetable a => a -> GameT Int
counters = getTimeCounters . toTarget

-- | Remove the card's time counters down to none.
runOut :: Targetable a => a -> GameT ()
runOut a = do
  n <- counters a
  run $ RemoveTokens GameSource (toTarget a) TimeCounter n

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

spec :: Spec
spec = describe "The Antlered Crown" do
  it "is a scenario the game can be dealt" . crownTest $ \_ ->
    isJust (lookupScenario "the-antlered-crown") `shouldBe` True

  it "sets up Chief Turch, the active Dunland Battlefield, a Raven War-camp and an enemy chosen from the Raven deck" . crownTest $ \pid -> do
    run Setup
    chooseLabel "$label.keepHand" pid
    _ <- setEncounterDeck [Objectives.chiefTurchTheAntleredCrown, Enemies.ravenChief, Locations.ravenChiefsCamp, Locations.dunlandBattlefield, Locations.ravenWarCamp, Locations.ravenWarCamp]
    [_, skirmisher] <- setRavenDeck [Enemies.ravenWarrior, Enemies.ravenSkirmisher]
    chooseOptionMatching "flip the quest" pid (const True)
    chooseTarget pid skirmisher
    placementOf Objectives.chiefTurchTheAntleredCrown `shouldReturn` Just (InPlayArea pid)
    placementOf Locations.dunlandBattlefield `shouldReturn` Just ActiveLocation
    placementOf Locations.ravenWarCamp `shouldReturn` Just StagingArea
    placementOf Enemies.ravenSkirmisher `shouldReturn` Just StagingArea
    codes . (.ravenDeck) <$> raven `shouldReturn` [toCardCode Enemies.ravenWarrior]
    getsGame (codes . scenarioSetAside . toAttrs . (.scenario))
      >>= (`shouldSatisfy` \cs -> all (`elem` cs) [toCardCode Enemies.ravenChief, toCardCode Locations.ravenChiefsCamp])
    codes <$> getEncounterDeck `shouldReturn` [toCardCode Locations.ravenWarCamp]
    (counters =<< getActiveQuest) `shouldReturn` 3
    (counters =<< locationOf Locations.dunlandBattlefield) `shouldReturn` 3

  -- \* The Raven deck

  it "sends an enemy from the Raven deck to its own discard pile, shuffled back in once the deck is empty" . crownTest $ \pid -> do
    _ <- setRavenDeck [Enemies.ravenWarrior]
    _ <- activeAt Locations.ravenVillage
    village <- locationOf Locations.ravenVillage
    runOut village
    placementOf Enemies.ravenWarrior `shouldReturn` Just StagingArea
    warrior <- enemyOf Enemies.ravenWarrior
    run $ EngageEnemy pid warrior
    run $ DealDamage GameSource (toTarget warrior) 5
    getEncounterDiscard `shouldReturn` []
    -- the next message sweeps it to the Raven deck's discard pile
    run $ CheckWindows []
    codes . (.ravenDiscard) <$> raven `shouldReturn` [toCardCode Enemies.ravenWarrior]
    run . AddToStaging =<< genEncounterCard Locations.ravenWarCamp
    runOut =<< locationOf Locations.ravenWarCamp
    placementOf Enemies.ravenWarrior `shouldReturn` Just (EngagedWith pid)
    (.ravenDiscard) <$> raven `shouldReturn` []

  it "keeps encounter cards in the encounter discard pile" . crownTest $ \_ -> do
    _ <- setRavenDeck []
    card <- genEncounterCard Enemies.ravenWarrior
    run $ AddToEncounterDiscard [card]
    run $ CheckWindows []
    codes <$> getEncounterDiscard `shouldReturn` [toCardCode Enemies.ravenWarrior]

  it "shuffles the Raven deck discard pile in and stages a card for each player at The Raven Clan" . crownTest $ \pid -> do
    [warrior] <- setRavenDeck [Enemies.ravenWarrior]
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (metaL .~ toJSON (Meta [] [warrior] [warrior.id]))
    q <- putQuestStage Quests.theRavenClan
    run $ Flip q
    resolveWhenRevealed pid
    placementOf Enemies.ravenWarrior `shouldReturn` Just StagingArea
    counters q `shouldReturn` 3
    staged Locations.ravenVillage
    (getLocationQuestPoints =<< locationOf Locations.ravenVillage) `shouldReturn` 5

  it "deals 2 more shadow cards from the Raven deck with Raven Village's shadow" . crownTest $ \pid -> do
    _ <- setRavenDeck [Enemies.ravenWarrior, Enemies.ravenSkirmisher]
    staged Enemies.ravenWarrior
    attacker <- enemyOf Enemies.ravenWarrior
    run $ EngageEnemy pid attacker
    _ <- setEncounterDeck [Locations.ravenVillage]
    run $ DealShadowCard attacker
    run $ EnemyAttack pid attacker
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    (.ravenDeck) <$> raven `shouldReturn` []

  -- \* Time X

  it "removes a time counter from each location as Battle for Dunland's last one goes, and resets" . crownTest $ \_ -> do
    q <- putQuestStage Quests.battleForDunland
    run $ Flip q
    battlefield <- activeAt Locations.dunlandBattlefield
    counters battlefield `shouldReturn` 3
    runOut q
    counters battlefield `shouldReturn` 2
    counters q `shouldReturn` 3

  it "reveals the top card of the Raven deck as Raven Village's last time counter goes" . crownTest $ \_ -> do
    _ <- setRavenDeck [Enemies.ravenWarrior]
    staged Locations.ravenVillage
    runOut =<< locationOf Locations.ravenVillage
    placementOf Enemies.ravenWarrior `shouldReturn` Just StagingArea

  it "has each player assign damage for the cards in hand as Dunland Battlefield's last time counter goes" . crownTest $ \pid -> do
    battlefield <- activeAt Locations.dunlandBattlefield
    hand <- length . (.hand) <$> getPlayer pid
    replicateM_ (3 - hand) $ putCardIntoHand pid Enemies.ravenWarrior
    runOut battlefield
    beravor <- toTarget <$> getHeroWithCode "01012"
    replicateM_ 3 $ chooseTarget pid beravor
    getDamage beravor `shouldReturn` 3

  it "raises threat by the cards in hand as Raven Country's last time counter goes" . crownTest $ \pid -> do
    country <- activeAt Locations.ravenCountry
    hand <- length . (.hand) <$> getPlayer pid
    threat <- (.threat) <$> getPlayer pid
    runOut country
    (.threat) <$> getPlayer pid `shouldReturn` threat + hand

  it "exhausts each damaged character and resets as Raven Chief's Camp's last time counter goes" . crownTest $ \_ -> do
    staged Locations.ravenChiefsCamp
    camp <- locationOf Locations.ravenChiefsCamp
    eowyn <- getHeroWithCode "01007"
    run $ DealDamage GameSource (toTarget eowyn) 1
    runOut camp
    heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` False
    counters camp `shouldReturn` 3

  it "removes a counter from each location, then refills those left with none, with Raising the Cry" . crownTest $ \_ -> do
    village <- activeAt Locations.ravenVillage
    run $ RemoveTokens GameSource (toTarget village) TimeCounter 1
    staged Locations.ravenCountry
    country <- locationOf Locations.ravenCountry
    _ <- setRavenDeck [Enemies.ravenWarrior]
    reveal [Treacheries.raisingTheCry]
    -- Raven Village ran out, revealed the Raven card, and is refilled to its Time 2
    counters village `shouldReturn` 2
    placementOf Enemies.ravenWarrior `shouldReturn` Just StagingArea
    counters country `shouldReturn` 3

  it "surges with Raising the Cry when no location is in the staging area" . crownTest $ \_ -> do
    reveal [Treacheries.raisingTheCry, Locations.ravenVillage]
    placementOf Locations.ravenVillage `shouldReturn` Just StagingArea

  -- \* Enemies

  it "cannot damage Raven Chief while his camp is in play, and his attack removes a counter from the active location" . crownTest $ \pid -> do
    staged Enemies.ravenChief
    chief <- enemyOf Enemies.ravenChief
    staged Locations.ravenChiefsCamp
    run $ DealDamage GameSource (toTarget chief) 3
    getDamage (toTarget chief) `shouldReturn` 0
    run . Discard GameSource . toTarget =<< locationOf Locations.ravenChiefsCamp
    run $ DealDamage GameSource (toTarget chief) 3
    getDamage (toTarget chief) `shouldReturn` 3
    battlefield <- activeAt Locations.dunlandBattlefield
    run $ EngageEnemy pid chief
    run $ EnemyAttack pid chief
    counters battlefield `shouldReturn` 2

  it "gives Raven Skirmisher's shadow the active location's Time X" . crownTest $ \pid -> do
    _ <- activeAt Locations.ravenCountry
    staged Enemies.ravenWarrior
    warrior <- enemyOf Enemies.ravenWarrior
    run $ EngageEnemy pid warrior
    _ <- setEncounterDeck [Enemies.ravenSkirmisher]
    run $ DealShadowCard warrior
    run $ EnemyAttack pid warrior
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    getEnemyAttack warrior `shouldReturn` 8

  it "lets the first player remove a counter from the active location or reveal another card with Raven Skirmisher" . crownTest $ \pid -> do
    battlefield <- activeAt Locations.dunlandBattlefield
    reveal [Enemies.ravenSkirmisher, Locations.ravenVillage]
    chooseLabel "$label.ravenSkirmisher.removeTimeCounter" pid
    counters battlefield `shouldReturn` 2
    getEncounterDeck >>= (`shouldSatisfy` (== [toCardCode Locations.ravenVillage]) . codes)

  it "has Raven Warrior remove time counters for the cards in the engaged player's hand" . crownTest $ \pid -> do
    battlefield <- activeAt Locations.dunlandBattlefield
    hand <- length . (.hand) <$> getPlayer pid
    replicateM_ (2 - hand) $ putCardIntoHand pid Enemies.ravenWarrior
    staged Enemies.ravenWarrior
    run . EngageEnemy pid =<< enemyOf Enemies.ravenWarrior
    counters battlefield `shouldReturn` 1

  -- \* Treacheries

  it "turns the progress in the staging area into threat with Driven Back, surging when there is none" . crownTest $ \_ -> do
    staged Locations.ravenVillage
    village <- locationOf Locations.ravenVillage
    run $ PlaceTokens GameSource (toTarget village) Progress 2
    threat <- getStagingThreat
    reveal [Treacheries.drivenBack]
    (.progress) <$> getLocation village `shouldReturn` 0
    getStagingThreat `shouldReturn` threat + 2
    reveal [Treacheries.drivenBack, Locations.ravenCountry]
    placementOf Locations.ravenCountry `shouldReturn` Just StagingArea

  it "draws 3 cards for each player with Fierce Folk" . crownTest $ \pid -> do
    hand <- length . (.hand) <$> getPlayer pid
    threat <- (.threat) <$> getPlayer pid
    reveal [Treacheries.fierceFolk]
    length . (.hand) <$> getPlayer pid `shouldReturn` hand + 3
    (.threat) <$> getPlayer pid `shouldReturn` threat + 3

  -- \* Chief Turch and the end

  it "makes Chief Turch immune to player card effects, defending without exhausting, and loses as he leaves play" . crownTest $ \pid -> do
    staged Objectives.chiefTurchTheAntleredCrown
    turch <- getsGame (fromJustNote "no Turch" . fmap toTarget . headMay . toList . (.entities.allies))
    placementOf Objectives.chiefTurchTheAntleredCrown `shouldReturn` Just (InPlayArea pid)
    run $ CheckWindows []
    mods <- getModifiers turch
    mods `shouldSatisfy` \ms -> ImmuneToPlayerCardEffects `elem` ms && DoesNotExhaustToDefend `elem` ms
    run $ Discard GameSource turch
    outcome `shouldReturn` Just Lost

  it "adds Raven Chief, his camp and the Raven cards at The Last Stage, and wins at the end of the round once he is in the victory display" . crownTest $ \pid -> do
    chief <- genEncounterCard Enemies.ravenChief
    camp <- genEncounterCard Locations.ravenChiefsCamp
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (setAsideL .~ [chief, camp])
    q <- putQuestStage Quests.theLastStage
    run $ Flip q
    resolveWhenRevealed pid
    placementOf Enemies.ravenChief `shouldReturn` Just StagingArea
    placementOf Locations.ravenChiefsCamp `shouldReturn` Just StagingArea
    counters q `shouldReturn` 2
    run $ CheckWindows [Window When RoundEnds]
    outcome `shouldReturn` Nothing
    run . AddToVictoryDisplay . toTarget =<< enemyOf Enemies.ravenChief
    run $ CheckWindows [Window When RoundEnds]
    outcome `shouldReturn` Just Won

  it "has each engaged enemy attack as The Last Stage's last time counter goes" . crownTest $ \pid -> do
    q <- putQuestStage Quests.theLastStage
    run $ Flip q
    resolveWhenRevealed pid
    staged Enemies.ravenWarrior
    run . EngageEnemy pid =<< enemyOf Enemies.ravenWarrior
    _ <- setEncounterDeck [Locations.ravenVillage]
    runOut q
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid . toTarget =<< getHeroWithCode "01012"
    passActions pid
    counters q `shouldReturn` 2
