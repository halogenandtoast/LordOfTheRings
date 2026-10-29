module LordOfTheRings.RingMaker.CelebrimborsSecretSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Staging (getEnemyThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (celebrimborsSecret, lookupScenario)
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (cardsUnderOrcsSearch, triggerEachScour)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

csTest :: (PlayerId -> GameT ()) -> IO ()
csTest = gameTestIn celebrimborsSecret testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [o.placement | o <- toList g.entities.objectives, toCardCode o == code]

staged :: CardDef -> GameT ()
staged def = run . AddToStaging =<< genEncounterCard def

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

objectiveOf :: CardDef -> GameT ObjectiveId
objectiveOf def = getsGame (fromJustNote "no such objective" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.objectives))

-- | The location, staged then made active.
active :: CardDef -> GameT LocationId
active def = do
  staged def
  lid <- locationOf def
  run $ PlaceLocation lid ActiveLocation
  pure lid

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

damageOn :: LocationId -> GameT Int
damageOn lid = getDamage (toTarget lid)

spec :: Spec
spec = describe "Celebrimbor's Secret" do
  it "is a scenario the game can be dealt" . csTest $ \_ ->
    isJust (lookupScenario "celebrimbors-secret") `shouldBe` True

  it "stages Bellach, The Orcs' Search and The Secret Chamber with the Mould on it, and an Ost-in-Edhil location" . csTest $ \pid -> do
    flipFirstQuest pid
    placementOf Enemies.bellachCelebrimborsSecret `shouldReturn` Just StagingArea
    placementOf Objectives.theOrcsSearch `shouldReturn` Just StagingArea
    placementOf Locations.theSecretChamber `shouldReturn` Just StagingArea
    chamber <- locationOf Locations.theSecretChamber
    placementOf Objectives.celebrimborsMould `shouldReturn` Just (AttachedTo $ toTarget chamber)
    -- the player adds an Ost-in-Edhil location of their choice
    Just q <- getQuestion pid
    case [t | TargetLabel t _ <- questionChoices q] of
      t : _ -> chooseTarget pid t
      [] -> error "no Ost-in-Edhil location offered"
    staging <- getsGame (map toCardCode . filter ((== StagingArea) . (.placement)) . toList . (.entities.locations))
    length staging `shouldBe` 2
    deck <- map toCardCode <$> getEncounterDeck
    for_ [Enemies.bellachCelebrimborsSecret, Objectives.theOrcsSearch, Objectives.celebrimborsMould, Locations.theSecretChamber] \c ->
      deck `shouldSatisfy` notElem (toCardCode c)

  it "places a location with damage equal to its quest points underneath The Orcs' Search, raising Bellach's threat" . csTest $ \_ -> do
    staged Objectives.theOrcsSearch
    staged Enemies.bellachCelebrimborsSecret
    bellach <- enemyOf Enemies.bellachCelebrimborsSecret
    lid <- active Locations.cityRemains
    run $ PlaceTokens GameSource (toTarget lid) Damage 2
    run $ CheckDefeated (toTarget lid)
    placementOf Locations.cityRemains `shouldReturn` Just ActiveLocation
    run $ PlaceTokens GameSource (toTarget lid) Damage 1
    run $ CheckDefeated (toTarget lid)
    placementOf Locations.cityRemains `shouldReturn` Nothing
    cardsUnderOrcsSearch `shouldReturn` 1
    getEnemyThreat bellach `shouldReturn` 1

  it "raises each player's threat by the cards underneath The Orcs' Search at the end of the refresh phase" . csTest $ \pid -> do
    staged Objectives.theOrcsSearch
    oid <- objectiveOf Objectives.theOrcsSearch
    card <- putCardIntoHand pid Treacheries.discovered
    run $ PlaceUnderneath (toTarget oid) [card]
    threat <- (.threat) <$> getPlayer pid
    run $ EndPhase RefreshPhase
    (.threat) <$> getPlayer pid `shouldReturn` threat + 1

  it "is lost once The Secret Chamber is placed underneath The Orcs' Search" . csTest $ \_ -> do
    staged Objectives.theOrcsSearch
    staged Locations.theSecretChamber
    lid <- locationOf Locations.theSecretChamber
    run $ PlaceTokens GameSource (toTarget lid) Damage 6
    run $ CheckDefeated (toTarget lid)
    outcome `shouldReturn` Just Lost

  it "gives The Secret Chamber's Mould to a hero of the first player as it is explored, and loses if that hero leaves play" . csTest $ \pid -> do
    staged Locations.theSecretChamber
    lid <- locationOf Locations.theSecretChamber
    run . (`PutObjectiveIntoPlayAttached` toTarget lid) =<< genEncounterCard Objectives.celebrimborsMould
    run $ PlaceLocation lid ActiveLocation
    run $ PlaceTokens GameSource (toTarget lid) Progress 6
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    placementOf Objectives.celebrimborsMould `shouldReturn` Just (AttachedTo eowyn)
    outcome `shouldReturn` Nothing
    run $ Destroy eowyn
    outcome `shouldReturn` Just Lost

  it "keeps Bellach from being engaged on the first stage" . csTest $ \pid -> do
    _ <- putQuestStage Quests.theRuinsOfOstInEdhil
    staged Enemies.bellachCelebrimborsSecret
    run $ OptionalEngagement pid
    getQuestion pid `shouldReturn` Nothing

  it "triggers each Scour effect in play: Bellach's Scout damages the active location, Collapsed Tower returns to the staging area" . csTest $ \pid -> do
    staged Objectives.theOrcsSearch
    tower <- active Locations.collapsedTower
    staged Enemies.bellachsScout
    runQueueT triggerEachScour >> void mainLoop
    -- the first player orders them: the Scout first, then the Tower
    scout <- enemyOf Enemies.bellachsScout
    chooseTarget pid scout
    damageOn tower `shouldReturn` 1
    chooseTarget pid tower
    placementOf Locations.collapsedTower `shouldReturn` Just StagingArea
    damageOn tower `shouldReturn` 3

  it "assigns Ancient Foundation's Scour damage among the locations, one at a time" . csTest $ \pid -> do
    staged Objectives.theOrcsSearch
    staged Locations.ancientFoundation
    staged Locations.ancientFoundation
    foundations <- getsGame (map toId . toList . (.entities.locations))
    runQueueT triggerEachScour >> void mainLoop
    -- the copies do not stack: 2 damage, X the 2 locations in play
    case foundations of
      [a, b] -> do
        chooseTarget pid a
        chooseTarget pid b
        damageOn a `shouldReturn` 1
        damageOn b `shouldReturn` 1
        getQuestion pid `shouldReturn` Nothing
      _ -> error "two foundations"

  it "triggers the topmost Scour in the discard pile as progress is placed on City Remains" . csTest $ \_ -> do
    staged Objectives.theOrcsSearch
    remains <- active Locations.cityRemains
    tower <- genEncounterCard Locations.collapsedTower
    scout <- genEncounterCard Enemies.bellachsScout
    run $ AddToEncounterDiscard [tower, scout]
    run $ PlaceTokens GameSource (toTarget remains) Progress 1
    -- Collapsed Tower, on top, returns from the discard pile with 2 damage
    placementOf Locations.collapsedTower `shouldReturn` Just StagingArea
    flip shouldReturn 2 . damageOn =<< locationOf Locations.collapsedTower
    damageOn remains `shouldReturn` 0

  it "places 1 damage on Ruined Plaza as it enters the staging area" . csTest $ \_ -> do
    staged Objectives.theOrcsSearch
    staged Locations.ruinedPlaza
    flip shouldReturn 1 . damageOn =<< locationOf Locations.ruinedPlaza

  it "surges with Desecrated Ruins when there is no active location" . csTest $ \_ -> do
    staged Objectives.theOrcsSearch
    reveal [Treacheries.desecratedRuins, Locations.ancientFoundation]
    placementOf Locations.ancientFoundation `shouldReturn` Just StagingArea

  it "places 3 damage on the active location with Desecrated Ruins" . csTest $ \_ -> do
    staged Objectives.theOrcsSearch
    lid <- active Locations.ancientFoundation
    reveal [Treacheries.desecratedRuins]
    damageOn lid `shouldReturn` 3

  it "places a random card from each hand underneath The Orcs' Search with Discovered!" . csTest $ \pid -> do
    staged Objectives.theOrcsSearch
    staged Enemies.bellachsScout
    _ <- putCardIntoHand pid Treacheries.discovered
    hand <- length . (.hand) <$> getPlayer pid
    reveal [Treacheries.discovered]
    chooseLabel "$label.discovered.hand" pid
    cardsUnderOrcsSearch `shouldReturn` 1
    length . (.hand) <$> getPlayer pid `shouldReturn` hand - 1

  it "surges with Spies from Mordor when no card is underneath The Orcs' Search" . csTest $ \_ -> do
    staged Objectives.theOrcsSearch
    reveal [Treacheries.spiesFromMordor, Locations.ancientFoundation]
    placementOf Locations.ancientFoundation `shouldReturn` Just StagingArea

  it "has Bellach take the Mould on The Enemy's Servant, and wins once he is defeated and the stage has its progress" . csTest $ \pid -> do
    staged Objectives.theOrcsSearch
    staged Enemies.bellachCelebrimborsSecret
    bellach <- enemyOf Enemies.bellachCelebrimborsSecret
    eowyn <- toTarget <$> getHeroWithCode "01007"
    run . (`PutObjectiveIntoPlayAttached` eowyn) =<< genEncounterCard Objectives.celebrimborsMould
    _ <- setEncounterDeck [Treacheries.desecratedRuins]
    stage <- putQuestStage Quests.theEnemysServant
    run $ Flip stage
    resolveWhenRevealed pid
    -- Bellach's Scour, the only one in play: the search finds no Orc
    chooseLabel "$label.continue" pid
    -- Bellach attacks the Mould's player: undefended, its damage on Beravor
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid . toTarget =<< getHeroWithCode "01012"
    passActions pid
    placementOf Objectives.celebrimborsMould `shouldReturn` Just (AttachedTo $ toTarget bellach)
    -- immune and unengageable until the stage has 12 progress
    run $ PlaceTokens GameSource stage Progress 12
    outcome `shouldReturn` Nothing
    run $ DealDamage GameSource (toTarget bellach) 7
    chooseTarget pid eowyn
    outcome `shouldReturn` Just Won

  it "triggers each Scour as the last time counter leaves the first stage, then places 3 more" . csTest $ \_ -> do
    staged Objectives.theOrcsSearch
    lid <- active Locations.ancientFoundation
    stage <- putQuestStage Quests.theRuinsOfOstInEdhil
    staged Enemies.bellachsScout
    run $ PlaceTokens GameSource stage TimeCounter 1
    run $ RemoveTokens GameSource stage TimeCounter 1
    -- Ancient Foundation's and the Scout's: the first player orders them
    lead <- getLead
    scout <- enemyOf Enemies.bellachsScout
    chooseTarget lead scout
    chooseTarget lead lid
    chooseTarget lead lid
    damageOn lid `shouldReturn` 2
    getsGame (fmap (countTokens TimeCounter . questTokens . toAttrs) . headMay . toList . (.entities.quests)) `shouldReturn` Just 3

  it "lets Bellach be engaged once The Enemy's Servant has 12 progress" . csTest $ \pid -> do
    stage <- putQuestStage Quests.theEnemysServant
    staged Enemies.bellachCelebrimborsSecret
    bellach <- enemyOf Enemies.bellachCelebrimborsSecret
    run $ OptionalEngagement pid
    getQuestion pid `shouldReturn` Nothing
    run $ PlaceTokens GameSource stage Progress 12
    run $ OptionalEngagement pid
    chooseTarget pid bellach
    placementOf Enemies.bellachCelebrimborsSecret `shouldReturn` Just (EngagedWith pid)
