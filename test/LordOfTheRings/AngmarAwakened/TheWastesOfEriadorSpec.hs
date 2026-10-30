module LordOfTheRings.AngmarAwakened.TheWastesOfEriadorSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost, Won))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (lookupScenario, theWastesOfEriador)
import LordOfTheRings.Scenario.Scenarios.TheWastesOfEriador.Helpers (isDay, isNight)
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (RoundEnds))
import TestImport

wastesTest :: (PlayerId -> GameT ()) -> IO ()
wastesTest = gameTestIn theWastesOfEriador testDeck

placementOf :: CardDef -> GameT (Maybe Placement)
placementOf def = do
  g <- getGame
  let code = toCardCode def
  pure
    $ headMay
    $ [l.placement | l <- toList g.entities.locations, toCardCode l == code]
    <> [e.placement | e <- toList g.entities.enemies, toCardCode e == code]
    <> [a.placement | a <- toList g.entities.allies, toCardCode a == code]
    <> [o.placement | o <- toList g.entities.objectives, toCardCode o == code]

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

locationOf :: CardDef -> GameT LocationId
locationOf def = getsGame (fromJustNote "no such location" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.locations))

staged :: CardDef -> GameT EnemyId
staged def = do
  run . AddToStaging =<< genEncounterCard def
  enemyOf def

stagedLocation :: CardDef -> GameT LocationId
stagedLocation def = do
  run . AddToStaging =<< genEncounterCard def
  locationOf def

-- | Put the Time objective into the staging area, the side faceup.
timeShowing :: CardDef -> GameT ()
timeShowing side = run . AddToStaging =<< genEncounterCard side

timeObjective :: GameT ObjectiveId
timeObjective = getsGame (fromJustNote "no Time objective" . fmap toId . headMay . toList . (.entities.objectives))

flipTime :: GameT ()
flipTime = run . Flip . toTarget =<< timeObjective

reveal :: [CardDef] -> GameT ()
reveal defs = do
  _ <- setEncounterDeck defs
  run RevealEncounterCard
  resolveRevealed

threatOf :: PlayerId -> GameT Int
threatOf pid = (.threat) <$> getPlayer pid

outcome :: GameT (Maybe Outcome)
outcome = getsGame (.outcome)

-- | Use every forced ability offered, one at a time, then pass the responses offered.
resolveForced :: PlayerId -> GameT ()
resolveForced pid =
  getQuestion pid >>= \case
    Just (ChooseOneAtATime (AbilityLabel {} : _)) -> chooseAbility pid >> resolveForced pid
    Just (QuestionLabel "$label.useResponse" _ _ _) -> chooseLabel "$label.pass" pid >> resolveForced pid
    _ -> pure ()

-- | The scenario's setup, with the encounter deck holding only the cards it searches for
-- and those given, revealed 1 for the player.
setUp :: PlayerId -> [CardDef] -> GameT ()
setUp pid rest = do
  run Setup
  chooseLabel "$label.keepHand" pid
  _ <- setEncounterDeck ([Enemies.packLeader, Objectives.daybreak, Objectives.amarthiul] <> rest)
  chooseOptionMatching "flip the quest" pid (const True)

-- | On to the stage, its When Revealed resolved.
advanceTo :: PlayerId -> [CardDef] -> GameT ()
advanceTo pid deck = do
  _ <- setEncounterDeck deck
  run . CompleteQuest . toId =<< getActiveQuest
  resolveWhenRevealed pid

spec :: Spec
spec = describe "The Wastes of Eriador" do
  it "is a scenario the game can be dealt" . wastesTest $ \_ ->
    isJust (lookupScenario "the-wastes-of-eriador") `shouldBe` True

  it "sets Pack Leader aside, stages Daybreak, gives Amarthiúl to the first player and reveals a card" . wastesTest $ \pid -> do
    setUp pid [Locations.northDowns]
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    map toCardCode aside `shouldSatisfy` elem (toCardCode Enemies.packLeader)
    placementOf Objectives.daybreak `shouldReturn` Just StagingArea
    placementOf Objectives.amarthiul `shouldReturn` Just (InPlayArea pid)
    placementOf Locations.northDowns `shouldReturn` Just StagingArea
    isDay `shouldReturn` True
    (.flipped) <$> getActiveQuest `shouldReturn` True

  it "gives heroes +1 willpower and stops engagement checks while it is Day" . wastesTest $ \pid -> do
    setUp pid [Locations.northDowns]
    beravor <- getHeroWithCode "01012"
    getModifiers (toTarget beravor) >>= (`shouldSatisfy` elem (WillpowerModifier 1))
    getModifiers GameTarget >>= (`shouldSatisfy` elem NoEngagementChecks)
    warg <- staged Enemies.whiteWarg
    run $ EngagementCheck pid 0
    (.placement) <$> getEnemy warg `shouldReturn` StagingArea

  it "turns to Night at the end of the round, revealing a card and raising each player's threat by 1" . wastesTest $ \pid -> do
    setUp pid [Locations.northDowns]
    threat <- threatOf pid
    _ <- setEncounterDeck [Locations.eriadorWastes]
    run $ CheckWindows [Window After RoundEnds]
    resolveForced pid
    isNight `shouldReturn` True
    threatOf pid `shouldReturn` threat + 1
    placementOf Locations.eriadorWastes `shouldReturn` Just StagingArea
    getModifiers GameTarget >>= (`shouldSatisfy` elem CannotCancelEncounterEffects)
    q <- toTarget <$> getActiveQuest
    run $ PlaceTokens GameSource q Progress 3
    (.progress) <$> getActiveQuest `shouldReturn` 0

  it "returns each engaged enemy to the staging area as it becomes Day" . wastesTest $ \pid -> do
    timeShowing Objectives.nightfall
    warg <- staged Enemies.northernWarg
    run $ EngageEnemy pid warg
    run $ CheckWindows [Window After RoundEnds]
    resolveForced pid
    isDay `shouldReturn` True
    (.placement) <$> getEnemy warg `shouldReturn` StagingArea

  it "has Blood-thirsty Warg in the staging area engage the first player as it becomes Night" . wastesTest $ \pid -> do
    timeShowing Objectives.daybreak
    warg <- staged Enemies.bloodThirstyWarg
    _ <- setEncounterDeck [Locations.northDowns, Locations.northDowns, Locations.northDowns]
    flipTime
    resolveForced pid
    (.placement) <$> getEnemy warg `shouldReturn` EngagedWith pid

  it "lowers Hunting Pack's engagement cost by each damaged character the checking player controls" . wastesTest $ \pid -> do
    hunters <- staged Enemies.huntingPack
    threat <- threatOf pid
    run $ RaiseThreat pid (38 - threat)
    run $ EngagementCheck pid 0
    (.placement) <$> getEnemy hunters `shouldReturn` StagingArea
    for_ ["01012", "01007"] \code -> do
      h <- getHeroWithCode code
      run $ PlaceTokens GameSource (toTarget h) Damage 1
    run $ EngagementCheck pid 0
    resolveForced pid
    (.placement) <$> getEnemy hunters `shouldReturn` EngagedWith pid

  it "has White Warg deal 2 damage to a character as it engages at Night" . wastesTest $ \pid -> do
    timeShowing Objectives.nightfall
    warg <- staged Enemies.whiteWarg
    run $ EngageEnemy pid warg
    beravor <- getHeroWithCode "01012"
    chooseTarget pid beravor
    resolveForced pid
    getDamage (toTarget beravor) `shouldReturn` 2

  it "has Pack Leader exhaust a character for each Warg engaged with the player" . wastesTest $ \pid -> do
    warg <- staged Enemies.northernWarg
    run $ EngageEnemy pid warg
    leader <- staged Enemies.packLeader
    run $ EngageEnemy pid leader
    for_ ["01012", "01007"] \code -> chooseTarget pid =<< getHeroWithCode code
    resolveForced pid
    exhausted <- getsGame (length . filter (heroExhausted . toAttrs) . toList . (.entities.heroes))
    exhausted `shouldBe` 2

  it "gives each Warg +1 threat for the phase as Northern Warg is revealed" . wastesTest $ \_ -> do
    white <- staged Enemies.whiteWarg
    reveal [Enemies.northernWarg]
    getModifiers (toTarget white) >>= (`shouldSatisfy` elem (ThreatModifier 1))

  it "gives Wolf of Angmar +2 attack and North Downs +2 threat while it is Night" . wastesTest $ \_ -> do
    wolf <- staged Enemies.wolfOfAngmar
    downs <- stagedLocation Locations.northDowns
    getLocationThreat downs `shouldReturn` 1
    timeShowing Objectives.nightfall
    getModifiers (toTarget wolf) >>= (`shouldSatisfy` elem (AttackModifier 2))
    getLocationThreat downs `shouldReturn` 3

  it "limits the current quest to 5 progress a round while Eriador Wastes is in the staging area" . wastesTest $ \_ -> do
    q <- putQuestStage Quests.acrossTheWastes
    _ <- stagedLocation Locations.eriadorWastes
    run $ PlaceTokens GameSource q Progress 7
    (.progress) <$> getActiveQuest `shouldReturn` 5

  it "has the first player engage a staged Warg and deal it a shadow card to travel to Eriador Wastes" . wastesTest $ \pid -> do
    wastes <- stagedLocation Locations.eriadorWastes
    getModifiers (toTarget wastes) >>= (`shouldSatisfy` elem CannotBeTraveledTo)
    warg <- staged Enemies.whiteWarg
    getModifiers (toTarget wastes) >>= (`shouldSatisfy` notElem CannotBeTraveledTo)
    _ <- setEncounterDeck [Locations.northDowns]
    run $ TravelTo wastes
    beravor <- getHeroWithCode "01012"
    chooseTarget pid beravor
    resolveForced pid
    e <- getEnemy warg
    e.placement `shouldBe` EngagedWith pid
    length e.shadowCards `shouldBe` 1

  it "puts a Warg from the encounter deck into play engaged with the first player to travel to Warg's Den" . wastesTest $ \pid -> do
    den <- stagedLocation Locations.wargsDen
    [warg] <- setEncounterDeck [Enemies.wolfOfAngmar]
    run $ TravelTo den
    chooseTarget pid (CardIdTarget warg.id)
    resolveForced pid
    placementOf Enemies.wolfOfAngmar `shouldReturn` Just (EngagedWith pid)

  it "has Sudden Darkness turn Day to Night, and raise each player's threat by 4 at Night" . wastesTest $ \pid -> do
    timeShowing Objectives.daybreak
    reveal [Treacheries.suddenDarkness]
    resolveForced pid
    isNight `shouldReturn` True
    threat <- threatOf pid
    reveal [Treacheries.suddenDarkness]
    threatOf pid `shouldReturn` threat + 4

  it "has Predatory Wolves reveal a Warg into the staging area for a player who searches" . wastesTest $ \pid -> do
    _ <- setEncounterDeck [Treacheries.predatoryWolves, Enemies.huntingPack]
    run RevealEncounterCard
    resolveRevealed
    -- with no ally to discard, the player searches
    hunters <- getEncounterDeck
    for_ (headMay hunters) \c -> chooseTarget pid (CardIdTarget c.id)
    placementOf Enemies.huntingPack `shouldReturn` Just StagingArea

  it "discards the allies Howling at Night reveals Wargs among, the others left in the discard pile" . wastesTest $ \pid -> do
    setUp pid [Locations.northDowns]
    advanceTo pid [Treacheries.predatoryWolves, Enemies.northernWarg]
    placementOf Enemies.northernWarg `shouldReturn` Just StagingArea
    toCardCode <$> getActiveQuest `shouldReturn` toCardCode Quests.howlingAtNight

  it "has Howling at Night discard an ally as it becomes Night, and draw a card as it becomes Day" . wastesTest $ \pid -> do
    setUp pid [Locations.northDowns]
    advanceTo pid []
    guardian <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    _ <- setEncounterDeck [Locations.northDowns]
    hand <- length . (.hand) <$> getPlayer pid
    flipTime
    resolveForced pid
    -- the objective ally is not discarded
    placementOf Objectives.amarthiul `shouldReturn` Just (InPlayArea pid)
    allies <- getsGame (map toTarget . toList . (.entities.allies))
    allies `shouldSatisfy` notElem guardian
    flipTime
    resolveForced pid
    length . (.hand) <$> getPlayer pid `shouldReturn` hand + 1

  it "turns to Night and stages Pack Leader as Battle with the Pack is revealed, and wins as he is destroyed" . wastesTest $ \pid -> do
    setUp pid [Locations.northDowns]
    advanceTo pid []
    advanceTo pid [Locations.northDowns, Locations.northDowns]
    resolveForced pid
    isNight `shouldReturn` True
    placementOf Enemies.packLeader `shouldReturn` Just StagingArea
    leader <- enemyOf Enemies.packLeader
    getModifiers (toTarget leader) >>= (`shouldSatisfy` elem CannotTakeDamage)
    run $ Destroy GameSource (toTarget leader)
    outcome `shouldReturn` Just Won

  it "loses the game as Amarthiúl leaves play" . wastesTest $ \pid -> do
    setUp pid [Locations.northDowns]
    run . Discard . toTarget =<< getsGame (fromJustNote "no Amarthiúl" . headMay . toList . (.entities.allies))
    outcome `shouldReturn` Just Lost
