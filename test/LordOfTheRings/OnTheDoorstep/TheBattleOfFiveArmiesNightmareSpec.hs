module LordOfTheRings.OnTheDoorstep.TheBattleOfFiveArmiesNightmareSpec (spec) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Decklist (loadPlayer)
import LordOfTheRings.Effect (EffectDuration (UntilEndOfAttack, UntilEndOfPhase, UntilEndOfRound))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Public (publicGame)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEnemyDefense)
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getCurrentQuest, getEnemy)
import LordOfTheRings.Modifier (ModifierType (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario (theBattleOfFiveArmies)
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (
  currentProgress,
  removeProgressFromStage,
 )
import LordOfTheRings.Scenario.Types (GameMode (..), modeL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (WindowType (..), mkWhen)
import TestImport hiding (before, first)

nightmareScenario :: Scenario
nightmareScenario = overAttrs (modeL .~ NightmareMode) theBattleOfFiveArmies

nightmare :: (PlayerId -> GameT ()) -> IO ()
nightmare = gameTestIn nightmareScenario testDeck

tableNightmare :: Int -> ([PlayerId] -> GameT ()) -> IO ()
tableNightmare n body = do
  seats <-
    traverse
      (\i -> either (error . show) id <$> loadPlayer ("player" <> tshow i) testDeck)
      (1 :| [2 .. n])
  env <- newGameEnv (newTableIn nightmareScenario seats) (const $ pure ()) 1
  runGameT env $ getsGame (.playerOrder) >>= body

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    (toId . fromJustNote "enemy" . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "location"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.locations)
    )

questFor :: CardDef -> GameT Target
questFor def =
  getsGame
    ( toTarget
        . fromJustNote "quest"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.quests)
    )

battle :: GameT ()
battle = do
  for_ [Quests.theEasternSpur, Quests.battleInTheDale, Quests.theDefenseOfRavenhill] \def ->
    run . PutQuestIntoPlay =<< genEncounterCard def
  run . SetCurrentQuest =<< questFor Quests.theEasternSpur

progressStage :: CardDef -> Int -> GameT ()
progressStage def n = do
  q <- questFor def
  run $ SetCurrentQuest q
  run $ PlaceTokens GameSource q Progress n

stageFive :: GameT Target
stageFive = do
  run . PutQuestIntoPlay =<< genEncounterCard Quests.theCloudsBurst
  q <- questFor Quests.theCloudsBurst
  run $ Flip q
  resolveRevealed
  let finishSearches = do
        questions <- getsGame (mapToList . (.question))
        case [pid | (pid, QuestionLabel prompt _ _ _) <- questions, prompt == "$label.searchFoundNothing"] of
          pid : _ -> answerQuestion pid 0 >> finishSearches
          [] -> pure ()
  finishSearches
  pure q

nightmareCount :: GameT Int
nightmareCount =
  getsGame (fromMaybe 0 . parseMaybe (withObject "public game" (.: "nightmareProgress")) . publicGame)

passTableActions :: GameT ()
passTableActions = do
  questions <- getsGame (mapToList . (.question))
  case [pid | (pid, QuestionLabel prompt _ _ _) <- questions, prompt == "$label.takeAction"] of
    pid : _ -> passActions pid >> passTableActions
    [] -> pure ()

spec :: Spec
spec = describe "The Battle of Five Armies Nightmare" do
  it "builds the exact Nightmare encounter deck and removes the Eagle" . nightmare $ \_ -> do
    run $ DoStep 1 Setup
    cards <- getEncounterDeck
    length cards `shouldBe` 45
    let copies def = length $ filter ((== toCardCode def) . toCardCode) cards
    map
      copies
      [ Enemies.darkBats
      , Locations.hiddenPath
      , Treacheries.fightingAmongFriends
      , Enemies.gundabadClimber
      , Enemies.gundabadWolfRider
      , Objectives.mistyMountainEagle
      ]
      `shouldBe` [0, 0, 0, 0, 0, 0]
    map copies [Enemies.vanguardOfBolg, Enemies.ravenousWarg] `shouldBe` [3, 2]
    map
      copies
      [ Enemies.greatWarg
      , Enemies.frenziedWargRider
      , Enemies.gundabadElite
      , Locations.embattledValley
      , Locations.easternRidge
      , Treacheries.dreadHasCome
      ]
      `shouldBe` [3, 4, 4, 2, 3, 3]

  it "keeps ordinary progress removal before stage 5" . nightmare $ \_ -> do
    battle
    progressStage Quests.theEasternSpur 4
    run $ RemoveProgressFromQuest GameSource 2
    currentProgress `shouldReturn` 2
    nightmareCount `shouldReturn` 0

  it "replaces removing any amount from empty stage 5 with one setup-card progress" . nightmare $ \_ -> do
    q <- stageFive
    run $ RemoveProgressFromQuest GameSource 20
    nightmareCount `shouldReturn` 1
    currentProgress `shouldReturn` 0
    run $ RemoveTokens GameSource q Progress 3
    nightmareCount `shouldReturn` 2
    run $ RemoveProgressFromQuest GameSource 0
    nightmareCount `shouldReturn` 3

  it "replaces a named-stage removal even after that stage has left play" . nightmare $ \_ -> do
    _ <- stageFive
    -- Ruins of Dale still instructs removal from stage 3B at the end of the round.
    runQueueT $ removeProgressFromStage GameSource Quests.battleInTheDale 2
    void mainLoop
    nightmareCount `shouldReturn` 1

  it "does not replace removal from a location" . nightmare $ \_ -> do
    _ <- stageFive
    l <- stageLocation Locations.embattledValley
    run $ PlaceTokens GameSource (LocationTarget l) Progress 2
    run $ RemoveTokens GameSource (LocationTarget l) Progress 1
    (.progress) <$> getLocation l `shouldReturn` 1
    nightmareCount `shouldReturn` 0

  for_ [1 .. 4] \players ->
    it ("loses at the printed setup-card threshold for " <> show players <> " player(s)")
      . tableNightmare players
      $ \_ -> do
        _ <- stageFive
        let limit = 2 * players + 3
        replicateM_ (limit - 1) $ run $ RemoveProgressFromQuest GameSource 1
        nightmareCount `shouldReturn` (limit - 1)
        getsGame (.outcome) `shouldReturn` Nothing
        run $ RemoveProgressFromQuest GameSource 1
        getsGame (.outcome) `shouldReturn` Just Lost

  it "leaves the standard scenario's stage-5 removal behavior unchanged"
    . gameTestIn theBattleOfFiveArmies testDeck
    $ \_ -> do
      _ <- stageFive
      replicateM_ 6 $ run $ RemoveProgressFromQuest GameSource 1
      getsGame (.outcome) `shouldReturn` Nothing
      nightmareCount `shouldReturn` 0

  it "has Great Warg remove all current-quest progress after killing a character" . nightmare $ \pid -> do
    battle
    progressStage Quests.theEasternSpur 4
    e <- stageEnemy Enemies.greatWarg
    run $ EngageEnemy pid e
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ EnemyAttack pid e
    passTableActions
    chooseTarget pid defender
    passTableActions
    currentProgress `shouldReturn` 0

  it "has Great Warg advance the setup counter after a kill at stage 5" . nightmare $ \pid -> do
    _ <- stageFive
    e <- stageEnemy Enemies.greatWarg
    run $ EngageEnemy pid e
    defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
    run $ EnemyAttack pid e
    passTableActions
    chooseTarget pid defender
    passTableActions
    nightmareCount `shouldReturn` 1

  it "does not remove progress for Great Warg if its defender survives" . nightmare $ \pid -> do
    battle
    progressStage Quests.theEasternSpur 4
    e <- stageEnemy Enemies.greatWarg
    run $ EngageEnemy pid e
    defender <- hero "01012"
    run $ EnemyAttack pid e
    passTableActions
    chooseTarget pid defender
    passTableActions
    currentProgress `shouldReturn` 4

  it "has Great Warg's shadow discard an attachment controlled by the defending player" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.goblinSniper
    h <- hero "01012"
    _ <- putAttachmentOn pid Attachments.citadelPlate h
    run . AttachShadowCard e =<< genEncounterCard Enemies.greatWarg
    run $ ResolveShadow e pid []
    getsGame (length . (.entities.attachments)) `shouldReturn` 0

  it "has Frenzied Warg-Rider remove progress from every quest stage when it attacks" . nightmare $ \pid -> do
    battle
    for_ [Quests.theEasternSpur, Quests.battleInTheDale, Quests.theDefenseOfRavenhill] \def -> progressStage def 2
    e <- stageEnemy Enemies.frenziedWargRider
    run $ CheckWindows [mkWhen $ EnemyAttacking e pid]
    getsGame (sort . map (.progress) . toList . (.entities.quests)) `shouldReturn` [1, 1, 1]

  it "has Frenzied Warg-Rider's stage-5 instruction add exactly one setup progress" . nightmare $ \pid -> do
    _ <- stageFive
    e <- stageEnemy Enemies.frenziedWargRider
    run $ CheckWindows [mkWhen $ EnemyAttacking e pid]
    nightmareCount `shouldReturn` 1

  it "deals 2 shadow damage for Frenzied Warg-Rider with no current progress, 1 with progress"
    . nightmare
    $ \pid -> do
      battle
      e <- stageEnemy Enemies.goblinSniper
      h <- hero "01012"
      run . AttachShadowCard e =<< genEncounterCard Enemies.frenziedWargRider
      run $ ResolveShadow e pid [h]
      getDamage h `shouldReturn` 2
      progressStage Quests.theEasternSpur 1
      run . AttachShadowCard e =<< genEncounterCard Enemies.frenziedWargRider
      run $ ResolveShadow e pid [h]
      getDamage h `shouldReturn` 3

  it "prevents optional engagement with Gundabad Elite" . nightmare $ \_ -> do
    e <- stageEnemy Enemies.gundabadElite
    getModifiers (EnemyTarget e)
      >>= (`shouldSatisfy` any (\case CannotBeOptionallyEngagedBy _ -> True; _ -> False))

  it "allows removing 2 progress instead of Gundabad Elite attacking" . nightmare $ \pid -> do
    battle
    progressStage Quests.theEasternSpur 3
    _ <- stageEnemy Enemies.gundabadElite
    run $ CheckWindows [mkWhen RoundEnds]
    chooseLabel "$label.gundabadElite.remove" pid
    currentProgress `shouldReturn` 1

  it "allows Gundabad Elite's removal option at stage 5 through its replacement" . nightmare $ \pid -> do
    _ <- stageFive
    _ <- stageEnemy Enemies.gundabadElite
    run $ CheckWindows [mkWhen RoundEnds]
    chooseLabel "$label.gundabadElite.remove" pid
    nightmareCount `shouldReturn` 1

  it "has Eastern Ridge raise each player's threat by the number of empty stages"
    . twoPlayerTestIn nightmareScenario testDeck
    $ \one two -> do
      battle
      progressStage Quests.theEasternSpur 1
      _ <- stageLocation Locations.easternRidge
      oneBefore <- (.threat) <$> getPlayer one
      twoBefore <- (.threat) <$> getPlayer two
      run $ CheckWindows [mkWhen RoundEnds]
      (.threat) <$> getPlayer one `shouldReturn` (oneBefore + 2)
      (.threat) <$> getPlayer two `shouldReturn` (twoBefore + 2)

  it "keeps Eastern Ridge's shadow defense bonus through the phase, not just the attack" . nightmare $ \pid -> do
    battle
    progressStage Quests.theEasternSpur 1
    e <- stageEnemy Enemies.goblinSniper
    defense <- getEnemyDefense e
    run . AttachShadowCard e =<< genEncounterCard Locations.easternRidge
    run $ ResolveShadow e pid []
    getEnemyDefense e `shouldReturn` (defense + 2)
    run $ EndEffects UntilEndOfAttack
    getEnemyDefense e `shouldReturn` (defense + 2)
    run $ EndEffects UntilEndOfPhase
    getEnemyDefense e `shouldReturn` defense

  it "has Gundabad Elite attack with a shadow when fewer than 2 progress can be removed" . nightmare $ \pid -> do
    battle
    progressStage Quests.theEasternSpur 1
    e <- stageEnemy Enemies.gundabadElite
    _ <- setEncounterDeck [Enemies.goblinSniper]
    run $ CheckWindows [mkWhen RoundEnds]
    length . (.shadowCards) <$> getEnemy e `shouldReturn` 1
    passTableActions
    h <- hero "01012"
    chooseTarget pid h
    passTableActions
    getDamage h `shouldReturn` 3
    currentProgress `shouldReturn` 1

  it "does not trigger Gundabad Elite's end-round ability while engaged" . nightmare $ \pid -> do
    battle
    e <- stageEnemy Enemies.gundabadElite
    run $ EngageEnemy pid e
    run $ CheckWindows [mkWhen RoundEnds]
    getQuestion pid `shouldReturn` Nothing

  it "has Embattled Valley assign damage only among each player's heroes" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    battle
    progressStage Quests.theEasternSpur 1
    _ <- stageLocation Locations.embattledValley
    _ <- putAllyIntoPlay one Allies.guardOfTheCitadel
    heroes <- getsGame (toList . (.entities.heroes))
    let owned pid = toTarget $ fromJustNote "hero" $ find (\h -> h.owner == pid && toCardCode h == "01012") heroes
    run $ CheckWindows [mkWhen RoundEnds]
    chooseTarget one (owned one)
    chooseTarget one (owned one)
    chooseTarget two (owned two)
    chooseTarget two (owned two)
    getDamage (owned one) `shouldReturn` 2
    getDamage (owned two) `shouldReturn` 2
    getsGame (sum . map (countTokens Damage . allyTokens . toAttrs) . toList . (.entities.allies))
      `shouldReturn` 0

  it "pays Embattled Valley's travel exhaustion as a group" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    battle
    l <- stageLocation Locations.embattledValley
    ally <- putAllyIntoPlay two Allies.guardOfTheCitadel
    heroes <- getsGame (toList . (.entities.heroes))
    let owned pid = toTarget $ fromJustNote "hero" $ find (\h -> h.owner == pid && toCardCode h == "01012") heroes
    run $ TravelTo l
    chooseTarget one (owned one)
    chooseTarget one (owned two)
    chooseTarget one ally
    getsGame (length . filter (heroExhausted . toAttrs) . toList . (.entities.heroes)) `shouldReturn` 2
    getsGame (length . filter (allyExhausted . toAttrs) . toList . (.entities.allies)) `shouldReturn` 1
    (.placement) <$> getLocation l `shouldReturn` ActiveLocation

  it "forbids traveling to Embattled Valley unless the full exhaustion cost can be paid" . nightmare $ \_ -> do
    battle
    l <- stageLocation Locations.embattledValley
    run . Exhaust =<< hero "01012"
    getModifiers (LocationTarget l) >>= (`shouldSatisfy` elem CannotBeTraveledTo)

  it "allows traveling to Embattled Valley without exhaustion when every stage has progress" . nightmare $ \_ -> do
    battle
    for_ [Quests.theEasternSpur, Quests.battleInTheDale, Quests.theDefenseOfRavenhill] \def -> progressStage def 1
    l <- stageLocation Locations.embattledValley
    run $ TravelTo l
    getsGame (length . filter (heroExhausted . toAttrs) . toList . (.entities.heroes)) `shouldReturn` 0

  it
    "has Dread Has Come! give Bolg a shadow, then prevent progress for the round if his attack destroys a character"
    . nightmare
    $ \pid -> do
      battle
      progressStage Quests.theEasternSpur 2
      bolg <- stageEnemy Enemies.bolg
      defender <- putAllyIntoPlay pid Allies.guardOfTheCitadel
      _ <- setEncounterDeck [Treacheries.dreadHasCome, Enemies.goblinSniper]
      run RevealEncounterCard
      resolveRevealed
      length . (.shadowCards) <$> getEnemy bolg `shouldReturn` 1
      passTableActions
      chooseTarget pid defender
      passTableActions
      q <- questFor Quests.theEasternSpur
      run $ PlaceTokens GameSource q Progress 2
      currentProgress `shouldReturn` 2
      run $ EndEffects UntilEndOfRound
      run $ SetCurrentQuest q
      run $ PlaceTokens GameSource q Progress 2
      currentProgress `shouldReturn` 4

  it "does not block progress for Dread Has Come! when Bolg destroys no character" . nightmare $ \pid -> do
    battle
    progressStage Quests.theEasternSpur 2
    _ <- stageEnemy Enemies.bolg
    defender <- hero "01012"
    _ <- putAttachmentOn pid Attachments.citadelPlate defender
    _ <- setEncounterDeck [Treacheries.dreadHasCome, Enemies.goblinSniper]
    run RevealEncounterCard
    resolveRevealed
    passTableActions
    chooseTarget pid defender
    passTableActions
    q <- questFor Quests.theEasternSpur
    run $ PlaceTokens GameSource q Progress 2
    currentProgress `shouldReturn` 4

  it "makes Bolg attack each player in order for Dread Has Come!" . twoPlayerTestIn nightmareScenario testDeck $ \one two -> do
    battle
    _ <- stageEnemy Enemies.bolg
    firstDefender <- putAllyIntoPlay one Allies.guardOfTheCitadel
    secondDefender <- putAllyIntoPlay two Allies.guardOfTheCitadel
    _ <- setEncounterDeck [Treacheries.dreadHasCome, Enemies.goblinSniper, Enemies.goblinSniper]
    run RevealEncounterCard
    resolveRevealed
    passTableActions
    chooseTarget one firstDefender
    passTableActions
    chooseTarget two secondDefender
    passTableActions
    current <- getCurrentQuest
    for_ current \q -> getModifiers q >>= (`shouldSatisfy` elem (ProgressLimit 0))

  it "gives Dread Has Come!'s shadow +2 attack" . nightmare $ \pid -> do
    e <- stageEnemy Enemies.goblinSniper
    run . AttachShadowCard e =<< genEncounterCard Treacheries.dreadHasCome
    run $ ResolveShadow e pid []
    getModifiers (EnemyTarget e) >>= (`shouldSatisfy` elem (AttackModifier 2))

  it "counts Vanguard of Bolg's existing shadow at stage 5 even with no quest progress" . nightmare $ \pid -> do
    _ <- stageFive
    e <- stageEnemy Enemies.goblinSniper
    run $ EngageEnemy pid e
    defender <- putAllyIntoPlay pid Allies.snowbournScout
    run . AttachShadowCard e =<< genEncounterCard Enemies.vanguardOfBolg
    run $ EnemyAttack pid e
    passTableActions
    chooseTarget pid defender
    passTableActions
    nightmareCount `shouldReturn` 1

  it "does not activate the replacement on the unflipped stage-5 A side" . nightmare $ \_ -> do
    run . PutQuestIntoPlay =<< genEncounterCard Quests.theCloudsBurst
    run $ RemoveProgressFromQuest GameSource 1
    nightmareCount `shouldReturn` 0
