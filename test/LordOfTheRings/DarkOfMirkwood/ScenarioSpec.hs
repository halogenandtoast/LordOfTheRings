module LordOfTheRings.DarkOfMirkwood.ScenarioSpec (spec) where

import Data.Aeson (Result (..))
import Data.Map.Strict qualified as Map
import LordOfTheRings.Effect
import LordOfTheRings.Enemy (EnemyAttrs (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getEngagementCost)
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (EnemyMatcher (..), PlayerMatcher (PlayerWithId), Query (select))
import LordOfTheRings.Modifier
import LordOfTheRings.Objective (ObjectiveAttrs (..))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Won))
import LordOfTheRings.Phase (Phase (EncounterPhase, PlanningPhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Quest.Runner (getQuestPoints)
import LordOfTheRings.Scenario (theCavesOfNibinDum, theOath)
import LordOfTheRings.Scenario.Types (scenarioSetAside)
import TestImport

spec :: Spec
spec = describe "The Dark of Mirkwood scenarios" do
  it "sets aside Goblin Troop and stages one Eaves of Mirkwood during The Oath setup" . gameTestIn theOath testDeck $ \pid -> do
    flipFirstQuest pid
    aside <- getsGame (scenarioSetAside . toAttrs . (.scenario))
    map toCardCode aside `shouldSatisfy` elem (toCardCode Enemies.goblinTroopDarkOfMirkwood)
    staged <- getsGame (map toCardCode . toList . (.entities.locations))
    staged `shouldBe` [toCardCode Locations.theEavesOfMirkwoodDarkOfMirkwood]

  it "sets aside the Chieftain and Pillar and attaches Cave Torch to the first player's hero" . gameTestIn theCavesOfNibinDum testDeck $ \pid -> do
    flipFirstQuest pid
    hero <- getHeroWithCode "01012"
    chooseTarget pid hero
    aside <- getsGame (map toCardCode . scenarioSetAside . toAttrs . (.scenario))
    aside `shouldSatisfy` elem (toCardCode Enemies.goblinChieftain)
    aside `shouldSatisfy` elem (toCardCode Locations.crackedPillar)
    torches <- getsGame (toList . (.entities.objectives))
    map toCardCode torches `shouldBe` [toCardCode Objectives.caveTorchDarkOfMirkwood]
    map (objectivePlacement . toAttrs) torches `shouldBe` [AttachedTo $ toTarget hero]
    chooseTarget pid =<< getActiveQuest
    chooseOptionMatching "choose a location" pid (const True)
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "makes successive players choose locations with different names" . twoPlayerTestIn theCavesOfNibinDum testDeck $ \one two -> do
    stage <- putQuestStage Quests.theDarkOfNibinDum
    [tunnel, _, _] <-
      setEncounterDeck
        [ Locations.goblinTunnelsDarkOfMirkwood
        , Locations.goblinTunnelsDarkOfMirkwood
        , Locations.branchingPathsDarkOfMirkwood
        ]
    run $ WhenRevealed stage
    chooseTarget one (CardIdTarget tunnel.id)
    Just question <- getQuestion two
    map toCardCode (searchedCards question)
      `shouldBe` [toCardCode Locations.branchingPathsDarkOfMirkwood]
    chooseOptionMatching "take the other location" two (const True)
    getsGame (length . (.entities.locations)) `shouldReturn` 2

  it "unlocks Chieftain damage only after eight progress on Oathkeepers" . gameTestIn theCavesOfNibinDum testDeck $ \_ -> do
    stage <- putQuestStage Quests.oathkeepers
    run $ Flip stage
    lead <- getLead
    chooseTarget lead stage
    chief <- genEncounterCard Enemies.goblinChieftain
    run $ AddToStaging chief
    eid <- getsGame (fromJustNote "chieftain" . headMay . Map.keys . (.entities.enemies))
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 0
    run $ PlaceTokens GameSource stage Progress 8
    run $ DealDamage GameSource (EnemyTarget eid) 1
    getDamage (EnemyTarget eid) `shouldReturn` 1
    toTarget <$> getActiveQuest `shouldReturn` stage

  it "holds Mirkwood Forest at twelve progress until Goblin Trail enters the victory display" . gameTestIn theOath testDeck $ \_ -> do
    stage <- putQuestStage Quests.mirkwoodForestTheOath
    run $ Flip stage
    -- Resolve the search without locations available, then place the needed progress.
    pid <- getLead
    chooseTarget pid stage
    chooseLabel "$label.continue" pid
    run $ PlaceTokens GameSource stage Progress 12
    toTarget <$> getActiveQuest `shouldReturn` stage
    trail <- genEncounterCard Locations.goblinTrail
    run $ AddToStaging trail
    lid <- getsGame (fromJustNote "trail" . headMay . Map.keys . (.entities.locations))
    run $ AddToVictoryDisplay (LocationTarget lid)
    getsGame (.outcome) `shouldReturn` Just Won

  it "treats engagement cost as zero during Surprise Attack's encounter phase" . gameTestIn theCavesOfNibinDum testDeck $ \_ -> do
    stage <- putQuestStage Quests.surpriseAttackNibinDum
    -- Mark the stage flipped directly so this test only concerns its continuous effect.
    run $ Flip stage
    pid <- getLead
    chooseTarget pid stage
    chooseLabel "$label.continue" pid
    enemy <- genEncounterCard Enemies.goblinSniperDarkOfMirkwood
    run $ AddToStaging enemy
    eid <- getsGame (fromJustNote "sniper" . headMay . Map.keys . (.entities.enemies))
    getEngagementCost eid `shouldReturn` 48
    g <- getGame
    putGame $ g {gamePhase = EncounterPhase}
    run $ Run []
    getEngagementCost eid `shouldReturn` 0
    current <- getGame
    putGame $ current {gamePhase = PlanningPhase}
    run $ Run []
    getEngagementCost eid `shouldReturn` 48

  for_ [("discarded", Discard GameSource), ("removed from the game", RemoveFromGame)] \(description, removeEnemy) ->
    it
      ( "reduces Surprise Attack's quest points after an enemy is "
          <> description
          <> " and advances with existing progress"
      )
      . gameTestIn theCavesOfNibinDum testDeck
      $ \pid -> do
        stage <- putQuestStage Quests.surpriseAttackNibinDum
        run $ Flip stage
        chooseTarget pid stage
        chooseLabel "$label.continue" pid
        for_ [Enemies.goblinSniperDarkOfMirkwood, Enemies.goblinRunnersDarkOfMirkwood]
          $ genEncounterCard
          >=> run
          . AddToStaging
        getActiveQuest >>= getQuestPoints . toAttrs >>= (`shouldBe` 8)
        run $ PlaceTokens GameSource stage Progress 4
        toTarget <$> getActiveQuest `shouldReturn` stage
        runner <-
          getsGame
            ( fromJustNote "runner"
                . fmap toId
                . find ((== toCardCode Enemies.goblinRunnersDarkOfMirkwood) . toCardCode)
                . toList
                . (.entities.enemies)
            )
        run $ removeEnemy (EnemyTarget runner)
        getsGame (.outcome) `shouldReturn` Just Won

  it "allows another player's ranged character to join the attack on Cracked Pillar" . twoPlayerTest testDeck $ \one two -> do
    pillar <- genEncounterCard Locations.crackedPillar
    run $ AddToStaging pillar
    lid <- getsGame (fromJustNote "pillar" . headMay . Map.keys . (.entities.locations))
    ranged <- heroOf two "01002"
    run $ CreateEffect $ Effect GameSource ranged [GainKeyword Ranged, AttackModifier 2] UntilEndOfRound
    run $ BeginLocationAttack one lid
    heroOf one "01012" >>= chooseTarget one
    chooseLabel "$label.doneAttacking" one
    chooseTarget two ranged
    passActions one
    passActions two
    passActions one
    passActions two
    getDamage (LocationTarget lid) `shouldReturn` 4

  it "runs a Pillar attack through normal declarations and routes damage to the location" . gameTest $ \pid -> do
    pillar <- genEncounterCard Locations.crackedPillar
    run $ AddToStaging pillar
    lid <- getsGame (fromJustNote "pillar" . headMay . Map.keys . (.entities.locations))
    theodred <- getHeroWithCode "01002"
    run
      $ CreateEffect
      $ Effect
        GameSource
        (toTarget theodred)
        [AttackBonusAgainstMatching (EnemyEngagedWith $ PlayerWithId pid) 1]
        UntilEndOfRound
    run $ BeginLocationAttack pid lid
    select AnyEnemy `shouldReturn` []
    actor <- getsGame (fromJustNote "attack actor" . headMay . toList . (.entities.enemies))
    (toAttrs actor).enemyAttackLocation `shouldBe` Just lid
    stored <- getGame
    case fromJSON (toJSON stored) :: Result Game of
      Success restored -> putGame restored
      Error err -> error err
    for_ ["01012", "01002", "01007"] $ getHeroWithCode >=> chooseTarget pid
    passActions pid
    getDamage (LocationTarget lid) `shouldReturn` 4
    heroExhausted . toAttrs <$> getHeroWithCode "01012" `shouldReturn` True
    getsGame (Map.null . (.entities.enemies)) `shouldReturn` True
    mods <- getModifiers (LocationTarget lid)
    (CannotBeTraveledTo `elem` mods) `shouldBe` False
    run $ TravelTo lid
    getDamage (LocationTarget lid) `shouldReturn` 0
