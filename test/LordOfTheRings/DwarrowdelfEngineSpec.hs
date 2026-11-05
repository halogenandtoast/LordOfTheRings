module LordOfTheRings.DwarrowdelfEngineSpec (spec) where

import LordOfTheRings.Effect (Effect (..), EffectDuration (..))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Keyword (Keyword (Regenerate))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getEnemy)
import LordOfTheRings.Message.Lifted (
  addToStagingFromDiscard,
  dealDamageToOneCharacter,
  discardHand,
  immediateAttack,
  removeAllProgress,
  shuffleSetAsideIntoEncounterDeck,
 )
import LordOfTheRings.Modifier (ModifierType (GainKeyword))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Outcome (Outcome (Lost))
import LordOfTheRings.Phase (Phase (RefreshPhase, ResourcePhase))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (scenarioSetAside))
import TestImport

enemyOf :: CardDef -> GameT EnemyId
enemyOf def = getsGame (fromJustNote "no such enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

arwen :: GameT Target
arwen = getsGame (toTarget . fromJustNote "no Arwen" . find ((== toCardCode Objectives.arwenUndomielTheRedhornGate) . toCardCode) . toList . (.entities.allies))

-- | A Forest Spider in the staging area.
spider :: GameT EnemyId
spider = do
  run . AddToStaging =<< genEncounterCard Enemies.forestSpider
  enemyOf Enemies.forestSpider

spec :: Spec
spec = describe "Dwarrowdelf engine" do
  it "heals an enemy with regenerate during refresh, before player actions" . gameTest $ \_ -> do
    eid <- spider
    run $ CreateEffect $ Effect GameSource (toTarget eid) [GainKeyword (Regenerate 2)] UntilEndOfRound
    run $ PlaceTokens GameSource (toTarget eid) Damage 3
    run $ BeginPhase RefreshPhase
    getDamage (toTarget eid) `shouldReturn` 1

  it "opens a window as a character exhausts, for Arwen's response" . gameTest $ \pid -> do
    run . AddToStaging =<< genEncounterCard Objectives.arwenUndomielTheRedhornGate
    a <- arwen
    run $ Exhaust a
    chooseAbilityOf pid a
    beravor <- getHeroWithCode "01012"
    initial <- (.resources) <$> getHeroWithCode "01012"
    chooseTarget pid (toTarget beravor)
    ((.resources) <$> getHeroWithCode "01012") `shouldReturn` initial + 1

  it "defeats the players as Arwen leaves play" . gameTest $ \_ -> do
    run . AddToStaging =<< genEncounterCard Objectives.arwenUndomielRoadToRivendell
    a <- getsGame (toTarget . fromJustNote "no Arwen" . headMay . toList . (.entities.allies))
    run $ Discard GameSource a
    getsGame (.outcome) `shouldReturn` Just Lost

  it "makes an immediate attack outside combat, its shadow discarded after" . gameTest $ \pid -> do
    eid <- spider
    run $ EngageEnemy pid eid
    getsGame (.phase) `shouldReturn` ResourcePhase
    _ <- setEncounterDeck [Locations.oldForestRoad]
    runQueueT $ immediateAttack pid eid
    void mainLoop
    passActions pid
    chooseLabel "$label.takeAttackUndefended" pid
    passActions pid
    chooseTarget pid . toTarget =<< getHeroWithCode "01012"
    passActions pid
    getsGame (maybe 0 (length . (.shadowCards)) . lookup eid . (.entities.enemies)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` [toCardCode Locations.oldForestRoad]
    damage <- getDamage . toTarget =<< getHeroWithCode "01012"
    damage `shouldSatisfy` (> 0)

  it "has a player deal damage to 1 character they control" . gameTest $ \pid -> do
    sniper <- getEnemy =<< spider
    runQueueT $ dealDamageToOneCharacter sniper pid 2
    void mainLoop
    eowyn <- toTarget <$> getHeroWithCode "01007"
    chooseTarget pid eowyn
    getDamage eowyn `shouldReturn` 2

  it "removes all progress from the quest and every location" . gameTest $ \_ -> do
    quest <- putQuestStage Quests.fliesAndSpiders
    run $ PlaceTokens GameSource quest Progress 2
    run . AddToStaging =<< genEncounterCard Locations.oldForestRoad
    road <- getsGame (toTarget . fromJustNote "no road" . headMay . toList . (.entities.locations))
    run $ PlaceTokens GameSource road Progress 1
    runQueueT $ removeAllProgress GameSource
    void mainLoop
    getsGame (map (.progress) . toList . (.entities.quests)) `shouldReturn` [0]
    getsGame (map (.progress) . toList . (.entities.locations)) `shouldReturn` [0]

  it "discards a player's hand" . gameTest $ \pid -> do
    run $ DrawCards (Drawing pid 3)
    runQueueT $ discardHand pid
    void mainLoop
    ((.hand) <$> getPlayer pid) `shouldReturn` []

  it "returns a card from the encounter discard pile to the staging area" . gameTest $ \_ -> do
    card <- genEncounterCard Enemies.forestSpider
    run $ AddToEncounterDiscard [card]
    runQueueT $ addToStagingFromDiscard card
    void mainLoop
    getEncounterDiscard `shouldReturn` []
    void $ enemyOf Enemies.forestSpider

  it "shuffles set-aside cards into the encounter deck" . gameTest $ \_ -> do
    _ <- setEncounterDeck []
    shuffled <- genEncounterCard Enemies.forestSpider
    kept <- genEncounterCard Enemies.forestSpider
    run $ SetAside shuffled
    run $ SetAside kept
    runQueueT $ shuffleSetAsideIntoEncounterDeck [shuffled]
    void mainLoop
    getEncounterDeck `shouldReturn` [shuffled]
    getsGame (scenarioSetAside . toAttrs . (.scenario)) `shouldReturn` [kept]
