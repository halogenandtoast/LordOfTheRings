module LordOfTheRings.Encounter.ScriptedCardsSpec (spec) where

import LordOfTheRings.Enemy (Enemy)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Helpers.Character
import LordOfTheRings.Helpers.Staging
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

reveal :: [CardDef] -> GameT ()
reveal defs = setEncounterDeck defs >> run RevealEncounterCard >> resolveRevealed

enemyWithCode :: CardCode -> GameT Enemy
enemyWithCode code = getsGame (fromJustNote "no enemy" . find ((== code) . toCardCode) . toList . (.entities.enemies))

spec :: Spec
spec = do
  describe "King Spider" do
    it "makes each player exhaust a character when revealed" . gameTest $ \pid -> do
      reveal [Enemies.kingSpider]
      eowyn <- getHeroWithCode "01007"
      chooseTarget pid eowyn
      heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` True

  describe "Hummerhorns" do
    it "deals 5 damage to a hero after engaging" . gameTest $ \pid -> do
      reveal [Enemies.hummerhorns]
      hummerhorns <- enemyWithCode "01075"
      run $ EngageEnemy pid (toId hummerhorns)
      beravor <- getHeroWithCode "01012"
      chooseTarget pid beravor
      getsGame (length . (.entities.heroes)) `shouldReturn` 2

  describe "Ungoliant's Spawn" do
    it "gives committed characters -1 willpower when revealed" . gameTest $ \pid -> do
      eowyn <- getHeroWithCode "01007"
      run $ CommitToQuest (toTarget eowyn)
      reveal [Enemies.ungoliantsSpawn]
      getWillpower (toTarget eowyn) `shouldReturn` 3

    it "raises the defending player's threat by 4 as a shadow" . gameTest $ \pid -> do
      reveal [Enemies.forestSpider]
      spider <- enemyWithCode "01096"
      run $ EngageEnemy pid (toId spider)
      _ <- setEncounterDeck [Enemies.ungoliantsSpawn]
      run DealShadowCards
      eowyn <- getHeroWithCode "01007"
      run $ DefendWith pid (toId spider) (toTarget eowyn)
      (.threat) <$> getPlayer pid `shouldReturn` 4

    it "raises it by 8 when the attack is undefended" . gameTest $ \pid -> do
      reveal [Enemies.forestSpider]
      spider <- enemyWithCode "01096"
      run $ EngageEnemy pid (toId spider)
      _ <- setEncounterDeck [Enemies.ungoliantsSpawn]
      run DealShadowCards
      run $ UndefendedAttack pid (toId spider)
      (.threat) <$> getPlayer pid `shouldReturn` 8

  describe "Eyes of the Forest" do
    it "discards every event in each player's hand" . gameTest $ \pid -> do
      event <- putCardIntoHand pid Events.aTestOfWill
      reveal [Treacheries.eyesOfTheForest]
      (.hand) <$> getPlayer pid `shouldReturn` []
      (.discard) <$> getPlayer pid `shouldReturn` [event]

  describe "Driven by Shadow" do
    it "gives each card in staging +1 threat until the end of the phase" . gameTest $ \_ -> do
      reveal [Locations.oldForestRoad]
      reveal [Treacheries.drivenByShadow]
      getStagingThreat `shouldReturn` 2

    it "surges when the staging area is empty" . gameTest $ \_ -> do
      reveal [Treacheries.drivenByShadow, Locations.oldForestRoad]
      getsGame (length . (.entities.locations)) `shouldReturn` 1

  describe "The Necromancer's Reach" do
    it "deals 1 damage to each exhausted character" . gameTest $ \_ -> do
      eowyn <- getHeroWithCode "01007"
      run $ Exhaust (toTarget eowyn)
      reveal [Treacheries.theNecromancersReach]
      (.damage) <$> getHeroWithCode "01007" `shouldReturn` 1
      (.damage) <$> getHeroWithCode "01012" `shouldReturn` 0

  describe "Forest Spider" do
    it "gets +1 attack until the end of the round after engaging" . gameTest $ \pid -> do
      reveal [Enemies.forestSpider]
      spider <- enemyWithCode "01096"
      run $ EngageEnemy pid (toId spider)
      getEnemyAttack (toId spider) `shouldReturn` 3
      runOnly $ EndPhase CombatPhase
      getEnemyAttack (toId spider) `shouldReturn` 3
