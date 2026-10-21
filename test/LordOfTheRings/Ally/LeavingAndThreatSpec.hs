module LordOfTheRings.Ally.LeavingAndThreatSpec (spec) where

import LordOfTheRings.Ally.CardDefs qualified as Allies
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import TestImport

hero :: CardCode -> GameT Target
hero code = toTarget <$> getHeroWithCode code

stageEnemy :: CardDef -> GameT EnemyId
stageEnemy def = do
  card <- genEncounterCard def
  run $ AddToStaging card
  getsGame (fromJustNote "no enemy" . fmap toId . find ((== toCardCode def) . toCardCode) . toList . (.entities.enemies))

threat :: PlayerId -> GameT Int
threat pid = (.threat) <$> getPlayer pid

spec :: Spec
spec = do
  describe "Descendant of Thorondor" do
    it "deals 2 damage to an enemy in the staging area after it leaves play" . gameTest $ \pid -> do
      descendant <- putAllyIntoPlay pid Allies.descendantOfThorondor
      eid <- stageEnemy Enemies.forestSpider
      run $ Discard descendant
      chooseAbilityOf pid descendant
      chooseTarget pid eid
      getsGame (fmap (.damage) . find ((== eid) . toId) . toList . (.entities.enemies)) `shouldReturn` Just 2

  describe "Landroval" do
    it "puts a destroyed hero back into play, once per game" . gameTest $ \pid -> do
      landroval <- putAllyIntoPlay pid Allies.landroval
      run . Destroy =<< hero "01012"
      chooseAbilityOf pid landroval
      (.damage) <$> getHeroWithCode "01012" `shouldReturn` 1
      -- played again from hand, he no longer answers a hero's destruction
      card <- getsGame (fromJustNote "not in hand" . find ((== toCardCode Allies.landroval) . toCardCode) . (.hand) . fromJustNote "no player" . headMay . toList . (.players))
      run $ PutAllyIntoPlay pid card.id
      again <- getsGame (toTarget . fromJustNote "not in play" . headMay . toList . (.entities.allies))
      run . Destroy =<< hero "01007"
      offersAbilityOf pid again `shouldReturn` False

  describe "Elfhelm" do
    it "lowers your threat by 1 after an encounter card raises it" . gameTest $ \pid -> do
      elfhelm <- putAllyIntoPlay pid Allies.elfhelm
      run $ RaiseThreat pid 5
      _ <- setEncounterDeck [Enemies.cavernGuardian]
      run RevealEncounterCard
      chooseAbilityOf pid elfhelm
      threat pid `shouldReturn` 5

    it "does not answer a threat raise of the player's own" . gameTest $ \pid -> do
      elfhelm <- putAllyIntoPlay pid Allies.elfhelm
      run $ RaiseThreat pid 5
      offersAbilityOf pid elfhelm `shouldReturn` False
