module LordOfTheRings.DamageSpec (spec) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import TestImport

spec :: Spec
spec = describe "Damage" do
  it "places damage on a character" . gameTest $ \_ -> do
    beravor <- getHeroWithCode "01012"
    run $ DealDamage GameSource (toTarget beravor) 2
    (.damage) <$> getHeroWithCode "01012" `shouldReturn` 2

  it "destroys a hero whose damage reaches its hit points" . gameTest $ \pid -> do
    beravor <- getHeroWithCode "01012"
    run $ DealDamage GameSource (toTarget beravor) 4
    getsGame (Map.member (toId beravor) . (.entities.heroes)) `shouldReturn` False
    map toCardCode . (.discard) <$> getPlayer pid `shouldReturn` ["01012"]

  it "discards a destroyed enemy" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.forestSpider]
    run RevealEncounterCard
    spider <- getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
    run $ DealDamage GameSource (toTarget spider) 4
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    map toCardCode <$> getEncounterDiscard `shouldReturn` ["01096"]

  it "puts a destroyed enemy with victory points in the victory display" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.hummerhorns]
    run RevealEncounterCard
    hummerhorns <- getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
    run $ DealDamage GameSource (toTarget hummerhorns) 3
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    getEncounterDiscard `shouldReturn` []
