module LordOfTheRings.EndGameSpec (spec) where

import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Game.Public (GroupScore (..), groupScore, publicGame)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Scenario.Types (victoryDisplayL)
import LordOfTheRings.Outcome
import TestImport

spec :: Spec
spec = describe "End of the game" do
  it "eliminates a player whose threat reaches 50" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ RaiseThreat pid 23
    (.eliminated) <$> getPlayer pid `shouldReturn` True

  it "uses the elimination level as the final threat when eliminated below it" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ EliminatePlayer pid
    (.threat) <$> getPlayer pid `shouldReturn` 50

  it "caps the final threat at the elimination level" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ RaiseThreat pid 40
    (.threat) <$> getPlayer pid `shouldReturn` 50

  it "loses the game when every player is eliminated" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ RaiseThreat pid 23
    getsGame (.outcome) `shouldReturn` Just Lost

  it "discards an eliminated player's hand and heroes" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ EliminatePlayer pid
    player <- getPlayer pid
    player.hand `shouldBe` []
    length player.discard `shouldBe` 9
    getsGame (length . (.entities.heroes)) `shouldReturn` 0

  it "eliminates a player who has no heroes left" . gameTest $ \pid -> do
    flipFirstQuest pid
    heroes <- getsGame (toList . (.entities.heroes))
    for_ heroes $ run . Discard . toTarget
    run CheckEliminations
    (.eliminated) <$> getPlayer pid `shouldReturn` True

  it "reproduces the rulebook's score of 52" . gameTestWith (deckList ["01012", "01001", "01002"] [("01013", 10)]) $ \pid -> do
    flipFirstQuest pid
    heroes <- getsGame (toList . (.entities.heroes))
    for_ heroes $ \h ->
      if toCardCode h == "01002"
        then run $ Discard (toTarget h)
        else run $ PlaceTokens GameSource (toTarget h) Damage 3
    run $ RaiseThreat pid 13
    victory <- EncounterCard <$> genEncounterCard Enemies.hummerhorns
    g <- getGame
    putGame $ g & scenarioL %~ overAttrs (victoryDisplayL .~ [victory])
    run $ GameOver Won
    seen <- getsGame publicGame
    let score = case seen of
          Object o -> KeyMap.lookup "score" o
          _ -> Nothing
    score `shouldBe` Just (object
      [ "threat" .= (43 :: Int)
      , "deadHeroes" .= (8 :: Int)
      , "damage" .= (6 :: Int)
      , "rounds" .= (0 :: Int)
      , "victoryPoints" .= (5 :: Int)
      , "total" .= (52 :: Int)
      ])
    getsGame ((.total) . groupScore) `shouldReturn` 52

  it "reports eliminated heroes as dead and their final threat as 50" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ EliminatePlayer pid
    seen <- getsGame publicGame
    let score = case seen of
          Object o -> KeyMap.lookup "score" o
          _ -> Nothing
    score `shouldBe` Just (object
      [ "threat" .= (50 :: Int)
      , "deadHeroes" .= (27 :: Int)
      , "damage" .= (0 :: Int)
      , "rounds" .= (0 :: Int)
      , "victoryPoints" .= (0 :: Int)
      , "total" .= (77 :: Int)
      ])

  it "adds 10 to the score for each round" . gameTest $ \pid -> do
    flipFirstQuest pid
    unscored <- getsGame ((.total) . groupScore)
    g <- getGame
    putGame g {gameRound = 3}
    score <- getsGame groupScore
    score.rounds `shouldBe` 30
    score.total `shouldBe` unscored + 30

  it "stops playing once the game is over" . gameTest $ \_ -> do
    run $ GameOver Lost
    runOnly EndRound
    peekQueue `shouldReturn` []
