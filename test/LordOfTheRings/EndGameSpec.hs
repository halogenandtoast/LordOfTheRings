module LordOfTheRings.EndGameSpec (spec) where

import LordOfTheRings.Outcome
import TestImport

spec :: Spec
spec = describe "End of the game" do
  it "eliminates a player whose threat reaches 50" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ RaiseThreat pid 23
    (.eliminated) <$> getPlayer pid `shouldReturn` True

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

  it "stops playing once the game is over" . gameTest $ \_ -> do
    run $ GameOver Lost
    runOnly EndRound
    peekQueue `shouldReturn` []
