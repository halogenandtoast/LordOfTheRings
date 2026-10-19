module LordOfTheRings.RefreshPhaseSpec (spec) where

import LordOfTheRings.Phase
import TestImport

spec :: Spec
spec = describe "Refresh phase" do
  it "readies every exhausted character" . gameTest $ \_ -> do
    eowyn <- getHeroWithCode "01007"
    run $ Exhaust (toTarget eowyn)
    run $ BeginPhase RefreshPhase
    heroExhausted . toAttrs <$> getHeroWithCode "01007" `shouldReturn` False

  it "raises each player's threat by 1" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase RefreshPhase
    (.threat) <$> getPlayer pid `shouldReturn` 28

  it "lets a lone player keep the first player token" . gameTest $ \pid -> do
    run $ BeginPhase RefreshPhase
    getsGame (.firstPlayer) `shouldReturn` pid

  it "moves on to the next phase when a phase ends" . gameTest $ \_ -> do
    runOnly $ EndPhase CombatPhase
    peekQueue `shouldReturn` [Run [BeginPhase RefreshPhase, EndPhase RefreshPhase]]

  it "ends the round after the refresh phase and starts the next" . gameTest $ \_ -> do
    runOnly $ EndPhase RefreshPhase
    peekQueue `shouldReturn` [EndRound]
    void runNext -- EndRound
    void runNext -- BeginRound
    getsGame (.round) `shouldReturn` 1
    peekQueue `shouldReturn` [BeginPhase ResourcePhase, EndPhase ResourcePhase]
