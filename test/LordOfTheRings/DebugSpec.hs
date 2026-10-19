module LordOfTheRings.DebugSpec (spec) where

import LordOfTheRings.Phase
import TestImport

spec :: Spec
spec = describe "Debug messages" do
  it "run while a question waits, then ask it again" . gameTest $ \pid -> do
    flipFirstQuest pid
    run $ BeginPhase QuestPhase
    Just asked <- getQuestion pid
    threat <- (.threat) <$> getPlayer pid
    _ <- runDebug $ RaiseThreat pid 3
    (.threat) <$> getPlayer pid `shouldReturn` threat + 3
    fmap questionChoices <$> getQuestion pid `shouldReturn` Just (questionChoices asked)

  it "read the messages the board sends" $ for_
    [ "{\"tag\":\"PlaceTokens\",\"contents\":[{\"tag\":\"GameSource\"},{\"tag\":\"HeroTarget\",\"contents\":\"0b6f5a1e-3c2d-4e8f-9a7b-1c2d3e4f5a6b\"},\"Damage\",1]}"
    , "{\"tag\":\"RaiseThreat\",\"contents\":[\"5d1c9e2a-7b3f-4a6e-8c0d-2f4b6a8c0e1d\",1]}"
    ]
    \json -> (eitherDecode json :: Either String Message) `shouldSatisfy` either (const False) (const True) :: IO ()
