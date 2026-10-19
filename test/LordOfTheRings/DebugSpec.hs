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

  it "search a deck and take any card into hand" . gameTest $ \pid -> do
    card : _ <- drop 5 . (.deck) <$> getPlayer pid
    _ <- runDebug $ DebugSearchDeck pid
    chooseTarget pid (CardIdTarget card.id)
    chooseLabel "$label.done" pid
    map (.id) . (.hand) <$> getPlayer pid >>= (`shouldSatisfy` elem card.id)
    map (.id) . (.deck) <$> getPlayer pid >>= (`shouldSatisfy` notElem card.id)

  it "read the messages the board sends" $ for_
    [ "{\"tag\":\"PlaceTokens\",\"contents\":[{\"tag\":\"GameSource\"},{\"tag\":\"HeroTarget\",\"contents\":\"0b6f5a1e-3c2d-4e8f-9a7b-1c2d3e4f5a6b\"},\"Damage\",1]}"
    , "{\"tag\":\"RaiseThreat\",\"contents\":[\"5d1c9e2a-7b3f-4a6e-8c0d-2f4b6a8c0e1d\",1]}"
    , "{\"tag\":\"DrawCards\",\"contents\":{\"player\":\"5d1c9e2a-7b3f-4a6e-8c0d-2f4b6a8c0e1d\",\"count\":1}}"
    , "{\"tag\":\"DebugSearchDeck\",\"contents\":\"5d1c9e2a-7b3f-4a6e-8c0d-2f4b6a8c0e1d\"}"
    , "{\"tag\":\"DebugSearchEncounterDeck\",\"contents\":\"5d1c9e2a-7b3f-4a6e-8c0d-2f4b6a8c0e1d\"}"
    ]
    \json -> (eitherDecode json :: Either String Message) `shouldSatisfy` either (const False) (const True) :: IO ()
