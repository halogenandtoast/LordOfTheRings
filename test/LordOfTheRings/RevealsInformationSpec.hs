module LordOfTheRings.RevealsInformationSpec (spec) where

import LordOfTheRings.Game.Public (revealsInformation)
import LordOfTheRings.Location.CardDefs qualified as Locations
import TestImport

-- | Whether running the message brings a hidden card into view.
reveals :: Message -> GameT Bool
reveals msg = do
  was <- getGame
  run msg
  revealsInformation was <$> getGame

spec :: Spec
spec = describe "Revealing information, which an undo may not reach back past for public statistics" do
  it "reveals a card drawn" . gameTest $ \pid -> do
    flipFirstQuest pid
    reveals (DrawCards $ Drawing pid 1) `shouldReturn` True

  it "reveals nothing raising threat" . gameTest $ \pid -> do
    flipFirstQuest pid
    reveals (RaiseThreat pid 1) `shouldReturn` False

  it "reveals nothing dealing a shadow card facedown, but the shadow card turning over" . gameTest $ \pid -> do
    flipFirstQuest pid
    e <- getsGame (fromJustNote "no enemy" . headMay . toList . (.entities.enemies))
    run $ EngageEnemy pid (toId e)
    [road] <- setEncounterDeck [Locations.oldForestRoad]
    reveals (DealShadowCard $ toId e) `shouldReturn` False
    reveals (RevealShadowCard (toId e) road.id) `shouldReturn` True
