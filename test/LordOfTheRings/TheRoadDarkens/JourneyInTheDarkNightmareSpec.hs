module LordOfTheRings.TheRoadDarkens.JourneyInTheDarkNightmareSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Event.CardDefs qualified as Events
import LordOfTheRings.Location.CardDefs qualified as Locations
import TestImport

stageLocation :: CardDef -> GameT LocationId
stageLocation def = do
  run . AddToStaging =<< genEncounterCard def
  getsGame
    ( toId
        . fromJustNote "staged location"
        . find ((== toCardCode def) . toCardCode)
        . toList
        . (.entities.locations)
    )

spec :: Spec
spec = describe "Journey in the Dark Nightmare" do
  it "Darkened Mine prevents progress on itself and other staged locations, but permits active-location progress" . gameTest $ \_ -> do
    mine <- stageLocation Locations.darkenedMine
    other <- stageLocation Locations.theOldFord
    run $ PlaceTokens GameSource (toTarget mine) Progress 1
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation mine `shouldReturn` 0
    (.progress) <$> getLocation other `shouldReturn` 0
    run $ PlaceLocation other ActiveLocation
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation other `shouldReturn` 1

  it "Darkened Mine stops blocking staged locations when it becomes active" . gameTest $ \_ -> do
    mine <- stageLocation Locations.darkenedMine
    other <- stageLocation Locations.theOldFord
    run $ PlaceLocation mine ActiveLocation
    run $ PlaceTokens GameSource (toTarget mine) Progress 1
    run $ PlaceTokens GameSource (toTarget other) Progress 1
    (.progress) <$> getLocation mine `shouldReturn` 1
    (.progress) <$> getLocation other `shouldReturn` 1

  it "Darkened Mine's shadow discards one card from the defending player's hand" . gameTest $ \pid -> do
    _ <- putCardIntoHand pid Events.aTestOfWill
    _ <- putCardIntoHand pid Events.fortuneOrFate
    handBefore <- length . (.hand) <$> getPlayer pid
    run . AddToStaging =<< genEncounterCard Enemies.forestSpider
    eid <- getsGame (toId . fromJustNote "shadow enemy" . headMay . toList . (.entities.enemies))
    run . AttachShadowCard eid =<< genEncounterCard Locations.darkenedMine
    run $ ResolveShadow eid pid []
    length . (.hand) <$> getPlayer pid `shouldReturn` (handBefore - 1)
