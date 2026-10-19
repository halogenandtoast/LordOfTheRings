module LordOfTheRings.RevealSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

spec :: Spec
spec = describe "Revealing encounter cards" do
  it "puts a revealed enemy into the staging area" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.forestSpider]
    run RevealEncounterCard
    map toCardCode . toList <$> getsGame (.entities.enemies) `shouldReturn` ["01096"]

  it "puts a revealed location into the staging area" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run RevealEncounterCard
    map toCardCode . toList <$> getsGame (.entities.locations) `shouldReturn` ["01099"]

  it "resolves a treachery and then discards it" . gameTest $ \_ -> do
    treachery <- setEncounterDeck [Treacheries.drivenByShadow]
    run RevealEncounterCard
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    getEncounterDiscard `shouldReturn` treachery

  it "reveals another card when a card has surge" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.easternCrows, Locations.oldForestRoad]
    run RevealEncounterCard
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "shuffles the discard pile back in when the deck runs out" . gameTest $ \_ -> do
    treachery <- setEncounterDeck [Treacheries.drivenByShadow]
    run RevealEncounterCard
    run RevealEncounterCard
    getEncounterDiscard `shouldReturn` treachery
    getEncounterDeck `shouldReturn` []
