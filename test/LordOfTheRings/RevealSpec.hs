module LordOfTheRings.RevealSpec (spec) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Phase (Phase (PlanningPhase, QuestPhase))
import LordOfTheRings.Scenario.Types (encounterDiscardL)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import TestImport

setEncounterDiscard :: [CardDef] -> GameT [EncounterCard]
setEncounterDiscard defs = do
  cards <- traverse genEncounterCard defs
  g <- getGame
  putGame $ g & scenarioL %~ overAttrs (encounterDiscardL .~ cards)
  pure cards

spec :: Spec
spec = describe "Revealing encounter cards" do
  it "resets an encounter deck emptied in the quest phase at once, before the card resolves" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.forestSpider]
    [road] <- setEncounterDiscard [Locations.oldForestRoad]
    putGame . (phaseL .~ QuestPhase) =<< getGame
    run RevealEncounterCard
    getEncounterDeck `shouldReturn` [road]
    getEncounterDiscard `shouldReturn` []

  it "reveals nothing from an empty encounter deck outside the quest phase" . gameTest $ \_ -> do
    _ <- setEncounterDeck []
    _ <- setEncounterDiscard [Enemies.forestSpider]
    putGame . (phaseL .~ PlanningPhase) =<< getGame
    run RevealEncounterCard
    getsGame (length . (.entities.enemies)) `shouldReturn` 0
    length <$> getEncounterDiscard `shouldReturn` 1

  it "discards on from the reset deck as it empties partway in the quest phase (ruling, Collateral Damage)" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.forestSpider]
    _ <- setEncounterDiscard [Locations.oldForestRoad, Locations.oldForestRoad, Enemies.forestSpider]
    putGame . (phaseL .~ QuestPhase) =<< getGame
    -- the 1 card, then 2 of the 4 the deck is reset to
    run $ DiscardTopOfEncounterDeck GameSource GameTarget 3
    length <$> getEncounterDeck `shouldReturn` 2
    length <$> getEncounterDiscard `shouldReturn` 2

  it "puts a revealed enemy into the staging area" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.forestSpider]
    run RevealEncounterCard
    map toCardCode . toList <$> getsGame (.entities.enemies) `shouldReturn` ["01096"]

  it "puts a revealed location into the staging area" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Locations.oldForestRoad]
    run RevealEncounterCard
    map toCardCode . toList <$> getsGame (.entities.locations) `shouldReturn` ["01099"]

  it "waits for the lead to click a revealed treachery, shown until it resolves" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Treacheries.drivenByShadow]
    run RevealEncounterCard
    getsGame (length . (.entities.treacheries)) `shouldReturn` 1
    resolveRevealed
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0

  it "resolves a treachery and then discards it" . gameTest $ \_ -> do
    treachery <- setEncounterDeck [Treacheries.drivenByShadow]
    run RevealEncounterCard
    resolveRevealed
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    getEncounterDiscard `shouldReturn` treachery

  it "reveals another card when a card has surge" . gameTest $ \_ -> do
    _ <- setEncounterDeck [Enemies.easternCrows, Locations.oldForestRoad]
    run RevealEncounterCard
    getsGame (length . (.entities.enemies)) `shouldReturn` 1
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "ends a surge chain in the quest phase once every card of the encounter deck has surged in it" . gameTest $ \_ -> do
    putGame . (phaseL .~ QuestPhase) =<< getGame
    -- the first is shuffled back in as the second empties the deck; the second's surge
    -- would only reveal the first again
    [firstOff, secondOff] <- setEncounterDeck [Treacheries.offTrack, Treacheries.offTrack]
    run RevealEncounterCard
    resolveRevealed
    getsGame (length . (.entities.treacheries)) `shouldReturn` 0
    getEncounterDeck `shouldReturn` [firstOff]
    getEncounterDiscard `shouldReturn` [secondOff]

  it "lets a surge chain go on while the encounter deck holds a card that has not surged in it" . gameTest $ \_ -> do
    putGame . (phaseL .~ QuestPhase) =<< getGame
    _ <- setEncounterDeck [Treacheries.offTrack]
    _ <- setEncounterDiscard [Locations.oldForestRoad]
    run RevealEncounterCard
    resolveRevealed
    getsGame (length . (.entities.locations)) `shouldReturn` 1

  it "shuffles the discard pile back in when the deck runs out" . gameTest $ \_ -> do
    treachery <- setEncounterDeck [Treacheries.drivenByShadow]
    run RevealEncounterCard
    resolveRevealed
    run RevealEncounterCard
    resolveRevealed
    getEncounterDiscard `shouldReturn` treachery
    getEncounterDeck `shouldReturn` []
