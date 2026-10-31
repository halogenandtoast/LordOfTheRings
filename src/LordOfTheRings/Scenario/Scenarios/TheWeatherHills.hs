module LordOfTheRings.Scenario.Scenarios.TheWeatherHills (theWeatherHills) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getLead)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries

newtype TheWeatherHills = TheWeatherHills ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWeatherHills :: Scenario
theWeatherHills =
  Scenario
    $ scenario (TheWeatherHills . (nightmareDeckL ?~ nightmare)) "the-weather-hills" "The Weather Hills"
 where
  -- Its setup card's "Shuffle each copy of Captive Villager and Rearguard Ambusher into
  -- the Orc deck" is the Orc deck taking them ('isOrcDeckCard'); "the players cannot win
  -- while a Captive Villager is guarded" is Cornered Animals', and "if a Captive Villager
  -- leaves play, the players lose" the Villager's own.
  nightmare =
    nightmareDeck
      Nightmare.theWeatherHillsNightmare
      EncounterSet.TheWeatherHillsNightmare
      [ (Enemies.corneredOrc, 3)
      , (Locations.shelteredValley, 2)
      , (Locations.concealedOrcCamp, 1)
      , (Enemies.angmarMarauder, 2)
      , (Enemies.angmarOrc, 2)
      , (Locations.ruinsOfArnor, 3)
      , (Treacheries.tragicDiscovery, 2)
      ]

{- | The encounter deck is built from The Weather Hills, Foul Weather, Angmar Orcs and
Ruins of Arnor. The Orc deck (rules insert) takes each enemy of Angmar Orcs and each
Concealed Orc-camp out of it, shuffled, to sit beside the quest deck: a card revealed from
it is staged as if revealed from the encounter deck.
-}
instance RunMessage TheWeatherHills where
  runMessage msg (TheWeatherHills attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets
          attrs
          [EncounterSet.TheWeatherHills, EncounterSet.FoulWeather, EncounterSet.AngmarOrcs, EncounterSet.RuinsOfArnor]
      let (orcs, encounterDeck) = partition isOrcDeckCard cards
      deck <- shuffleM orcs
      firstStage <- genEncounterCard Quests.scatteredAmongTheHills
      laterStages <- traverse genEncounterCard [Quests.corneredAnimals]
      putQuestIntoPlay firstStage
      pure
        $ TheWeatherHills
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & metaL .~ toJSON (Meta deck)
    ScenarioSpecific "orcDeck.reveal" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      let (top, rest) = splitAt n meta.orcDeck
      traverse_ (push . RevealCard) top
      pure $ withMeta meta {orcDeck = rest}
    -- an enemy or location guards the Captive Villager; another Villager is revealed and
    -- the next card guards this one. With the Orc deck empty it is free of encounters.
    ScenarioSpecific "orcDeck.guard" (parseMaybe parseJSON -> Just oid) -> case meta.orcDeck of
      card : rest -> do
        if cdCardType (toCardDef card) `elem` [EnemyType, LocationType]
          then guardObjectiveWith oid card.id >> push (RevealCard card)
          else push (RevealCard card) >> guardFromOrcDeck oid
        pure $ withMeta meta {orcDeck = rest}
      [] -> do
        putObjectiveAllyIntoPlay oid =<< getLead
        pure $ TheWeatherHills attrs
    ScenarioSpecific "orcDeck.shuffleIn" _ -> do
      deck <- shuffleM (meta.orcDeck <> attrs.encounterDeck)
      pure $ TheWeatherHills $ attrs & encounterDeckL .~ deck & metaL .~ toJSON (Meta [])
    _ -> TheWeatherHills <$> runMessage msg attrs
   where
    meta = getMeta attrs
    withMeta m = TheWeatherHills $ attrs & metaL .~ toJSON m
