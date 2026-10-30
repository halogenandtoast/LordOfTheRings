module LordOfTheRings.Scenario.Scenarios.TheWeatherHills (theWeatherHills) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheWeatherHills.Helpers

newtype TheWeatherHills = TheWeatherHills ScenarioAttrs
  deriving anyclass (IsScenario)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWeatherHills :: Scenario
theWeatherHills = Scenario $ scenario TheWeatherHills "the-weather-hills" "The Weather Hills"

{- | The encounter deck is built from The Weather Hills, Foul Weather, Angmar Orcs and
Ruins of Arnor. The Orc deck (rules insert) takes each enemy of Angmar Orcs and each
Concealed Orc-camp out of it, shuffled, to sit beside the quest deck: a card revealed from
it is staged as if revealed from the encounter deck.
-}
instance RunMessage TheWeatherHills where
  runMessage msg (TheWeatherHills attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        concatMapM
          (gatherEncounterSet attrs)
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
    ScenarioSpecific "orcDeck.shuffleIn" _ -> do
      deck <- shuffleM (meta.orcDeck <> attrs.encounterDeck)
      pure $ TheWeatherHills $ attrs & encounterDeckL .~ deck & metaL .~ toJSON (Meta [])
    _ -> TheWeatherHills <$> runMessage msg attrs
   where
    meta = getMeta attrs
    withMeta m = TheWeatherHills $ attrs & metaL .~ toJSON m
