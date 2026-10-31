module LordOfTheRings.Scenario.Scenarios.TheCityOfCorsairs (theCityOfCorsairs) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher.Types (CardMatcher (CardMatchAny), cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Scenario.Scenarios.DreamChaserCampaign (runDreamChaserCampaign)

newtype TheCityOfCorsairs = TheCityOfCorsairs ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theCityOfCorsairs :: Scenario
theCityOfCorsairs = Scenario $ scenario TheCityOfCorsairs "the-city-of-corsairs" "The City of Corsairs"

{- | The encounter deck is built from the Coast of Umbar, Corsair Raiders, Stormy Weather,
Umbar Navy and Voyage Across Belegaer sets; The City of Corsairs set is set aside as the
inactive second encounter deck, until Fate of the Dream-chaser makes it the active one.
-}
instance RunMessage TheCityOfCorsairs where
  runMessage msg (TheCityOfCorsairs attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        concatMapM
          (gatherEncounterSet attrs)
          [ EncounterSet.CoastOfUmbar
          , EncounterSet.CorsairRaiders
          , EncounterSet.StormyWeather
          , EncounterSet.UmbarNavy
          , EncounterSet.VoyageAcrossBelegaer
          ]
      secondDeck <- gatherEncounterSet attrs EncounterSet.TheCityOfCorsairs
      firstStage <- genEncounterCard Quests.theCoastOfUmbar
      laterStages <- traverse genEncounterCard [Quests.fateOfTheDreamChaser, Quests.theHavenOfUmbar, Quests.finalConfrontation]
      putQuestIntoPlay firstStage
      pure
        $ TheCityOfCorsairs
        $ attrs
        & encounterDeckL .~ encounterDeck
        & questDeckL .~ laterStages
        & setAsideL .~ secondDeck
    ScenarioSpecific "cityOfCorsairs.stormcallerDestroyed" _ -> pure $ TheCityOfCorsairs $ attrs & metaL .~ toJSON True
    -- ponytail: the old encounter deck and discard pile are set aside inactive, and nothing
    -- in the scenario makes them active again, so they are let go
    ScenarioSpecific "cityOfCorsairs.switchEncounterDeck" _ -> do
      let carried =
            filter
              (cardMatches $ CardMatchAny [cardIs Treacheries.battleHardened, cardIs Locations.watchTower])
              (attrs.encounterDeck <> attrs.encounterDiscard)
      deck <- shuffleM $ attrs.scenarioSetAside <> fromMaybe [] attrs.corsairDeck <> attrs.scenarioCorsairDiscard <> carried
      pure
        $ TheCityOfCorsairs
        $ attrs
        & encounterDeckL .~ deck
        & encounterDiscardL .~ []
        & setAsideL .~ []
        & corsairDeckL .~ Nothing
        & corsairDiscardL .~ []
    -- Campaign: the Dream-chaser campaign card's setup, Forced effects and resolution
    _ | isJust attrs.campaign -> TheCityOfCorsairs <$> runDreamChaserCampaign msg attrs
    _ -> TheCityOfCorsairs <$> runMessage msg attrs
