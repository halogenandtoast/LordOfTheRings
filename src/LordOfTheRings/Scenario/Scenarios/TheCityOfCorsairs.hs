module LordOfTheRings.Scenario.Scenarios.TheCityOfCorsairs (theCityOfCorsairs) where

import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (characterController, getLead)
import LordOfTheRings.Matcher.Types (CardMatcher (CardMatchAny), cardIs, cardMatches)
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Scenario.Scenarios.DreamChaserCampaign (runDreamChaserCampaign)

newtype TheCityOfCorsairs = TheCityOfCorsairs ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

{- | Nightmare: the cards of the nightmare set with the Coast of Umbar icon join the
encounter deck (and the Corsair deck, as it is prepared); those with The City of Corsairs
icon join the second encounter deck. Its two Forced effects on stages 2B and 3A are the
deck switch's here and The Haven of Umbar's, in nightmare mode.
-}
theCityOfCorsairs :: Scenario
theCityOfCorsairs =
  Scenario
    $ scenario (TheCityOfCorsairs . (nightmareDeckL ?~ nightmare)) "the-city-of-corsairs" "The City of Corsairs"
 where
  nightmare =
    nightmareDeck
      Nightmare.theCityOfCorsairsNightmare
      EncounterSet.CoastOfUmbarNightmare
      [ (Enemies.corsairInfiltrator, 2)
      , (Locations.fogBank, 2)
      , (Locations.starlitSea, 2)
      , (Enemies.scoutingShip, 2)
      , (Treacheries.serpentsTail, 2)
      , (Locations.streetsOfUmbar, 1)
      ]

-- | The cards of the second encounter deck: The City of Corsairs set, and its nightmare one.
inSecondDeck :: EncounterCard -> Bool
inSecondDeck = (`elem` map Just [EncounterSet.TheCityOfCorsairs, EncounterSet.TheCityOfCorsairsNightmare]) . cdEncounterSet . toCardDef

{- | The encounter deck is built from the Coast of Umbar, Corsair Raiders, Stormy Weather,
Umbar Navy and Voyage Across Belegaer sets; The City of Corsairs set is set aside as the
inactive second encounter deck, until Fate of the Dream-chaser makes it the active one.
-}
instance RunMessage TheCityOfCorsairs where
  runMessage msg (TheCityOfCorsairs attrs) = case msg of
    DoStep 1 Setup -> do
      cards <-
        gatherEncounterSets attrs
          $ [ EncounterSet.CoastOfUmbar
            , EncounterSet.CorsairRaiders
            , EncounterSet.StormyWeather
            , EncounterSet.UmbarNavy
            , EncounterSet.VoyageAcrossBelegaer
            , EncounterSet.TheCityOfCorsairs
            ]
          <> [EncounterSet.TheCityOfCorsairsNightmare | attrs.nightmare]
      let (secondDeck, encounterDeck) = partition inSecondDeck cards
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
    -- The old encounter deck and discard pile are set aside inactive. Nothing in the
    -- scenario makes them active again, so they are let go rather than kept with the
    -- set-aside cards, where effects reading set-aside cards would find them.
    ScenarioSpecific "cityOfCorsairs.switchEncounterDeck" _ -> do
      -- Nightmare: "also search the encounter deck and discard pile for each copy of Umbar
      -- Bridge and each copy of Inspiring Fury and shuffle them into the second encounter
      -- deck."
      let carried =
            filter
              ( cardMatches
                  $ CardMatchAny
                  $ [cardIs Treacheries.battleHardened, cardIs Locations.watchTower]
                  <> [cardIs c | attrs.nightmare, c <- [Locations.umbarBridge, Treacheries.inspiringFury]]
              )
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
    -- Patrol Ship: "*Forced: After Patrol Ship is discarded from the encounter deck during a
    -- Sailing test, cancel all success symbols found unless you add it to the staging area."
    -- Each copy is asked in turn.
    SailingTestResult top found canceled
      | attrs.nightmare
      , ship : _ <- filter (cardMatches $ cardIs Enemies.patrolShip) top -> do
          lead <- getLead
          chooseOneM lead do
            questionLabeled "patrolShip.choose"
            showing [EncounterCard ship]
            labeled "patrolShip.stage" do
              addToStaging ship
              push $ SailingTestResult (filter (/= ship) top) found canceled
            labeled "patrolShip.cancel" $ push $ DoStep 1 (SailingTestResult top found found)
          pure $ TheCityOfCorsairs attrs
    DoStep 1 inner@(SailingTestResult {}) -> TheCityOfCorsairs <$> runMessage inner attrs
    -- Walls of Umbar: "*Forced: After Walls of Umbar is discarded from the encounter deck
    -- during a Sailing test, raise each player's threat by 1 for each ally he or she
    -- committed to that test."
    SailingTestEnded top committed | attrs.nightmare -> do
      let walls = length $ filter (cardMatches $ cardIs Locations.wallsOfUmbar) top
      when (walls > 0) do
        controllers <- traverse characterController [c | c@(AllyTarget _) <- committed]
        for_ (catMaybes controllers) \pid -> raiseThreat pid walls
      TheCityOfCorsairs <$> runMessage msg attrs
    -- Campaign: the Dream-chaser campaign card's setup, Forced effects and resolution
    _ | isJust attrs.campaign -> TheCityOfCorsairs <$> runDreamChaserCampaign msg attrs
    _ -> TheCityOfCorsairs <$> runMessage msg attrs
