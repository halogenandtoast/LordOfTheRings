module LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors (acrossTheEttenmoors) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Campaign (PoolCard (..), addToPool)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.CardDefs qualified as Nightmare
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AngmarAwakened (angmarSetup)
import LordOfTheRings.Treachery.CardDefs qualified as Treacheries
import LordOfTheRings.Window (Timing (After), Window (..), WindowType (PhaseBegins))

newtype AcrossTheEttenmoors = AcrossTheEttenmoors ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

acrossTheEttenmoors :: Scenario
acrossTheEttenmoors =
  Scenario
    $ scenario (AcrossTheEttenmoors . (nightmareDeckL ?~ nightmare)) "across-the-ettenmoors" "Across the Ettenmoors"
 where
  -- its setup card's Forced, a safe location guarding each side quest stages 2A and 3A
  -- reveal, is theirs; its stage 1 replaces the standard one
  nightmare =
    nightmareDeck
      Nightmare.acrossTheEttenmoorsNightmare
      EncounterSet.AcrossTheEttenmoorsNightmare
      [ (Quests.intoTheEttenmoors, 1)
      , (Locations.shroudedHills, 2)
      , (Treacheries.noRest, 1)
      , (Treacheries.aradorsBane, 2)
      , (Locations.trollFells, 2)
      , (Enemies.goblinPursuer, 2)
      , (Treacheries.coldFromAngmar, 2)
      ]

-- | The encounter deck is built from the Across the Ettenmoors, Eriador Wilds and Foul
-- Weather sets. The safe keyword's rules are kept by the safe locations themselves.
instance RunMessage AcrossTheEttenmoors where
  runMessage msg (AcrossTheEttenmoors attrs) = case msg of
    DoStep 1 Setup -> do
      encounterDeck <-
        gatherEncounterSets attrs [EncounterSet.AcrossTheEttenmoors, EncounterSet.EriadorWilds, EncounterSet.FoulWeather]
      firstStage <- genEncounterCard $ nightmareOr attrs Quests.intoTheEttenmoors Quests.intoTheEttenmoorsNightmare
      laterStages <- traverse genEncounterCard [Quests.aMiserableJourney, Quests.journeysEnd]
      putQuestIntoPlay firstStage
      pure $ AcrossTheEttenmoors $ attrs & encounterDeckL .~ encounterDeck & questDeckL .~ laterStages
    -- Campaign: Setup: Set all 5 Losing Time treacheries aside, out of play.
    DoStep 9 Setup | isJust attrs.campaign -> do
      angmarSetup attrs
      losingTime <- replicateM 5 (genEncounterCard Treacheries.losingTime)
      AcrossTheEttenmoors <$> runMessage msg (attrs & setAsideL %~ (<> losingTime))
    -- Forced: At the beginning of the quest phase, place 1 resource on this card. Then, if
    -- there are 3 or more resources on it, remove 3 resources from it to reveal 1 set-aside
    -- copy of Losing Time and place it in the encounter discard pile.
    -- The resources on the campaign card are the scenario's meta. It answers the phase's
    -- window, before the players commit.
    CheckWindows ws | isJust attrs.campaign, Window After (PhaseBegins QuestPhase) `elem` ws -> do
      let resources = 1 + fromMaybe (0 :: Int) (parseMaybe parseJSON attrs.scenarioMeta)
          aside = [c | c <- attrs.scenarioSetAside, toCardCode c == toCardCode Treacheries.losingTime]
      case aside of
        card : _ | resources >= 3 -> do
          push $ RevealCard card
          AcrossTheEttenmoors <$> runMessage msg (attrs & metaL .~ toJSON (resources - 3) & setAsideL %~ filter (/= card))
        _ -> AcrossTheEttenmoors <$> runMessage msg (attrs & metaL .~ toJSON resources)
    -- Resolution: Add each copy of Losing Time in the encounter deck or encounter discard
    -- pile to the Campaign Pool.
    ResolveCampaign | Just l <- attrs.campaign -> do
      let losingTime = [c | c <- attrs.encounterDeck <> attrs.encounterDiscard, toCardCode c == toCardCode Treacheries.losingTime]
          l' = foldl' (\l'' c -> addToPool (PoolCard (toCardCode c) Nothing Nothing) l'') l losingTime
      AcrossTheEttenmoors <$> runMessage msg (attrs {scenarioCampaign = Just l'})
    _ -> AcrossTheEttenmoors <$> runMessage msg attrs
