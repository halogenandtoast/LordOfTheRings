module LordOfTheRings.Scenario.Scenarios.TheUrukHai (theUrukHai) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Campaign (CampaignLogUpdate (RemoveFromPool), updateLog)
import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.EncounterSet qualified as EncounterSet
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheUrukHai.Helpers (pursuitOf)

newtype TheUrukHai = TheUrukHai ScenarioAttrs
  deriving anyclass (IsScenario, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theUrukHai :: Scenario
theUrukHai = Scenario $ scenario TheUrukHai "the-uruk-hai" "The Uruk-hai"

{- | The Uruk-hai, Orcs of the White Hand and Snaga Orcs make up the encounter deck (MEC45
rules). Uglúk, To Isengard! and a copy of Eastemnet are set aside for stage 1A's setup.

The pursuit tracker is the scenario's: its value is kept in the meta, set to 10 as stage 1A
reads, and moved by 'increasePursuit', never below 0. Each stage's quest points are the
pursuit value, so each move has the stage checked again, as one with as much progress as
its quest points is defeated. Whether the Uruk-hai reach Isengard To Isengard! checks at
the end of the round, as its Forced effect reads.
-}
instance RunMessage TheUrukHai where
  runMessage msg (TheUrukHai attrs) = case msg of
    DoStep 1 Setup -> do
      cards <- gatherEncounterSets attrs [EncounterSet.TheUrukHai, EncounterSet.OrcsOfTheWhiteHand, EncounterSet.SnagaOrcs]
      let aside = take 1 (filter (isCard Enemies.ugluk) cards) <> take 1 (filter (isCard Objectives.toIsengard) cards) <> take 1 (filter (isCard Locations.eastemnet) cards)
          deck = filter (`notElem` aside) cards
      firstStage <- genEncounterCard Quests.followTheOrcs
      secondStage <- genEncounterCard Quests.rescueTheCaptives
      putQuestIntoPlay firstStage
      pure
        $ TheUrukHai
        $ attrs
        & encounterDeckL .~ deck
        & setAsideL .~ aside
        & questDeckL .~ [secondStage]
        & metaL .~ object ["pursuit" .= (0 :: Int)]
        -- Campaign: Setup: Remove Gildor Inglorion and Mr. Underhill from the Campaign
        -- Pool, before its cards are dealt in. (Each burden of the Shadow of the Past,
        -- Flight to the Ford and The Ring Goes South burden sets is left out of the
        -- encounter deck: see 'removedBurdenSets'.)
        & campaignL %~ fmap (updateLog (RemoveFromPool gildorInglorion) . updateLog (RemoveFromPool mrUnderhill))
    ScenarioSpecific "urukHai.pursuit" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      let p = max 0 (pursuitOf attrs.meta + n)
      checkStages
      pure $ TheUrukHai $ attrs & metaL .~ object ["pursuit" .= p]
    ScenarioSpecific "urukHai.setPursuit" (parseMaybe parseJSON -> Just (n :: Int)) -> do
      checkStages
      pure $ TheUrukHai $ attrs & metaL .~ object ["pursuit" .= max 0 n]
    _ -> TheUrukHai <$> runMessage msg attrs
   where
    isCard def = (== toCardCode def) . toCardCode
    -- The Black Riders' boons, Gildor Inglorion the objective ally and Mr. Underhill
    gildorInglorion = "141077"
    mrUnderhill = "141017"
    checkStages = traverse_ (checkStage . toId) =<< getsGame (toList . (.entities.quests))
