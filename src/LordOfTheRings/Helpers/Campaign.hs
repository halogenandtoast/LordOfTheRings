-- | What a card reads of the campaign log in campaign mode, as the Dream-chaser
-- campaign's scalable and upgradable cards do.
module LordOfTheRings.Helpers.Campaign (
  getCampaignLog,
  campaignLevel,
  campaignUpgraded,
  campaignPartNumber,
  campaignCardResources,
) where

import LordOfTheRings.Campaign
import LordOfTheRings.Card.Code (CardCode, HasCardCode (..))
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (scenarioCampaign, scenarioId)

getCampaignLog :: HasGame m => m (Maybe CampaignLog)
getCampaignLog = getsGame (scenarioCampaign . toAttrs . gameScenario)

-- | The card's level, 1 outside campaign mode.
campaignLevel :: (HasGame m, HasCardCode c) => c -> m Int
campaignLevel c = maybe 1 (level $ toCardCode c) <$> getCampaignLog

-- | Every one of the card's checkboxes marked.
campaignUpgraded :: HasGame m => CardCode -> m Bool
campaignUpgraded code = maybe False (isUpgraded code) <$> getCampaignLog

-- | The campaign card's part number: the scenario's place in its campaign, from 1.
campaignPartNumber :: HasGame m => m Int
campaignPartNumber = do
  sid <- getsGame (scenarioId . toAttrs . gameScenario)
  maybe 0 (`partNumber` sid) <$> getCampaignLog

campaignCardResources :: HasGame m => m Int
campaignCardResources = maybe 0 (.cardResources) <$> getCampaignLog
