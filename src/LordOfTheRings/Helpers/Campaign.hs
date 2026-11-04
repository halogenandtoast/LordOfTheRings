-- | What a card reads of the campaign log in campaign mode, as the Dream-chaser
-- campaign's scalable and upgradable cards do.
module LordOfTheRings.Helpers.Campaign (
  getCampaignLog,
  campaignSide,
  campaignLevel,
  campaignUpgraded,
  campaignPartNumber,
  campaignCardResources,
  templeSecrets,
  getTempleSecrets,
) where

import LordOfTheRings.Campaign
import LordOfTheRings.Card.CardDef (CardDef)
import LordOfTheRings.Card.Code (CardCode, HasCardCode (..))
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Game.Base (gameScenario)
import LordOfTheRings.Location.Grid (Pos (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs, scenarioCampaign, scenarioId)

getCampaignLog :: HasGame m => m (Maybe CampaignLog)
getCampaignLog = getsGame (scenarioCampaign . toAttrs . gameScenario)

-- | The side of a card the game plays: in campaign mode its Campaign Mode only side, as
-- the Dream-chaser campaign turns the Stormcaller over to it.
campaignSide :: HasGame m => CardDef -> CardDef -> m CardDef
campaignSide standard side = maybe standard (const side) <$> getCampaignLog

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

{- | Temple of the Deceived's secrets still facedown, each by where the Temple of the
Deceived location it lies under is on the Island Map: the rightmost column (the sixth in
nightmare mode, the fifth otherwise), the secrets recorded row by row.
-}
templeSecrets :: ScenarioAttrs -> [(Pos, CardCode)]
templeSecrets attrs =
  [ (Pos column r, code)
  | Just l <- [attrs.campaign]
  , (r, code) <- zip [0 ..] (recorded Secrets l)
  , code /= ""
  ]
 where
  column = if attrs.nightmare then 5 else 4

getTempleSecrets :: HasGame m => m [(Pos, CardCode)]
getTempleSecrets = getsGame (templeSecrets . toAttrs . gameScenario)
