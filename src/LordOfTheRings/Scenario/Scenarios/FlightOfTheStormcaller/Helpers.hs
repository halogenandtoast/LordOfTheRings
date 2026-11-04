{- | What the cards of Flight of the Stormcaller share: the Stormcaller's area, with its own
quest deck of stages 2C, 3C and 4C, and the progress the Stormcaller places on its stage.

Cards in the Stormcaller's area are placed 'StormcallerArea': not in the staging area, and
immune to player card effects (the Stormcaller's text gives that). Once the players reach
the Stormcaller's stage, the scenario moves them to the staging area, where they stay; the
Stormcaller returns to its area as it advances.
-}
module LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (
  Meta (..),
  stormcallerSide,
  selectStormcaller,
  getMeta,
  getStormcallerMeta,
  getStormcallerStage,
  getPlayersStage,
  atStormcallersStage,
  stageOf,
  placeStormcallerProgress,
  moveProgressToStormcaller,
  addToStormcallerArea,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (EncounterCard, cdStage, toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (ReverseQueue, removeTokens)
import LordOfTheRings.Placement (Placement (StormcallerArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Card.CardDef (CardDef)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Helpers.Campaign (campaignSide)
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithCardCode), select)
import LordOfTheRings.Quest.Types (isSideQuest)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

-- | The Stormcaller's quest stage, the stages under it, and the progress on it.
data Meta = Meta
  { stormcallerStage :: Maybe EncounterCard
  , stormcallerDeck :: [EncounterCard]
  , stormcallerProgress :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta Nothing [] 0) $ parseMaybe parseJSON attrs.meta

getStormcallerMeta :: HasGame m => m Meta
getStormcallerMeta = getsGame (getMeta . toAttrs . (.scenario))

stageOf :: EncounterCard -> Int
stageOf = fromMaybe 0 . cdStage . toCardDef

-- | The Stormcaller the game plays: in campaign mode its Campaign Mode only side.
stormcallerSide :: HasGame m => m CardDef
stormcallerSide = campaignSide Enemies.stormcaller Enemies.stormcallerCampaign

-- | The Stormcaller in play.
selectStormcaller :: HasGame m => m [EnemyId]
selectStormcaller = select . EnemyWithCardCode . toCardCode =<< stormcallerSide

-- | The Stormcaller's stage number, 0 before its area is prepared.
getStormcallerStage :: HasGame m => m Int
getStormcallerStage = maybe 0 stageOf . (.stormcallerStage) <$> getStormcallerMeta

-- | The players' stage number.
getPlayersStage :: HasGame m => m Int
getPlayersStage =
  getsGame (fromMaybe 0 . headMay . mapMaybe (cdStage . toCardDef) . filter (not . isSideQuest) . toList . (.entities.quests))

-- | The players are at the same stage as the Stormcaller.
atStormcallersStage :: HasGame m => m Bool
atStormcallersStage = do
  stormcaller <- getStormcallerStage
  players <- getPlayersStage
  pure $ stormcaller > 0 && stormcaller == players

-- | Progress placed on the Stormcaller's quest stage.
placeStormcallerProgress :: ReverseQueue m => Int -> m ()
placeStormcallerProgress n = when (n > 0) $ push $ ScenarioSpecific "stormcaller.progress" (toJSON n)

-- | Up to this much progress moves from the quest to the Stormcaller's quest stage.
moveProgressToStormcaller :: (ReverseQueue m, Sourceable source) => source -> Target -> Int -> Int -> m ()
moveProgressToStormcaller source quest progress n = do
  let moved = min n progress
  when (moved > 0) do
    removeTokens source quest Progress moved
    placeStormcallerProgress moved

-- | The enemy or location goes to the Stormcaller's area: to the staging area, while the
-- players are at the Stormcaller's stage.
addToStormcallerArea :: ReverseQueue m => Target -> m ()
addToStormcallerArea target = unlessM atStormcallersStage case target of
  EnemyTarget eid -> push $ PlaceEnemy eid StormcallerArea
  LocationTarget lid -> push $ PlaceLocation lid StormcallerArea
  _ -> pure ()
