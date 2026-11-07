module LordOfTheRings.Scenario.Scenarios.MurderAtThePrancingPony.Helpers
  (Meta (..), emptyMeta, getMeta, investigateValue, lookAtInvestigation, lookAtBottom, suspects, hideouts) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardCode, CardDef, EncounterCard, toCardDef)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Investigate))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher (getLocation, keywordValue)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types (ScenarioAttrs)
import LordOfTheRings.Source
import LordOfTheRings.Target

data Meta = Meta
  { investigationDeck :: [EncounterCard]
  , hidden :: [EncounterCard]
  , removed :: [EncounterCard]
  , known :: Map PlayerId [CardCode]
  , lastLook :: Map PlayerId [EncounterCard]
  , accusationCorrect :: Maybe Bool
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

emptyMeta :: Meta
emptyMeta = Meta [] [] [] mempty mempty Nothing

getMeta :: ScenarioAttrs -> Meta
getMeta a = fromMaybe emptyMeta $ parseMaybe parseJSON a.meta

suspects, hideouts :: [CardDef]
suspects = [Enemies.toddTheTroll, Enemies.susieTheSpider, Enemies.wolfPeltJake, Enemies.johnnyGoblinFingers, Enemies.oldOrcEyes]
hideouts = [Locations.billFernysHouse, Locations.staddleHidingHole, Locations.combeStorehouse, Locations.archetTradingPost, Locations.chetwoodCampsite]

investigateValue :: HasGame m => LocationId -> m Int
investigateValue lid = do
  location <- getLocation lid
  keywordValue (\case Investigate n -> Just n; _ -> Nothing) (toTarget lid) (toCardDef location)

lookAtInvestigation :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> Int -> m ()
lookAtInvestigation pid source n = push $ LookAtInvestigationDeck pid (toSource source) n

lookAtBottom :: (ReverseQueue m, Sourceable source) => PlayerId -> source -> m ()
lookAtBottom pid source = push $ LookAtBottomOfInvestigationDeck pid (toSource source)
