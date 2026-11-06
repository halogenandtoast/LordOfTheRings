module LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Meta (Meta (..), metaOf, withMeta, isSmaugCard, discardSmaug) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card
import LordOfTheRings.EncounterSet (EncounterSet (TheBattleOfLakeTown))
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Types

-- The Smaug deck and discard are separate from encounter-card search effects.
data Meta = Meta {smaugDeck :: [EncounterCard], smaugDiscard :: [EncounterCard]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

metaOf :: ScenarioAttrs -> Meta
metaOf a = fromMaybe (Meta [] []) $ parseMaybe parseJSON a.scenarioMeta

withMeta :: Meta -> ScenarioAttrs -> ScenarioAttrs
withMeta m a = a {scenarioMeta = toJSON m}

isSmaugCard :: EncounterCard -> Bool
isSmaugCard card = cdEncounterSet def == Just TheBattleOfLakeTown && cdCardType def == EnemyType && "Smaug" `isInfixOf` title
 where
  def = toCardDef card
  Name title _ = cdName def

discardSmaug :: EncounterCard -> ScenarioAttrs -> ScenarioAttrs
discardSmaug card a = withMeta (m {smaugDiscard = card : smaugDiscard m}) a
 where m = metaOf a
