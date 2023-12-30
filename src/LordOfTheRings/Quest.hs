{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Quest (module LordOfTheRings.Quest, module LordOfTheRings.Quest.Type) where

import Control.Monad.Random
import Data.Aeson
import Data.Map.Strict qualified as Map
import Data.Maybe (fromMaybe)
import LordOfTheRings.Card
import LordOfTheRings.Enemy.Cards qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Game.Base
import LordOfTheRings.Id
import LordOfTheRings.Location.Cards qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Type
import LordOfTheRings.Question
import LordOfTheRings.Queue
import LordOfTheRings.Run

getLead :: (HasGame m) => m PlayerId
getLead = (.id) <$> selectJust ActivePlayer

instance Run Quest where
  run msg (Quest a) = Quest <$> run msg a

newtype FliesAndSpiders = FliesAndSpiders QuestAttrs
  deriving newtype (Show, ToJSON)

instance IsQuest FliesAndSpiders
instance Run FliesAndSpiders where
  run msg s@(FliesAndSpiders a) = case msg of
    DoStep 7 Setup -> do
      lead <- getLead
      push $ Ask lead $ ChooseOne [QuestChoice a.id [FlipQuest a.id]]
      pure s
    FlipQuest qId | a.id == qId -> do
      lead <- getLead
      send $ FlipCard a.card.id
      pushAll
        [ Search lead EncounterDeck (cardIs Enemies.forestSpider) ToStaging Shuffle
        , Search lead EncounterDeck (cardIs Locations.oldForestRoad) ToStaging Shuffle
        ]
      pure $ FliesAndSpiders $ a {side = B}
    _ -> FliesAndSpiders <$> run msg a

instance Run QuestAttrs where
  run msg attrs = case msg of
    _ -> pure attrs

lookupQuest :: CardCode -> QuestAttrs -> Quest
lookupQuest cCode = fromMaybe (error "No such quest") $ Map.lookup cCode allData
 where
  allData =
    Map.fromList
      [ ("01119", Quest . FliesAndSpiders)
      ]

newQuest :: (MonadRandom m) => EncounterCard 'QuestKind -> m Quest
newQuest card = do
  questId <- getRandom
  let attrs = QuestAttrs {id = questId, card, side = A}
  pure $ lookupQuest card.def.code attrs
