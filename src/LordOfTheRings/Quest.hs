{-# OPTIONS_GHC -Wno-orphans #-}

module LordOfTheRings.Quest (module LordOfTheRings.Quest, module LordOfTheRings.Quest.Type) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card
import LordOfTheRings.Enemy.Cards qualified as Enemies
import LordOfTheRings.Frontend
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Game.Base
import LordOfTheRings.Id
import LordOfTheRings.Target
import LordOfTheRings.Location.Cards qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Type
import LordOfTheRings.Question
import LordOfTheRings.Classes.HasQueue
import LordOfTheRings.Classes.RunMessage

getLead :: (HasGame m) => m PlayerId
getLead = (.id) <$> selectJust ActivePlayer

instance RunMessage Quest where
  runMessage msg (Quest a) = Quest <$> runMessage msg a

newtype FliesAndSpiders = FliesAndSpiders QuestAttrs
  deriving newtype (Show, ToJSON)

instance IsQuest FliesAndSpiders
instance RunMessage FliesAndSpiders where
  runMessage msg s@(FliesAndSpiders a) = case msg of
    DoStep 7 Setup -> do
      lead <- getLead
      push $ Ask lead $ ChooseOne [targetChoice a [Flip $ toTarget a]]
      pure s
    Flip (isTarget a -> True) -> do
      lead <- getLead
      send $ FlipCard a.card.id
      pushAll
        [ Search lead EncounterDeck (cardIs Enemies.forestSpider) ToStaging Shuffle
        , Search lead EncounterDeck (cardIs Locations.oldForestRoad) ToStaging Shuffle
        ]
      pure $ FliesAndSpiders $ a {side = B}
    _ -> FliesAndSpiders <$> runMessage msg a

instance RunMessage QuestAttrs where
  runMessage msg attrs = case msg of
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
