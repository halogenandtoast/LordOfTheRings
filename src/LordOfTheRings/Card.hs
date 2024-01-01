{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE NoFieldSelectors #-}

module LordOfTheRings.Card (
  module LordOfTheRings.Card,
  module X,
) where

import LordOfTheRings.Card.Code as X
import LordOfTheRings.Card.EncounterCard as X hiding (unique)
import LordOfTheRings.Card.PlayerCard as X hiding (unique)
import LordOfTheRings.Card.PlayerCard qualified as PlayerCard
import LordOfTheRings.Id
import LordOfTheRings.Name
import LordOfTheRings.Prelude hiding (id)

class Display a where
  display :: a -> Text

instance Display Name where
  display (Name title mSubtitle) = case mSubtitle of
    Nothing -> title
    Just subtitle -> title <> ": " <> subtitle

data Card = PlayerCard SomePlayerCard | EncounterCard SomeEncounterCard
  deriving stock (Show, Generic)

instance Reference Card where
  toRef = \case
    PlayerCard pc -> CardRef pc.id
    EncounterCard ec -> CardRef ec.id

instance ToJSON Card where
  toJSON = \case
    PlayerCard pc -> toJSON pc
    EncounterCard ec -> toJSON ec

instance HasField "id" Card CardId where
  getField = \case
    PlayerCard pc -> pc.id
    EncounterCard ec -> ec.id

instance HasField "name" Card Name where
  getField = \case
    PlayerCard pc -> pc.name
    EncounterCard ec -> ec.name

instance HasField "unique" Card Bool where
  getField = \case
    PlayerCard pc -> pc.unique
    EncounterCard ec -> ec.unique

instance HasField "sphere" Card (Maybe Sphere) where
  getField = \case
    PlayerCard pc -> Just pc.sphere
    EncounterCard _ -> Nothing

instance HasField "cost" Card (Maybe ResourceCost) where
  getField = cardCost

data SomeCardDef = SomePlayerCardDef SomePlayerCardDef | SomeEncounterCardDef SomeEncounterCardDef

cardCost :: Card -> Maybe ResourceCost
cardCost (PlayerCard pc) = playerCardCost pc
cardCost _ = Nothing

costAsResources :: ResourceCost -> Int
costAsResources = \case
  StaticCost n -> n
  DynamicCost -> 0

unique :: PlayerCardDef k -> PlayerCardDef k
unique def = def {PlayerCard.unique = True}
