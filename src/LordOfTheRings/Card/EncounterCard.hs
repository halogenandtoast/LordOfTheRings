module LordOfTheRings.Card.EncounterCard (module LordOfTheRings.Card.EncounterCard) where

import LordOfTheRings.Card.Code
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Prelude

data EncounterCard = MkEncounterCard
  { ecId :: CardId
  , ecCardCode :: CardCode
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON EncounterCard where
  toJSON = genericToJSON (aesonOptions $ Just "ec")

instance FromJSON EncounterCard where
  parseJSON = genericParseJSON (aesonOptions $ Just "ec")

instance HasCardCode EncounterCard where
  toCardCode = ecCardCode

instance HasField "id" EncounterCard CardId where
  getField = ecId
