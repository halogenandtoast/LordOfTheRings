module LordOfTheRings.Card.PlayerCard (module LordOfTheRings.Card.PlayerCard) where

import LordOfTheRings.Card.Code
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Prelude

data PlayerCard = MkPlayerCard
  { pcId :: CardId
  , pcCardCode :: CardCode
  , pcOwner :: Maybe PlayerId
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON PlayerCard where
  toJSON = genericToJSON (aesonOptions $ Just "pc")

instance FromJSON PlayerCard where
  parseJSON = genericParseJSON (aesonOptions $ Just "pc")

instance HasCardCode PlayerCard where
  toCardCode = pcCardCode

instance HasField "id" PlayerCard CardId where
  getField = pcId

instance HasField "owner" PlayerCard (Maybe PlayerId) where
  getField = pcOwner
