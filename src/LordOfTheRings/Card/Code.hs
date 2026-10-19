module LordOfTheRings.Card.Code (CardCode (..), HasCardCode (..)) where

import LordOfTheRings.Prelude

newtype CardCode = CardCode {unCardCode :: Text}
  deriving newtype (Ord, Eq, IsString, Show, ToJSON, FromJSON, ToJSONKey, FromJSONKey)

class HasCardCode a where
  toCardCode :: a -> CardCode

instance HasCardCode CardCode where
  toCardCode = id
