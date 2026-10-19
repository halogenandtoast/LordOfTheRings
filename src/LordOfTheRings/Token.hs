module LordOfTheRings.Token (module LordOfTheRings.Token) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Prelude

data Token = Damage | Progress | Resource
  deriving stock (Show, Eq, Ord, Generic, Enum, Bounded)
  deriving anyclass (ToJSON, FromJSON)

instance ToJSONKey Token where
  toJSONKey = genericToJSONKey defaultJSONKeyOptions

instance FromJSONKey Token where
  fromJSONKey = genericFromJSONKey defaultJSONKeyOptions

type Tokens = Map Token Int

countTokens :: Token -> Tokens -> Int
countTokens = Map.findWithDefault 0

addTokens :: Token -> Int -> Tokens -> Tokens
addTokens token amount tokens =
  let val = max 0 (countTokens token tokens + amount)
   in if val == 0 then Map.delete token tokens else Map.insert token val tokens

subtractTokens :: Token -> Int -> Tokens -> Tokens
subtractTokens token amount = addTokens token (negate amount)

removeAllTokens :: Token -> Tokens -> Tokens
removeAllTokens = Map.delete
