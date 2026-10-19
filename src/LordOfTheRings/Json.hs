module LordOfTheRings.Json (aesonOptions, camelCase, withDefaults) where

import Data.Aeson.KeyMap qualified as KeyMap
import Data.Char qualified as C
import LordOfTheRings.Prelude

aesonOptions :: Maybe String -> Options
aesonOptions ms = defaultOptions {fieldLabelModifier = camelCase . drop len}
 where
  len = maybe 0 length ms

camelCase :: String -> String
camelCase = \case
  (c : cs) -> C.toLower c : cs
  [] -> []

-- | Fill in fields a stored object predates, so it still decodes after a field is added.
withDefaults :: [(Key, Value)] -> Value -> Value
withDefaults defaults = \case
  Object o -> Object $ foldr (\(k, v) -> KeyMap.insertWith (\_ old -> old) k v) o defaults
  v -> v
