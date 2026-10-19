module LordOfTheRings.Json (aesonOptions, camelCase) where

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
