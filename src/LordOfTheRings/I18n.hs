-- | Text for the players goes out as translation keys, as in Arkham: a @$@, a dotted key
-- into the client's locale files, and its variables inline,
--
-- > $log.takesDamage name=s:"Gimli" __name=s:"01004" count=i:2
--
-- The client looks the key up in the player's language. A card's name travels with its
-- code (the @__@ variable), so the client can show the name in that language too.
module LordOfTheRings.I18n (
  I18nVar,
  ikey,
  ikeyWith,
  intVar,
  textVar,
  nameVar,
  cardVar,
) where

import Data.Text qualified as T
import LordOfTheRings.Card (CardCode (..), CardDef (..), HasCardDef (..), display)
import LordOfTheRings.Prelude

data I18nValue = IntValue Int | TextValue Text

type I18nVar = (Text, I18nValue)

ikey :: Text -> Text
ikey key = ikeyWith key []

ikeyWith :: Text -> [I18nVar] -> Text
ikeyWith key vars = unwords ("$" <> key : map render vars)
 where
  render (k, IntValue n) = k <> "=i:" <> tshow n
  -- only the quote and backslash are escaped: `show` would mangle names like Glóin
  render (k, TextValue t) = k <> "=s:\"" <> T.concatMap escape t <> "\""
  escape = \case
    '"' -> "\\\""
    '\\' -> "\\\\"
    c -> T.singleton c

intVar :: Text -> Int -> I18nVar
intVar k n = (k, IntValue n)

textVar :: Text -> Text -> I18nVar
textVar k t = (k, TextValue t)

-- | A name the engine knows only as text, such as a player's.
nameVar :: Text -> Text -> [I18nVar]
nameVar k t = [textVar k t]

-- | A card's name, with its code so the client can show it in the player's language.
cardVar :: (HasCardDef a) => Text -> a -> [I18nVar]
cardVar k a = [textVar k (display $ cdName def), textVar ("__" <> k) (unCardCode $ cdCardCode def)]
 where
  def = toCardDef a
