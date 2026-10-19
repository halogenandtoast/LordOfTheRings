module LordOfTheRings.Game.Public (publicGame) where

import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Game.Base
import LordOfTheRings.Prelude

-- | The game as the players may see it: decks are hidden information, so only their
-- sizes are sent. Stored games and exports keep everything.
publicGame :: Game -> Value
publicGame = hideDecks . toJSON
 where
  hideDecks = \case
    Object o -> Object $ adjust (overObjects hidePlayer) "players" $ adjust hideScenario "scenario" o
    v -> v
  hidePlayer = withSize "deck" "deckSize"
  hideScenario = withSize "questDeck" "questDeckSize" . withSize "encounterDeck" "encounterDeckSize"
  withSize key sizeKey = \case
    Object o | Just (Array cards) <- KeyMap.lookup key o ->
      Object $ KeyMap.insert sizeKey (toJSON $ length cards) $ KeyMap.delete key o
    v -> v
  adjust f k o = maybe o (\v -> KeyMap.insert k (f v) o) (KeyMap.lookup k o)
  overObjects f = \case
    Object o -> Object $ fmap f o
    v -> v
