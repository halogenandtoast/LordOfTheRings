{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Player.Types (module LordOfTheRings.Player.Types) where

import LordOfTheRings.Card
import LordOfTheRings.Classes.Entity
import LordOfTheRings.Id
import LordOfTheRings.Json
import Data.Aeson.KeyMap qualified as KeyMap
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

data Player = Player
  { playerId :: PlayerId
  , playerName :: Text
  , playerHand :: [Card]
  , playerDeck :: [Card]
  , playerDiscard :: [Card]
  , playerThreat :: Int
  , playerEliminated :: Bool
  }
  deriving stock (Show, Eq, Generic)

makeLensesWith suffixedFields ''Player

-- | The deck is hidden information, so only its size is sent.
instance ToJSON Player where
  toJSON p = case genericToJSON (aesonOptions $ Just "player") p of
    Object o -> Object $ KeyMap.insert "deckSize" (toJSON $ length p.playerDeck) $ KeyMap.delete "deck" o
    v -> v

newPlayer :: PlayerId -> Text -> [Card] -> Player
newPlayer pid name deck =
  Player
    { playerId = pid
    , playerName = name
    , playerHand = []
    , playerDeck = deck
    , playerDiscard = []
    , playerThreat = 0
    , playerEliminated = False
    }

instance Entity Player where
  type EntityId Player = PlayerId
  type EntityAttrs Player = Player
  toId = playerId
  toAttrs a = a
  overAttrs f a = f a

instance Targetable Player where
  toTarget = PlayerTarget . playerId

instance Sourceable Player where
  toSource = PlayerSource . playerId

instance HasField "id" Player PlayerId where
  getField = playerId

instance HasField "name" Player Text where
  getField = playerName

instance HasField "hand" Player [Card] where
  getField = playerHand

instance HasField "deck" Player [Card] where
  getField = playerDeck

instance HasField "discard" Player [Card] where
  getField = playerDiscard

instance HasField "threat" Player Int where
  getField = playerThreat

instance HasField "eliminated" Player Bool where
  getField = playerEliminated
