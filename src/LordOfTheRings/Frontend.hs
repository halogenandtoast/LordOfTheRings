{-# OPTIONS_GHC -Wno-partial-fields #-}

module LordOfTheRings.Frontend (module LordOfTheRings.Frontend) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Queue

data FrontendMessage
  = Log {message :: Text}
  | Announce {message :: Text}
  | FlipCard {cardId :: CardId}
  | -- | cards shuffled back into a deck, which may be drawn straight out again
    ShuffleIntoDeck {cardIds :: [CardId]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (Monad m) => Frontend m where
  send :: FrontendMessage -> m ()

instance (Frontend m) => Frontend (QueueT msg m) where
  send = lift . send
