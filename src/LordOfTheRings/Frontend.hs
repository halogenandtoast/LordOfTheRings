{-# OPTIONS_GHC -Wno-partial-fields #-}

module LordOfTheRings.Frontend (module LordOfTheRings.Frontend) where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Id
import LordOfTheRings.Prelude

data FrontendMessage = Log {message :: Text} | FlipCard {cardId :: CardId}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (Monad m) => Frontend m where
  send :: FrontendMessage -> m ()
