module LordOfTheRings.Frontend where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Prelude

data FrontendMessage = Log {message :: Text} | Noop
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

class (Monad m) => Frontend m where
  send :: FrontendMessage -> m ()
