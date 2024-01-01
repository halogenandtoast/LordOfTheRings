module LordOfTheRings.Window (module LordOfTheRings.Window) where

import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Timing = After
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Window = Window {timing :: Timing, kind :: WindowKind}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowKind = EnterPlay Target
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
