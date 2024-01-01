{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LordOfTheRings.Question (module LordOfTheRings.Question) where

import Data.Aeson
import GHC.Generics
import {-# SOURCE #-} LordOfTheRings.Ability
import LordOfTheRings.Id
import LordOfTheRings.Prelude

targetChoice :: (Reference a) => a -> [msg] -> Choice msg
targetChoice (toRef -> target) msgs = TargetChoice {target, messages = msgs}

data Choice msg
  = Label {label :: Text, messages :: [msg]}
  | TargetChoice {target :: Target, messages :: [msg]}
  | UseAbilityLabel {ability :: Ability}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Question msg = ChooseOne [Choice msg] | ChooseOneAtATime [Choice msg]
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
