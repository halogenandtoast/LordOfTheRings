{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LordOfTheRings.Question (module LordOfTheRings.Question) where

import {-# SOURCE #-} LordOfTheRings.Ability
import LordOfTheRings.Target
import LordOfTheRings.Prelude

targetChoice :: (Targetable a) => a -> [msg] -> Choice msg
targetChoice (toTarget -> target) msgs = TargetChoice {target, messages = msgs}

data Choice msg
  = Label {label :: Text, messages :: [msg]}
  | TargetChoice {target :: Target, messages :: [msg]}
  | UseAbilityLabel {ability :: Ability}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Question msg = ChooseOne [Choice msg] | ChooseOneAtATime [Choice msg]
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
