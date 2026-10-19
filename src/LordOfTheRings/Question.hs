{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LordOfTheRings.Question (module LordOfTheRings.Question) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Window

data UI msg
  = Label {label :: Text, messages :: [msg]}
  | TargetLabel {target :: Target, messages :: [msg]}
  | AbilityLabel {player :: PlayerId, ability :: Ability, windows :: [Window], messages :: [msg]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

data Question msg
  = ChooseOne {choices :: [UI msg]}
  | ChooseOneAtATime {choices :: [UI msg]}
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON)

questionChoices :: Question msg -> [UI msg]
questionChoices = \case
  ChooseOne cs -> cs
  ChooseOneAtATime cs -> cs

targetLabel :: (Targetable a) => a -> [msg] -> UI msg
targetLabel (toTarget -> target) messages = TargetLabel {target, messages}
