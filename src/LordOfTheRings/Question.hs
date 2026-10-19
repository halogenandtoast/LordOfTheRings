{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LordOfTheRings.Question (module LordOfTheRings.Question) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.Card (Card)
import LordOfTheRings.Id
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Window

data UI msg
  = Label {label :: Text, messages :: [msg]}
  | TargetLabel {target :: Target, messages :: [msg]}
  | AbilityLabel {player :: PlayerId, ability :: Ability, windows :: [Window], messages :: [msg]}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Question msg
  = ChooseOne {choices :: [UI msg]}
  | ChooseOneAtATime {choices :: [UI msg]}
  | -- | A prompt, the card causing the decision and the cards it is aimed at.
    QuestionLabel {label :: Text, source :: Maybe Target, targets :: [Target], question :: Question msg}
  | -- | The cards a search looked through, shown to the player choosing from them.
    SearchCards {cards :: [Card], question :: Question msg}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

questionChoices :: Question msg -> [UI msg]
questionChoices = \case
  ChooseOne cs -> cs
  ChooseOneAtATime cs -> cs
  QuestionLabel _ _ _ q -> questionChoices q
  SearchCards _ q -> questionChoices q

targetLabel :: (Targetable a) => a -> [msg] -> UI msg
targetLabel (toTarget -> target) messages = TargetLabel {target, messages}
