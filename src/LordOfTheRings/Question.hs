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
  | -- | A window whose choices hang on the game as it stands: when the game changes under
    -- it, the message runs again for fresh choices instead of the question being re-asked.
    PlayerWindow {rebuild :: msg, question :: Question msg}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

questionChoices :: Question msg -> [UI msg]
questionChoices = \case
  ChooseOne cs -> cs
  ChooseOneAtATime cs -> cs
  QuestionLabel _ _ _ q -> questionChoices q
  SearchCards _ q -> questionChoices q
  PlayerWindow _ q -> questionChoices q

-- | The message that builds the question afresh, if it is a window that can.
questionRebuild :: Question msg -> Maybe msg
questionRebuild = \case
  QuestionLabel _ _ _ q -> questionRebuild q
  SearchCards _ q -> questionRebuild q
  PlayerWindow msg _ -> Just msg
  _ -> Nothing

targetLabel :: (Targetable a) => a -> [msg] -> UI msg
targetLabel (toTarget -> target) messages = TargetLabel {target, messages}
