{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-partial-fields #-}

module LordOfTheRings.Question (module LordOfTheRings.Question) where

import Data.Aeson
import GHC.Generics
import LordOfTheRings.Id
import LordOfTheRings.Prelude

data Choice msg
  = Label {label :: Text, messages :: [msg]}
  | CardChoice {cardId :: CardId, messages :: [msg]}
  | HeroChoice {heroId :: HeroId, messages :: [msg]}
  | QuestChoice {questId :: QuestId, messages :: [msg]}
  deriving stock (Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Question msg = ChooseOne [Choice msg] | ChooseOneAtATime [Choice msg]
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

choiceMessages :: Choice msg -> [msg]
choiceMessages = \case
  Label _ msgs -> msgs
  CardChoice _ msgs -> msgs
  HeroChoice _ msgs -> msgs
  QuestChoice _ msgs -> msgs
