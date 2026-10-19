module LordOfTheRings.Quest.Cards.AnduinPassage (anduinPassage) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype AnduinPassage = AnduinPassage QuestAttrs
  deriving anyclass (IsQuest, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

anduinPassage :: QuestCard AnduinPassage
anduinPassage = quest AnduinPassage Cards.anduinPassage
