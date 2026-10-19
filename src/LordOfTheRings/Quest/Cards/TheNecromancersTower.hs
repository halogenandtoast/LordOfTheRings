module LordOfTheRings.Quest.Cards.TheNecromancersTower (theNecromancersTower) where

import LordOfTheRings.Quest.CardDefs qualified as Cards
import LordOfTheRings.Quest.Import.Lifted

newtype TheNecromancersTower = TheNecromancersTower QuestAttrs
  deriving anyclass (IsQuest, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theNecromancersTower :: QuestCard TheNecromancersTower
theNecromancersTower = quest TheNecromancersTower Cards.theNecromancersTower
