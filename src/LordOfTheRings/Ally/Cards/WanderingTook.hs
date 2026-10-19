module LordOfTheRings.Ally.Cards.WanderingTook (wanderingTook) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype WanderingTook = WanderingTook AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

wanderingTook :: AllyCard WanderingTook
wanderingTook = ally WanderingTook Cards.wanderingTook
