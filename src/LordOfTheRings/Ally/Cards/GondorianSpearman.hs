module LordOfTheRings.Ally.Cards.GondorianSpearman (gondorianSpearman) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted

newtype GondorianSpearman = GondorianSpearman AllyAttrs
  deriving anyclass (IsAlly, HasAbilities, HasModifiersFor, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

gondorianSpearman :: AllyCard GondorianSpearman
gondorianSpearman = ally GondorianSpearman Cards.gondorianSpearman
