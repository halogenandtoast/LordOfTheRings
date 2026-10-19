module LordOfTheRings.Treachery.Cards.TheNecromancersReach (theNecromancersReach) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheNecromancersReach = TheNecromancersReach TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, RunMessage)
  deriving newtype (Show, Eq, ToJSON, Entity)

theNecromancersReach :: TreacheryCard TheNecromancersReach
theNecromancersReach = treachery TheNecromancersReach Cards.theNecromancersReach
