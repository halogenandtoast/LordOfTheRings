module LordOfTheRings.Treachery.Cards.TheNecromancersReach (theNecromancersReach) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheNecromancersReach = TheNecromancersReach TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theNecromancersReach :: TreacheryCard TheNecromancersReach
theNecromancersReach = treachery TheNecromancersReach Cards.theNecromancersReach

instance RunMessage TheNecromancersReach where
  runMessage msg t@(TheNecromancersReach attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      exhausted <- selectCharacters ExhaustedHero ExhaustedAlly
      for_ exhausted \c -> dealDamage attrs c 1
      pure t
    _ -> TheNecromancersReach <$> runMessage msg attrs
