module LordOfTheRings.Treachery.Cards.MassingAtOsgiliath (massingAtOsgiliath) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMassingAtOsgiliath.Helpers (crossedAnduin)

newtype MassingAtOsgiliath = MassingAtOsgiliath TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

massingAtOsgiliath :: TreacheryCard MassingAtOsgiliath
massingAtOsgiliath = treachery MassingAtOsgiliath Cards.massingAtOsgiliath

instance RunMessage MassingAtOsgiliath where
  runMessage msg t@(MassingAtOsgiliath attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      crossed <- crossedAnduin
      phaseModifier attrs GameTarget (RevealedEncounterCardsGainDoomed $ if crossed then 3 else 1)
      pure t
    _ -> MassingAtOsgiliath <$> runMessage msg attrs
