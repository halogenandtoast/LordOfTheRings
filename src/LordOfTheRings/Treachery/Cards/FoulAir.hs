module LordOfTheRings.Treachery.Cards.FoulAir (foulAir) where

import LordOfTheRings.Scenario.Scenarios.TheLongDark.Helpers (locateTest, triggerLostEffects)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FoulAir = FoulAir TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

foulAir :: TreacheryCard FoulAir
foulAir = treachery FoulAir Cards.foulAir

-- | When Revealed: The first player makes a locate test. If this test is failed, deal 2
-- damage to all characters and trigger all "Lost:" effects in play.
instance RunMessage FoulAir where
  runMessage msg t@(FoulAir attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      characters <- selectCharacters AnyHero AnyAlly
      locateTest attrs (for_ characters (\c -> dealDamage attrs c 2) >> triggerLostEffects attrs) (pure ())
      pure t
    _ -> FoulAir <$> runMessage msg attrs
