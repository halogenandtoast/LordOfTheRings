module LordOfTheRings.Treachery.Cards.TheDeadRideBehind (theDeadRideBehind) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (NightTime (..), currentNight)

newtype TheDeadRideBehind = TheDeadRideBehind TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theDeadRideBehind :: TreacheryCard TheDeadRideBehind
theDeadRideBehind = treachery TheDeadRideBehind Cards.theDeadRideBehind

instance RunMessage TheDeadRideBehind where
  runMessage msg t@(TheDeadRideBehind a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      time <- currentNight
      let limit = case time of Just Midnight -> 4; Just Dusk -> 3; _ -> 2
      characters <- selectCharacters AnyHero AnyAlly
      affected <- filterM (fmap (< limit) . getWillpower) characters
      for_ affected \target -> dealDamage a target 1
      pure t
    _ -> TheDeadRideBehind <$> runMessage msg a
