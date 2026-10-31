module LordOfTheRings.Treachery.Cards.TheThievesPloy (theThievesPloy) where

import LordOfTheRings.Scenario.Scenarios.PerilInPelargir.Helpers (getScroll, scrollHolder)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheThievesPloy = TheThievesPloy TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theThievesPloy :: TreacheryCard TheThievesPloy
theThievesPloy = treachery TheThievesPloy Cards.theThievesPloy

-- | Surge.
-- When Revealed: If Alcaron's Scroll is attached to a hero, choose an enemy engaged with
-- that hero's controller, if able. Attach Alcaron's Scroll to that enemy, then add 1
-- progress to that enemy.
instance RunMessage TheThievesPloy where
  runMessage msg t@(TheThievesPloy attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      holder <- scrollHolder
      scroll <- getScroll
      for_ ((,) <$> holder <*> scroll) \(pid, oid) -> do
        enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
        chooseOrRunOneM pid do
          questionLabeled "theThievesPloy.enemy"
          causedBy attrs
          targets enemies \e -> do
            attachObjective oid e
            placeProgress attrs e 1
      pure t
    _ -> TheThievesPloy <$> runMessage msg attrs
