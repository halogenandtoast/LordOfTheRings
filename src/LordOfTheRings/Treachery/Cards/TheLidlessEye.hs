module LordOfTheRings.Treachery.Cards.TheLidlessEye (theLidlessEye) where

import LordOfTheRings.Scenario.Scenarios.TheMountainOfFire.Helpers (theOneRing)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheLidlessEye = TheLidlessEye TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theLidlessEye :: TreacheryCard TheLidlessEye
theLidlessEye = treachery TheLidlessEye Cards.theLidlessEye

-- | Peril.
-- When Revealed: Make a Fortitude test. If The One Ring is exhausted, add 2 Eye icons to
-- the test. If you fail this test, raise your threat by 10.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if you failed a Fortitude test
-- this round).
instance RunMessage TheLidlessEye where
  runMessage msg t@(TheLidlessEye attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      pid <- getRevealer
      exhausted <- maybe False (.exhausted) <$> theOneRing
      fortitudeTestWith pid attrs (\ft -> ft {fortitudeEyes = if exhausted then 2 else 0}) (encounterRaisesThreat pid 10) (pure ())
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      failed <- failedFortitudeTestThisRound pid
      enemyGetsAttack attrs eid (if failed then 2 else 1)
      pure t
    _ -> TheLidlessEye <$> runMessage msg attrs
