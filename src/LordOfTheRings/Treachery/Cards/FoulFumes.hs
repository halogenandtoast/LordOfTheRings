module LordOfTheRings.Treachery.Cards.FoulFumes (foulFumes) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype FoulFumes = FoulFumes TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

foulFumes :: TreacheryCard FoulFumes
foulFumes = treachery FoulFumes Cards.foulFumes

-- | When Revealed: Each player makes a Fortitude test. If you fail this test, deal 1 damage
-- to each character you control. Until the end of the round, treat the text box of each
-- damaged character as if it were blank (except for Traits): the characters this effect
-- damaged, read once the damage is dealt.
-- Shadow: If you failed a Fortitude test this round, this attack is considered undefended.
instance RunMessage FoulFumes where
  runMessage msg t@(FoulFumes attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \pid -> fortitudeTest pid attrs (doStep 1 (WithTarget (PlayerTarget pid) msg)) (pure ())
      pure t
    -- the characters he controls as he fails
    DoStep 1 (WithTarget (PlayerTarget pid) (WhenRevealed (isTarget attrs -> True))) -> do
      characters <- charactersControlledBy pid
      for_ characters \c -> dealDamage attrs c 1
      doStep 2 msg
      pure t
    DoStep 2 (DoStep 1 (WithTarget _ (WhenRevealed (isTarget attrs -> True)))) -> do
      damaged <- damageDealtBy attrs
      for_ (ordNub [c | (c, n) <- damaged, n > 0]) \c -> roundModifier attrs c Blank
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      failed <- failedFortitudeTestThisRound pid
      when failed $ makeUndefended pid eid
      pure t
    _ -> FoulFumes <$> runMessage msg attrs
