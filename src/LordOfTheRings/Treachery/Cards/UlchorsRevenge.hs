module LordOfTheRings.Treachery.Cards.UlchorsRevenge (ulchorsRevenge) where

import LordOfTheRings.Helpers.ControlledLocation (controlledLocations)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype UlchorsRevenge = UlchorsRevenge TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ulchorsRevenge :: TreacheryCard UlchorsRevenge
ulchorsRevenge = treachery UlchorsRevenge Cards.ulchorsRevenge

-- | Doomed 1.
-- When Revealed: Thane Ulchor makes an immediate attack against each player that controls
-- a location (in turn order).
-- Shadow: Attacking enemy gets +1 attack for each location you control.
instance RunMessage UlchorsRevenge where
  runMessage msg t@(UlchorsRevenge attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      thanes <- select thaneUlchor
      eachPlayer \pid -> do
        controls <- notNull <$> controlledLocations pid
        when controls $ for_ thanes (immediateAttack pid)
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      n <- length <$> controlledLocations pid
      when (n > 0) $ enemyGetsAttack attrs eid n
      pure t
    _ -> UlchorsRevenge <$> runMessage msg attrs
