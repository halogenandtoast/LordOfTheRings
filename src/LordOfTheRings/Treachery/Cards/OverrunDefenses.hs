module LordOfTheRings.Treachery.Cards.OverrunDefenses (overrunDefenses) where

import LordOfTheRings.Scenario.Scenarios.AssaultOnOsgiliath.Helpers (controlledLocations)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OverrunDefenses = OverrunDefenses TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

overrunDefenses :: TreacheryCard OverrunDefenses
overrunDefenses = treachery OverrunDefenses Cards.overrunDefenses

-- | When Revealed: Add X to the total threat in the staging area this phase. X is the
-- number of locations the players control. If X is less than 4, Overrun Defenses gains
-- doomed 2.
-- Shadow: Exhaust a character you control (2 characters instead if you control 4 or more
-- locations).
instance RunMessage OverrunDefenses where
  runMessage msg t@(OverrunDefenses attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- length <$> select (LocationControlledBy AnyPlayer)
      when (x > 0) $ phaseModifier attrs GameTarget (StagingThreatBonus x)
      when (x < 4) $ raiseThreatFromDoomed 2
      pure t
    ResolveShadowEffect cid _ pid _ | cid == attrs.cardId -> do
      controlled <- length <$> controlledLocations pid
      chooseAndExhaust attrs pid (if controlled >= 4 then 2 else 1)
      pure t
    _ -> OverrunDefenses <$> runMessage msg attrs
