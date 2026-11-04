module LordOfTheRings.Treachery.Cards.RageAndHunger (rageAndHunger) where

import LordOfTheRings.Scenario.Scenarios.ShelobsLair.Helpers (getShelob, shelob)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RageAndHunger = RageAndHunger TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rageAndHunger :: TreacheryCard RageAndHunger
rageAndHunger = treachery RageAndHunger Cards.rageAndHunger

-- | When Revealed: Shelob gets +2 attack until the end of the phase. Then, she makes an
-- immediate attack against each player, in player order.
-- Shadow: If attacking enemy is Shelob, she makes an additional attack against you after
-- this one.
instance RunMessage RageAndHunger where
  runMessage msg t@(RageAndHunger attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      getShelob >>= traverse_ \e -> do
        phaseModifier attrs e (AttackModifier 2)
        eachPlayer \p -> immediateAttack p e
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      isShelob <- selectAny (EnemyWithId eid <> shelob)
      when isShelob $ additionalAttack pid eid
      pure t
    _ -> RageAndHunger <$> runMessage msg attrs
