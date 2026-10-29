module LordOfTheRings.Treachery.Cards.ConspicuousLot (conspicuousLot) where

import LordOfTheRings.Scenario.Scenarios.TroubleInTharbad.Helpers (anyThreatAtMost20, attackingEnemyGetsBonus)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ConspicuousLot = ConspicuousLot TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

conspicuousLot :: TreacheryCard ConspicuousLot
conspicuousLot = treachery ConspicuousLot Cards.conspicuousLot

-- | When Revealed: Each player raises his threat by 1 for each ally he controls. Then, if
-- any player's threat is 20 or less, this card gains Surge.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending player's threat
-- is 20 or less).
instance RunMessage ConspicuousLot where
  runMessage msg t@(ConspicuousLot attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      eachPlayer \p -> do
        allies <- length <$> select (AllyControlledBy $ PlayerWithId p)
        encounterRaisesThreat p allies
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      whenM anyThreatAtMost20 revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      attackingEnemyGetsBonus attrs eid pid
      pure t
    _ -> ConspicuousLot <$> runMessage msg attrs
