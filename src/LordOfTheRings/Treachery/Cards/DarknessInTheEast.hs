module LordOfTheRings.Treachery.Cards.DarknessInTheEast (darknessInTheEast) where

import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DarknessInTheEast = DarknessInTheEast TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

darknessInTheEast :: TreacheryCard DarknessInTheEast
darknessInTheEast = treachery DarknessInTheEast Cards.darknessInTheEast

-- | When Revealed: Raise each player's threat by X, where X is the active location's
-- Threat. If there is no active location, Darkness in the East gains surge.
-- Shadow: Attacking enemy gets +1 Attack (+2 Attack instead if you raised your threat this
-- round).
instance RunMessage DarknessInTheEast where
  runMessage msg t@(DarknessInTheEast attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      select IsActiveLocation >>= \case
        lid : _ -> do
          x <- getLocationThreat lid
          eachPlayer \p -> encounterRaisesThreat p x
        [] -> revealEncounterCard
      pure t
    ResolveShadowEffect cid eid pid _ | cid == attrs.cardId -> do
      raised <- elem RaisedThreatThisRound <$> getModifiers (PlayerTarget pid)
      enemyGetsAttack attrs eid (if raised then 2 else 1)
      pure t
    _ -> DarknessInTheEast <$> runMessage msg attrs
