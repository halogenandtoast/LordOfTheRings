module LordOfTheRings.Treachery.Cards.TerrorDrivesThem (terrorDrivesThem) where

import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower, towerProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TerrorDrivesThem = TerrorDrivesThem TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

terrorDrivesThem :: TreacheryCard TerrorDrivesThem
terrorDrivesThem = treachery TerrorDrivesThem Cards.terrorDrivesThem

-- | Surge.
-- When Revealed: Place 1 progress token on To the Tower.
-- Shadow: Attacking enemy gets +1 attack for each progress token on To the Tower.
instance RunMessage TerrorDrivesThem where
  runMessage msg t@(TerrorDrivesThem attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeOnTower attrs 1
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid =<< towerProgress
      pure t
    _ -> TerrorDrivesThem <$> runMessage msg attrs
