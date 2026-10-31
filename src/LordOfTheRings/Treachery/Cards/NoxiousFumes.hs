module LordOfTheRings.Treachery.Cards.NoxiousFumes (noxiousFumes) where

import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower, towerProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype NoxiousFumes = NoxiousFumes TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

noxiousFumes :: TreacheryCard NoxiousFumes
noxiousFumes = treachery NoxiousFumes Cards.noxiousFumes

-- | When Revealed: Place 1 progress token on To the Tower. Each player must assign damage
-- among characters he controls equal to the number of progress tokens on To the Tower.
-- Shadow: If this attack destroys a character, place 1 progress on To the Tower.
instance RunMessage NoxiousFumes where
  runMessage msg t@(NoxiousFumes attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeOnTower attrs 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      x <- towerProgress
      when (x > 0) $ eachPlayer \pid -> dealDamageDivided attrs pid x
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeOnTower attrs 1
      pure t
    _ -> NoxiousFumes <$> runMessage msg attrs
