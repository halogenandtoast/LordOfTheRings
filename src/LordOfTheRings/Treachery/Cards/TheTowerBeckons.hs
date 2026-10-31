module LordOfTheRings.Treachery.Cards.TheTowerBeckons (theTowerBeckons) where

import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (placeOnTower, towerProgress)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheTowerBeckons = TheTowerBeckons TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTowerBeckons :: TreacheryCard TheTowerBeckons
theTowerBeckons = treachery TheTowerBeckons Cards.theTowerBeckons

-- | When Revealed: Place 1 progress token on To the Tower. Until the end of the phase, The
-- Tower Beckons adds X threat to the total threat in the staging area, where X is the
-- number of progress tokens on To the Tower.
-- Shadow: Place 1 progress token on To the Tower.
instance RunMessage TheTowerBeckons where
  runMessage msg t@(TheTowerBeckons attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeOnTower attrs 1
      doStep 1 msg
      pure t
    -- ponytail: X is counted once the token is placed, not kept up as the tower's count
    -- changes later this phase
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      x <- towerProgress
      when (x > 0) $ phaseModifier attrs GameTarget (StagingThreatBonus x)
      pure t
    ResolveShadowEffect cid _ _ _ | cid == attrs.cardId -> do
      placeOnTower attrs 1
      pure t
    _ -> TheTowerBeckons <$> runMessage msg attrs
