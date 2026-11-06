module LordOfTheRings.Location.Cards.TheMastersManor (theMastersManor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (lakeTownObjectives, smaugEnemies)

newtype TheMastersManor = TheMastersManor LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theMastersManor :: LocationCard TheMastersManor
theMastersManor = location TheMastersManor Cards.theMastersManor

instance RunMessage TheMastersManor where
  runMessage msg l@(TheMastersManor a) = case msg of
    WhenRevealed (isTarget a -> True) -> do
      towns <- lakeTownObjectives
      enemies <- smaugEnemies
      for_ enemies \eid -> do
        damage <- getDamage (toTarget eid)
        for_ towns \oid -> moveDamage a eid oid damage
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ pid _) | cid == a.cardId -> l <$ discardHand pid
    _ -> TheMastersManor <$> runMessage msg a
