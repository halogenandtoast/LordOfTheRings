module LordOfTheRings.Treachery.Cards.ChaosInTheCavern (chaosInTheCavern) where

import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Goblin))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype ChaosInTheCavern = ChaosInTheCavern TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

chaosInTheCavern :: TreacheryCard ChaosInTheCavern
chaosInTheCavern = treachery ChaosInTheCavern Cards.chaosInTheCavern

{- | When Revealed: All engaged enemies return to the staging area. Then, each Goblin enemy
gets +1 threat until the end of the phase.
Shadow: attacking enemy is returned to the staging area after its attack resolves.
-}
instance RunMessage ChaosInTheCavern where
  runMessage msg t@(ChaosInTheCavern attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      engaged <- select (EnemyEngagedWith AnyPlayer)
      for_ engaged \e -> placeEnemy e StagingArea
      goblins <- select (EnemyWithTrait Goblin)
      for_ goblins \g -> phaseModifier attrs g (ThreatModifier 1)
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      returnToStagingAfterAttack eid
      pure t
    _ -> ChaosInTheCavern <$> runMessage msg attrs
