module LordOfTheRings.Treachery.Cards.EnemiesCloseBehind (enemiesCloseBehind) where

import LordOfTheRings.Scenario.Scenarios.TheRiverRunning.Helpers (placeOnEasterlingPursuit, pursuitResources)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype EnemiesCloseBehind = EnemiesCloseBehind TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

enemiesCloseBehind :: TreacheryCard EnemiesCloseBehind
enemiesCloseBehind = treachery EnemiesCloseBehind Cards.enemiesCloseBehind

{- | When Revealed: Place 1 resource on Easterling Pursuit. Then, raise each player's threat
by 1 for each resource on Easterling Pursuit, counted once that one is placed.
Shadow: Attacking enemy gets +1 Attack. If this attack destroys a character, place 1
resource on Easterling Pursuit.
-}
instance RunMessage EnemiesCloseBehind where
  runMessage msg t@(EnemiesCloseBehind attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      placeOnEasterlingPursuit attrs 1
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      n <- pursuitResources
      eachPlayer \p -> encounterRaisesThreat p n
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      enemyGetsAttack attrs eid 1
      ifThisAttackDestroys eid msg
      pure t
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == attrs.cardId -> do
      placeOnEasterlingPursuit attrs 1
      pure t
    _ -> EnemiesCloseBehind <$> runMessage msg attrs
