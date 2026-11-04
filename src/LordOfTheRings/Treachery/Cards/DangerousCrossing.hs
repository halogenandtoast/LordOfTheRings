module LordOfTheRings.Treachery.Cards.DangerousCrossing (dangerousCrossing) where

import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype DangerousCrossing = DangerousCrossing TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

dangerousCrossing :: TreacheryCard DangerousCrossing
dangerousCrossing = treachery DangerousCrossing Cards.dangerousCrossing

-- | When Revealed: Each player assigns X damage among characters he controls, where X is
-- the total threat of the active location. If X is less than 3, Dangerous Crossing gains
-- surge.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending character is
-- damaged).
instance RunMessage DangerousCrossing where
  runMessage msg t@(DangerousCrossing attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      x <- sum <$> (traverse getLocationThreat =<< select IsActiveLocation)
      when (x > 0) $ eachPlayer \p -> dealDamageDivided attrs p x
      when (x < 3) revealEncounterCard
      pure t
    ResolveShadowEffect cid eid _ defenders | cid == attrs.cardId -> do
      damaged <- anyM (fmap (> 0) . getDamage) defenders
      enemyGetsAttack attrs eid (if damaged then 2 else 1)
      pure t
    _ -> DangerousCrossing <$> runMessage msg attrs
