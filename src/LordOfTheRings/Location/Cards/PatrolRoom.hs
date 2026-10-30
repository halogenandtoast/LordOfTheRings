module LordOfTheRings.Location.Cards.PatrolRoom (patrolRoom) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype PatrolRoom = PatrolRoom LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Capture 2, which the scenario resolves.
patrolRoom :: LocationCard PatrolRoom
patrolRoom = location PatrolRoom Cards.patrolRoom

-- | Travel: Each player raises his threat by 3 to travel here.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if the defending player's
-- threat is 20 or higher).
instance RunMessage PatrolRoom where
  runMessage msg l@(PatrolRoom a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer (`raiseThreat` 3)
      PatrolRoom <$> runMessage msg a
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      threat <- (.threat) <$> getPlayer pid
      enemyGetsAttack a eid (if threat >= 20 then 2 else 1)
      pure l
    _ -> PatrolRoom <$> runMessage msg a
