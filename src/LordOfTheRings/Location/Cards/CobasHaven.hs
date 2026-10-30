module LordOfTheRings.Location.Cards.CobasHaven (cobasHaven) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype CobasHaven = CobasHaven LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

cobasHaven :: LocationCard CobasHaven
cobasHaven = location CobasHaven Cards.cobasHaven

-- | While Cobas Haven is the active location, reduce the Boarding X value on each Ship
-- enemy by 1.
instance HasModifiersFor CobasHaven where
  getModifiersFor (CobasHaven a) = when (a.placement == ActiveLocation) do
    ships <- select shipEnemy
    for_ ships \s -> modified a s [BoardingModifier (-1)]

-- | Shadow: If you are off-course, attacking enemy gets +2 attack.
instance RunMessage CobasHaven where
  runMessage msg l@(CobasHaven a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      whenM isOffCourse $ enemyGetsAttack (CardIdSource cid) eid 2
      pure l
    _ -> CobasHaven <$> runMessage msg a
