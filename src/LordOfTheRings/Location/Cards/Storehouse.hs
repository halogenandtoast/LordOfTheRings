module LordOfTheRings.Location.Cards.Storehouse (storehouse) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype Storehouse = Storehouse LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

storehouse :: LocationCard Storehouse
storehouse = location Storehouse Cards.storehouse

-- | Underworld 1.
-- Shadow: Attacking enemy gets +1 attack (+2 attack instead if undefended.)
instance RunMessage Storehouse where
  runMessage msg l@(Storehouse a) = case msg of
    ResolveShadowEffect cid eid _ defenders | cid == a.cardId -> do
      enemyGetsAttack a eid (ifUndefended defenders 2 1)
      pure l
    _ -> Storehouse <$> runMessage msg a
