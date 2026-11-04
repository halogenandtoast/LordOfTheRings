module LordOfTheRings.Location.Cards.ShiveringRiver (shiveringRiver) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ShiveringRiver = ShiveringRiver LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

shiveringRiver :: LocationCard ShiveringRiver
shiveringRiver = location ShiveringRiver Cards.shiveringRiver

-- | While Shivering River is in the staging area, it contributes its [Threat] to the total
-- [Threat] in each other player's staging area: Foundations of Stone's scenario adds it to
-- each other area's, as the players are separated.
-- Shadow: Attacking enemy gets +1 [Attack]. (+3 [Attack] instead if there is more than one
-- player at this staging area.)
instance RunMessage ShiveringRiver where
  runMessage msg l@(ShiveringRiver a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      players <- length <$> getRemainingPlayers
      enemyGetsAttack a eid (if players > 1 then 3 else 1)
      pure l
    _ -> ShiveringRiver <$> runMessage msg a
