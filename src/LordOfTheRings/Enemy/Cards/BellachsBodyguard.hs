module LordOfTheRings.Enemy.Cards.BellachsBodyguard (bellachsBodyguard) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (Unplaced))
import LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (getBellach, isScour)

newtype BellachsBodyguard = BellachsBodyguard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

bellachsBodyguard :: EnemyCard BellachsBodyguard
bellachsBodyguard = enemy BellachsBodyguard Cards.bellachsBodyguard

-- | Bellach cannot take damage.
instance HasModifiersFor BellachsBodyguard where
  getModifiersFor (BellachsBodyguard a) = getBellach >>= traverse_ \b -> modified a b [CannotTakeDamage]

-- | Scour: Heal all damage from Bellach's Bodyguard. From the discard pile it has none.
-- Shadow: Attacking enemy cannot take damage until the end of the phase.
instance RunMessage BellachsBodyguard where
  runMessage msg e@(BellachsBodyguard attrs) = case msg of
    _ | isScour attrs msg -> do
      unless (attrs.placement == Unplaced) $ healAllDamage attrs attrs.id
      pure e
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      phaseModifier attrs eid CannotTakeDamage
      pure e
    _ -> BellachsBodyguard <$> runMessage msg attrs
