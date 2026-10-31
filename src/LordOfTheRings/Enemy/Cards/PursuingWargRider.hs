module LordOfTheRings.Enemy.Cards.PursuingWargRider (pursuingWargRider) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (InPlayArea))
import LordOfTheRings.Scenario.Scenarios.ReturnToMirkwood.Helpers (getGollum)

newtype PursuingWargRider = PursuingWargRider EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

pursuingWargRider :: EnemyCard PursuingWargRider
pursuingWargRider = enemy PursuingWargRider Cards.pursuingWargRider

-- | Cannot be optionally engaged.
instance HasModifiersFor PursuingWargRider where
  getModifiersFor (PursuingWargRider a) = modifySelf a [CannotBeOptionallyEngagedBy AnyPlayer]

-- | Forced: After the player guarding Gollum changes, Pursuing Warg-rider engages the
-- player guarding Gollum and makes an immediate attack against that player.
-- Gollum is placed with a player only as the guard changes.
instance RunMessage PursuingWargRider where
  runMessage msg (PursuingWargRider attrs) = case msg of
    PlaceObjective oid (InPlayArea pid) -> do
      gollum <- (== Just oid) <$> getGollum
      when gollum do
        engageEnemy pid attrs.id
        immediateAttack pid attrs.id
      PursuingWargRider <$> runMessage msg attrs
    _ -> PursuingWargRider <$> runMessage msg attrs
