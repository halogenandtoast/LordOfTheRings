module LordOfTheRings.Location.Cards.RollingSeas (rollingSeas) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype RollingSeas = RollingSeas LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

rollingSeas :: LocationCard RollingSeas
rollingSeas = location RollingSeas Cards.rollingSeas

-- | Travel: Shift your heading off-course to travel here.
-- Shadow: If this attack deals damage to a Ship objective, shift your heading off-course.
instance RunMessage RollingSeas where
  runMessage msg l@(RollingSeas a) = case msg of
    TravelTo lid | lid == a.id -> do
      shiftHeadingOffCourse a
      pure l
    -- read off the damage the attack deals, once it is over
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ships <- map toTarget <$> select shipObjective
      afterAttack eid [IfAttackDamaged eid ships [ShiftHeading (CardIdSource cid) ShiftOffCourse]]
      pure l
    _ -> RollingSeas <$> runMessage msg a
