module LordOfTheRings.Location.Cards.RollingSeas (rollingSeas) where

import LordOfTheRings.Helpers.Character (getDefenseAgainst)
import LordOfTheRings.Helpers.Staging (getEnemyAttack)
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
    ResolveShadowEffect cid eid pid defenders | cid == a.cardId -> do
      ships <- map toTarget <$> shipObjectivesOf pid
      attack <- getEnemyAttack eid
      defense <- sum <$> traverse (getDefenseAgainst eid) defenders
      shipEnemyAttack <- selectAny (EnemyWithId eid <> shipEnemy)
      let onShip = if null defenders then shipEnemyAttack && notNull ships else any (`elem` ships) defenders
      when (onShip && attack > defense) $ afterAttack eid [ShiftHeading (CardIdSource cid) ShiftOffCourse]
      pure l
    _ -> RollingSeas <$> runMessage msg a
