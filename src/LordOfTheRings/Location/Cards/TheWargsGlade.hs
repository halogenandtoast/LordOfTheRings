module LordOfTheRings.Location.Cards.TheWargsGlade (theWargsGlade) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Creature))

newtype TheWargsGlade = TheWargsGlade LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theWargsGlade :: LocationCard TheWargsGlade
theWargsGlade = location TheWargsGlade Cards.theWargsGlade

-- | Forced: After a character takes damage from an attack made by a Creature enemy, remove
-- 1 progress from the current quest. Once for each character so damaged.
-- (Its riddle is answered by the scenario.)
instance RunMessage TheWargsGlade where
  runMessage msg (TheWargsGlade attrs) = case msg of
    CheckWindows ws -> do
      creatures <- select (EnemyWithTrait Creature)
      for_ [() | Window After (DamagedByEnemyAttack _ eid) <- ws, eid `elem` creatures] \_ ->
        removeProgressFromQuest attrs 1
      TheWargsGlade <$> runMessage msg attrs
    _ -> TheWargsGlade <$> runMessage msg attrs
