module LordOfTheRings.Location.Cards.BelfalasIslet (belfalasIslet) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (notGuarding)

newtype BelfalasIslet = BelfalasIslet LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

belfalasIslet :: LocationCard BelfalasIslet
belfalasIslet = location BelfalasIslet Cards.belfalasIslet

-- | You cannot travel to Belfalas Islet if you are off-course.
instance HasModifiersFor BelfalasIslet where
  getModifiersFor (BelfalasIslet a) = whenM isOffCourse $ modified a a [CannotBeTraveledTo]

-- | Response: After you travel to Belfalas Islet, choose a Ship enemy that is not
-- guarding an objective. While Belfalas Islet is the active location, the chosen enemy is
-- considered to be out of play.
instance HasAbilities BelfalasIslet where
  getAbilities (BelfalasIslet a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

instance RunMessage BelfalasIslet where
  runMessage msg l@(BelfalasIslet a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      ships <- notGuarding =<< select shipEnemy
      -- out of play only while its source, the active location, is in play
      chooseOrRunOneM pid do
        questionLabeled "belfalasIslet.choose"
        causedBy a
        targets ships \e -> createEffect a e [ConsideredOutOfPlay, Blank] UntilSpent
      pure l
    _ -> BelfalasIslet <$> runMessage msg a
