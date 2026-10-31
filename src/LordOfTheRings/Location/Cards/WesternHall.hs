module LordOfTheRings.Location.Cards.WesternHall (westernHall) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheSeventhLevel.Helpers (addResourceToEachGoblin, resourcesOn)
import LordOfTheRings.Trait (Trait (Goblin))

newtype WesternHall = WesternHall LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

westernHall :: LocationCard WesternHall
westernHall = location WesternHall Cards.westernHall

-- | Each Goblin enemy gets +1 defense for each resource on it. This ability does not stack
-- with other copies of Western Hall: the first copy in play gives it.
instance HasModifiersFor WesternHall where
  getModifiersFor (WesternHall a) = do
    firstCopy <- selectNone (LocationWithCardCode (toCardCode a) <> LocationWithIdBelow a.id)
    when firstCopy do
      goblins <- select (EnemyWithTrait Goblin)
      for_ goblins \g -> do
        n <- resourcesOn g
        when (n > 0) $ modified a g [DefenseModifier n]

-- | Travel: Add 1 resource to each Goblin enemy in play.
instance RunMessage WesternHall where
  runMessage msg (WesternHall a) = case msg of
    TravelTo lid | lid == a.id -> do
      addResourceToEachGoblin a
      WesternHall <$> runMessage msg a
    _ -> WesternHall <$> runMessage msg a
