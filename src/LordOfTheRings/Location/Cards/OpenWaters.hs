module LordOfTheRings.Location.Cards.OpenWaters (openWaters) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype OpenWaters = OpenWaters LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

openWaters :: LocationCard OpenWaters
openWaters = location OpenWaters Cards.openWaters

readyShips :: AllyMatcher
readyShips = shipObjective <> ReadyAlly

-- | While Open Waters is in the staging area, each Ship enemy gets -5 engagement cost and
-- +1 threat.
-- Travel: One player must exhaust a Ship objective to travel here.
instance HasModifiersFor OpenWaters where
  getModifiersFor (OpenWaters a) = do
    able <- selectAny readyShips
    modified a a [CannotBeTraveledTo | not able]
    when (a.placement == StagingArea) do
      ships <- select shipEnemy
      for_ ships \e -> modified a e [EngagementCostModifier (-5), ThreatModifier 1]

instance RunMessage OpenWaters where
  runMessage msg l@(OpenWaters a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      ships <- select readyShips
      chooseOrRunOneM lead do
        questionLabeled "openWaters.exhaust"
        causedBy a
        targets ships exhaust
      pure l
    _ -> OpenWaters <$> runMessage msg a
