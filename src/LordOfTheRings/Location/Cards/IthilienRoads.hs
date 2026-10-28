-- | The Blood of Gondor's four roads out of the Cross-roads.
module LordOfTheRings.Location.Cards.IthilienRoads (
  southernRoad,
  westernRoad,
  northernRoad,
  easternRoad,
) where

import LordOfTheRings.Card (cdCardCode, toCardCode)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)

newtype IthilienRoad = IthilienRoad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Travel: The first player must take 1 hidden card to travel here.
southernRoad :: LocationCard IthilienRoad
southernRoad = location IthilienRoad Cards.southernRoad

-- | Western Road gets +5 quest points while it is in the staging area.
-- Travel: The first player must take 1 hidden card to travel here.
westernRoad :: LocationCard IthilienRoad
westernRoad = location IthilienRoad Cards.westernRoad

-- | Travel: The first player must take 1 hidden card to travel here.
northernRoad :: LocationCard IthilienRoad
northernRoad = location IthilienRoad Cards.northernRoad

-- | Eastern Road gets +5 quest points while it is in the staging area.
-- Travel: The first player must take 2 hidden cards to travel here.
easternRoad :: LocationCard IthilienRoad
easternRoad = location IthilienRoad Cards.easternRoad

isRoad :: LocationAttrs -> [CardDef] -> Bool
isRoad a = elem (toCardCode a) . map cdCardCode

instance HasModifiersFor IthilienRoad where
  getModifiersFor (IthilienRoad a) =
    when (isRoad a [Cards.westernRoad, Cards.easternRoad] && a.placement == StagingArea)
      $ modified a a [QuestPointModifier 5]

instance RunMessage IthilienRoad where
  runMessage msg l@(IthilienRoad a) = case msg of
    TravelTo lid | lid == a.id -> do
      lead <- getLead
      replicateM_ (if isRoad a [Cards.easternRoad] then 2 else 1) $ takeHiddenCard lead
      pure l
    _ -> IthilienRoad <$> runMessage msg a
