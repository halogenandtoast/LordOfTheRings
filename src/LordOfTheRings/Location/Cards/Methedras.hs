module LordOfTheRings.Location.Cards.Methedras (methedras) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.ToCatchAnOrc.Helpers (searches)

newtype Methedras = Methedras LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

methedras :: LocationCard Methedras
methedras = location Methedras Cards.methedras

-- | While Methedras is the active location, each location in the staging area gets +1
-- threat.
instance HasModifiersFor Methedras where
  getModifiersFor (Methedras a) = when (a.placement == ActiveLocation) do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ThreatModifier 1]

-- | Forced: When Methedras leaves play as an explored location, each player Searches 3.
instance HasAbilities Methedras where
  getAbilities (Methedras a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage Methedras where
  runMessage msg l@(Methedras a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \p -> searches p 3
      pure l
    _ -> Methedras <$> runMessage msg a
