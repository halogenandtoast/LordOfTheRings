module LordOfTheRings.Location.Cards.OrcPassage (orcPassage) where

import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype OrcPassage = OrcPassage LocationAttrs
  deriving anyclass IsLocation
  deriving newtype (Show, Eq, ToJSON, Entity)

orcPassage :: LocationCard OrcPassage
orcPassage = location OrcPassage Cards.orcPassage

-- | While Orc Passage is in the staging area, encounter card effects cannot be canceled.
instance HasModifiersFor OrcPassage where
  getModifiersFor (OrcPassage a) =
    when (a.placement == StagingArea) $ modified a GameTarget [CannotCancelEncounterEffects, WhenRevealedCannotBeCanceled, ShadowCannotBeCanceled]

-- | Forced: When Orc Passage is explored, search the encounter deck and discard pile for a
-- copy of Torech Gorgor Patrol and add it to the staging area. The first player searches.
instance HasAbilities OrcPassage where
  getAbilities (OrcPassage a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage OrcPassage where
  runMessage msg l@(OrcPassage attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      lead <- getLead
      searchEncounterDeckAndDiscard lead attrs (cardIs Enemies.torechGorgorPatrol)
      pure l
    _ -> OrcPassage <$> runMessage msg attrs
