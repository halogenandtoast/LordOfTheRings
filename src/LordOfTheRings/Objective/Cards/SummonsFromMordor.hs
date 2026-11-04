module LordOfTheRings.Objective.Cards.SummonsFromMordor (summonsFromMordor) where

import LordOfTheRings.Objective.CardDefs qualified as Cards
import LordOfTheRings.Objective.Import.Lifted
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Scenario.Scenarios.TheBlackSerpent.Helpers (attachedToEncounterCard)

newtype SummonsFromMordor = SummonsFromMordor ObjectiveAttrs
  deriving anyclass IsObjective
  deriving newtype (Show, Eq, ToJSON, Entity)

summonsFromMordor :: ObjectiveCard SummonsFromMordor
summonsFromMordor = objective SummonsFromMordor Cards.summonsFromMordor

-- | Guarded (location). While Summons from Mordor is attached to an encounter card, each
-- player's threat cannot be reduced.
instance HasModifiersFor SummonsFromMordor where
  getModifiersFor (SummonsFromMordor a) = whenM (attachedToEncounterCard a) do
    players <- getRemainingPlayers
    for_ players \p -> modified a (PlayerTarget p) [CannotReduceThreat]

-- | Action: Exhaust Summons from Mordor and raise your threat by 1 to add 1 resource to
-- attached hero's pool.
instance HasAbilities SummonsFromMordor where
  getAbilities (SummonsFromMordor a) =
    [mkAbility a 1 $ ActionAbility (Costs [exhaustCost a, RaiseThreatCost 1]) | AttachedTo (HeroTarget _) <- [a.placement], not a.exhausted]

instance RunMessage SummonsFromMordor where
  runMessage msg o@(SummonsFromMordor attrs) = case msg of
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      for_ [h | AttachedTo h@(HeroTarget _) <- [attrs.placement]] \h -> placeResources attrs h 1
      pure o
    _ -> SummonsFromMordor <$> runMessage msg attrs
