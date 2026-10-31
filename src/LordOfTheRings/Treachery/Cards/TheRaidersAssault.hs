module LordOfTheRings.Treachery.Cards.TheRaidersAssault (theRaidersAssault) where

import LordOfTheRings.Card (IsCard (toCard))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (mayGuard, unguardedObjectives)
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheRaidersAssault = TheRaidersAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

theRaidersAssault :: TreacheryCard TheRaidersAssault
theRaidersAssault = treachery TheRaidersAssault Cards.theRaidersAssault

-- | When Revealed: Choose a Ship enemy that is not currently guarding an objective. Attach
-- that enemy to an unguarded objective in the staging area, guarding it (return that
-- enemy to the staging area if it is engaged). If no Ship enemy is returned to the
-- staging area by this effect, The Raiders' Assault gains surge.
instance RunMessage TheRaidersAssault where
  runMessage msg t@(TheRaidersAssault attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      ships <- traverse (\e -> (e,) <$> getEnemy e) =<< mayGuard =<< select shipEnemy
      objectives <- unguardedObjectives
      lead <- getLead
      if null ships || null objectives
        then revealEncounterCard
        else chooseOneM lead do
          questionLabeled "theRaidersAssault.enemy"
          causedBy attrs
          for_ ships \(eid, enemy) -> targeting eid do
            let engaged = enemy.placement /= StagingArea
            chooseOrRunOneM lead do
              questionLabeled "theRaidersAssault.objective"
              causedBy attrs
              targets objectives \o -> do
                guardObjectiveWith o (toCard enemy).id
                when engaged $ placeEnemy eid StagingArea
            unless engaged revealEncounterCard
      pure t
    _ -> TheRaidersAssault <$> runMessage msg attrs
