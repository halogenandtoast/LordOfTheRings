module LordOfTheRings.Treachery.Cards.MinceThemFine (minceThemFine) where

import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith, StagingArea))
import LordOfTheRings.Scenario.Scenarios.WeMustAwayEreBreakOfDay.Helpers (trollEnemy)

newtype MinceThemFine = MinceThemFine TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

minceThemFine :: TreacheryCard MinceThemFine
minceThemFine = treachery MinceThemFine Cards.minceThemFine

instance RunMessage MinceThemFine where
  runMessage msg t@(MinceThemFine attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      trolls <- select (trollEnemy <> EnemyEngagedWith AnyPlayer)
      for_ trolls \eid -> do
        enemy <- getEnemy eid
        case enemy.placement of
          EngagedWith pid -> immediateAttack pid eid
          _ -> pure ()
      doStep 1 msg
      pure t
    DoStep 1 (WhenRevealed (isTarget attrs -> True)) -> do
      stage <- getMainStage
      when (stage == 1) advanceQuest
      pure t
    ResolveShadowEffect cid eid _ _ | cid == attrs.cardId -> do
      troll <- selectAny (EnemyWithId eid <> trollEnemy)
      when troll $ afterAttack eid [PlaceEnemy eid StagingArea]
      pure t
    _ -> MinceThemFine <$> runMessage msg attrs
