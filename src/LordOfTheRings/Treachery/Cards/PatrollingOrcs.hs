module LordOfTheRings.Treachery.Cards.PatrollingOrcs (patrollingOrcs) where

import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype PatrollingOrcs = PatrollingOrcs TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

patrollingOrcs :: TreacheryCard PatrollingOrcs
patrollingOrcs = treachery PatrollingOrcs Cards.patrollingOrcs

-- | Counts as a Condition attachment with the text: "Forced: After a player engages an
-- enemy, he must either exhaust a character he controls, or remove 1 time counter from the
-- current quest."
instance HasAbilities PatrollingOrcs where
  getAbilities (PatrollingOrcs a) = [mkAbility a 1 $ forced $ AnyEnemyEngagedWindow After | AttachedTo _ <- [a.placement]]

-- | Surge. Doomed 1.
-- When Revealed: Attach to the current quest.
instance RunMessage PatrollingOrcs where
  runMessage msg t@(PatrollingOrcs attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 windows -> do
      for_ [pid | Window _ (EnemyEngaged _ pid) <- windows] \pid -> do
        chooseOrRunOneM pid do
          questionLabeled "patrollingOrcs.choose"
          causedBy attrs
          labeled "patrollingOrcs.exhaust" $ chooseAndExhaust attrs pid 1
          labeled "patrollingOrcs.timeCounter" $ removeTimeCountersFromCurrentQuest attrs 1
      pure t
    _ -> PatrollingOrcs <$> runMessage msg attrs
