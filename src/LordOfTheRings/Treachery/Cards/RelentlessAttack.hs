module LordOfTheRings.Treachery.Cards.RelentlessAttack (relentlessAttack) where

import LordOfTheRings.Phase (Phase (EncounterPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype RelentlessAttack = RelentlessAttack TreacheryAttrs
  deriving anyclass IsTreachery
  deriving newtype (Show, Eq, ToJSON, Entity)

relentlessAttack :: TreacheryCard RelentlessAttack
relentlessAttack = treachery RelentlessAttack Cards.relentlessAttack

-- | Counts as a Condition attachment with the text: "Each enemy gets -X engagement cost. X
-- is the number of locations controlled by players. Forced: At the beginning of the
-- encounter phase, if there are fewer than X enemies in the staging area, reveal an
-- encounter card."
instance HasModifiersFor RelentlessAttack where
  getModifiersFor (RelentlessAttack a) = case a.placement of
    AttachedTo (QuestTarget _) -> do
      x <- length <$> select (LocationControlledBy AnyPlayer)
      when (x > 0) do
        enemies <- select AnyEnemy
        for_ enemies \e -> modified a e [EngagementCostModifier (-x)]
    _ -> pure ()

instance HasAbilities RelentlessAttack where
  getAbilities (RelentlessAttack a) = case a.placement of
    AttachedTo (QuestTarget _) -> [mkAbility a 1 $ forced $ PhaseBeginsWindow After EncounterPhase]
    _ -> []

-- | Doomed 1.
-- When Revealed: Attach to the current quest.
instance RunMessage RelentlessAttack where
  runMessage msg t@(RelentlessAttack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      x <- length <$> select (LocationControlledBy AnyPlayer)
      staged <- length <$> select EnemyInStagingArea
      when (staged < x) revealEncounterCard
      pure t
    _ -> RelentlessAttack <$> runMessage msg attrs
