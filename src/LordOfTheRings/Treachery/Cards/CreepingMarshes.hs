module LordOfTheRings.Treachery.Cards.CreepingMarshes (creepingMarshes) where

import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype CreepingMarshes = CreepingMarshes TreacheryAttrs
  deriving anyclass (IsTreachery, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

creepingMarshes :: TreacheryCard CreepingMarshes
creepingMarshes = treachery CreepingMarshes Cards.creepingMarshes

-- | Counts as a Condition attachment with the text: "Forced: At the end of the refresh
-- phase, place 1 mire token on each location in the staging area. Then, discard Creeping
-- Marshes."
instance HasAbilities CreepingMarshes where
  getAbilities (CreepingMarshes a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | AttachedTo _ <- [a.placement]]

-- | Surge. Doomed 1.
-- When Revealed: Attach to the current quest.
instance RunMessage CreepingMarshes where
  runMessage msg t@(CreepingMarshes attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      traverse_ (attachTreachery attrs.treacheryId) =<< getCurrentQuest
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      staged <- select LocationInStagingArea
      for_ staged \l -> placeResources attrs l 1
      discard attrs
      pure t
    _ -> CreepingMarshes <$> runMessage msg attrs
