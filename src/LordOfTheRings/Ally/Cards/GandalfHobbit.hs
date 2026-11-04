module LordOfTheRings.Ally.Cards.GandalfHobbit (gandalfHobbit) where

import LordOfTheRings.Ally.CardDefs qualified as Cards
import LordOfTheRings.Ally.Import.Lifted
import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (placementController)

newtype GandalfHobbit = GandalfHobbit AllyAttrs
  deriving anyclass IsAlly
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | The Gandalf of Over Hill and Under Hill.
gandalfHobbit :: AllyCard GandalfHobbit
gandalfHobbit = ally GandalfHobbit Cards.gandalfHobbit

-- | Gandalf does not exhaust to commit to a quest.
instance HasModifiersFor GandalfHobbit where
  getModifiersFor (GandalfHobbit a) = modifySelf a [DoesNotExhaustToCommit]

-- | Forced: At the end of the refresh phase, discard Gandalf from play. You may raise your
-- threat by 2 to cancel this effect.
instance HasAbilities GandalfHobbit where
  getAbilities (GandalfHobbit a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase]

instance RunMessage GandalfHobbit where
  runMessage msg s@(GandalfHobbit a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      for_ (placementController a.placement) \pid -> chooseOneM pid do
        questionLabeled "gandalfHobbit.discardOrStay"
        causedBy a
        labeled "gandalfHobbit.raiseThreat" $ raiseThreat pid 2
        labeled "gandalfHobbit.discard" $ discard a
      pure s
    _ -> GandalfHobbit <$> runMessage msg a
