module LordOfTheRings.Treachery.Cards.OffTrack (offTrack) where

import LordOfTheRings.Phase (Phase (RefreshPhase))
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype OffTrack = OffTrack TreacheryAttrs
  deriving anyclass (IsTreachery)
  deriving newtype (Show, Eq, ToJSON, Entity)

offTrack :: TreacheryCard OffTrack
offTrack = treachery OffTrack Cards.offTrack

-- | Counts as a Condition attachment with the text: "Limit 1 per location. Attached location
-- gets +2 quest points and gains: 'Forced: Remove an additional time counter from the
-- current quest at the end of each refresh phase, if able.'"
instance HasModifiersFor OffTrack where
  getModifiersFor (OffTrack a) = withHost a \host -> modified a host [QuestPointModifier 2]

instance HasAbilities OffTrack where
  getAbilities (OffTrack a) = [mkAbility a 1 $ forced $ PhaseEndsWindow When RefreshPhase | AttachedTo _ <- [a.placement]]

-- | Surge.
-- When Revealed: Attach to a location in play.
instance RunMessage OffTrack where
  runMessage msg t@(OffTrack attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      locations <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . toTarget) =<< select AnyLocation
      lead <- getLead
      chooseOrRunOneM lead do
        questionLabeled "offTrack.attach"
        causedBy attrs
        targets locations $ attachTreachery attrs.treacheryId
      pure t
    UseCardAbility _ (isSource attrs -> True) 1 _ -> do
      removeTimeCountersFromCurrentQuest attrs 1
      pure t
    _ -> OffTrack <$> runMessage msg attrs
