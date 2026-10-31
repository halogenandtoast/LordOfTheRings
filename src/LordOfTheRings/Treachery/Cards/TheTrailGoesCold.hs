module LordOfTheRings.Treachery.Cards.TheTrailGoesCold (theTrailGoesCold) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Helpers.Staging (getLocationThreat)
import LordOfTheRings.Phase (Phase (QuestPhase))
import LordOfTheRings.Placement (Placement (ActiveLocation, AttachedTo))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TheTrailGoesCold = TheTrailGoesCold TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theTrailGoesCold :: TreacheryCard TheTrailGoesCold
theTrailGoesCold = treachery TheTrailGoesCold Cards.theTrailGoesCold

-- | Counts as a Condition attachment with the text: "Limit 1 per location. Attached
-- location contributes its threat during the quest phase as if it were in the staging
-- area."
instance HasModifiersFor TheTrailGoesCold where
  getModifiersFor (TheTrailGoesCold a) = case a.placement of
    AttachedTo (LocationTarget lid) -> do
      phase <- getsGame gamePhase
      host <- getLocation lid
      when (phase == QuestPhase && host.placement == ActiveLocation) do
        threat <- getLocationThreat lid
        modified a GameTarget [StagingThreatBonus threat]
    _ -> pure ()

-- | When Revealed: Attach to the active location and remove all progress from that
-- location. If The Trail Goes Cold does not attach to a location as a result of its
-- effect, it gains surge.
instance RunMessage TheTrailGoesCold where
  runMessage msg t@(TheTrailGoesCold attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      active <- select IsActiveLocation
      free <- filterM (fmap null . treacheriesOn attrs.treacheryCardCode . toTarget) active
      case free of
        lid : _ -> do
          attachTreachery attrs.treacheryId lid
          l <- getLocation lid
          when (l.progress > 0) $ removeTokens attrs lid Progress l.progress
        [] -> revealEncounterCard
      pure t
    _ -> TheTrailGoesCold <$> runMessage msg attrs
