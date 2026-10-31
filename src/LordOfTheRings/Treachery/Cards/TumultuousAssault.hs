module LordOfTheRings.Treachery.Cards.TumultuousAssault (tumultuousAssault) where

import LordOfTheRings.Keyword (Keyword (Ambush))
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Treachery.CardDefs qualified as Cards
import LordOfTheRings.Treachery.Import.Lifted

newtype TumultuousAssault = TumultuousAssault TreacheryAttrs
  deriving anyclass (IsTreachery, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

tumultuousAssault :: TreacheryCard TumultuousAssault
tumultuousAssault = treachery TumultuousAssault Cards.tumultuousAssault

-- | When Revealed: Return each engaged enemy with the ambush keyword to the staging area.
-- Each player makes engagement checks against each enemy in the staging area, as if it
-- were the encounter phase. If there are no enemies with the ambush keyword in play,
-- Tumultuous Assault gains surge.
instance RunMessage TumultuousAssault where
  runMessage msg t@(TumultuousAssault attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      ambushers <- select (EnemyWithKeyword Ambush)
      engaged <- select (EnemyWithKeyword Ambush <> EnemyEngagedWith AnyPlayer)
      for_ engaged \e -> placeEnemy e StagingArea
      makeEngagementChecks
      when (null ambushers) revealEncounterCard
      pure t
    _ -> TumultuousAssault <$> runMessage msg attrs
