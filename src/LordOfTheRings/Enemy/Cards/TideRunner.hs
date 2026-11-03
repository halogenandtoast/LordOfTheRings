module LordOfTheRings.Enemy.Cards.TideRunner (tideRunner) where

import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (EngagedWith))
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (addToStormcallerArea)

newtype TideRunner = TideRunner EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

tideRunner :: EnemyCard TideRunner
tideRunner = enemy TideRunner Cards.tideRunner

-- | Boarding 1. Cannot have attachments.
-- While you are off-course, Tide-runner is considered to be in the Stormcaller's area and
-- the staging area, regardless of its current location.
instance HasModifiersFor TideRunner where
  getModifiersFor (TideRunner a) = do
    modifySelf a [CannotHaveAttachments]
    case a.placement of
      EngagedWith _ -> whenM isOffCourse $ modified a GameTarget [EngagedEnemiesAddThreat (EnemyWithId a.id)]
      _ -> pure ()

-- | When Revealed: Either choose a player to engage Tide-runner, or add Tide-runner to the
-- Stormcaller's area and it gains surge.
instance RunMessage TideRunner where
  runMessage msg e@(TideRunner attrs) = case msg of
    WhenRevealed (isTarget attrs -> True) -> do
      lead <- getLead
      players <- getRemainingPlayers
      chooseOneM lead do
        questionLabeled "tideRunner.choose"
        causedBy attrs
        targets players \p -> engageEnemy p attrs.id
        labeled "tideRunner.stormcaller" do
          addToStormcallerArea (toTarget attrs)
          revealEncounterCard
      pure e
    _ -> TideRunner <$> runMessage msg attrs
