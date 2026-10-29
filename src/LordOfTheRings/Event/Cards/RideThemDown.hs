module LordOfTheRings.Event.Cards.RideThemDown (rideThemDown) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype RideThemDown = RideThemDown EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

target :: EnemyMatcher
target = EnemyInStagingArea <> not_ UniqueEnemy <> EnemyNotImmuneToPlayerCards

-- | Quest Action: Choose a non-unique enemy in the staging area. Until the end of the
-- phase, progress that would be placed on the quest from questing successfully is instead
-- assigned as damage to the chosen enemy. (Progress must still be placed on any active
-- location first.)
rideThemDown :: EventCard RideThemDown
rideThemDown = eventWith RideThemDown Cards.rideThemDown $ criteriaL .~ (DuringPhase QuestPhase <> exists target)

instance RunMessage RideThemDown where
  runMessage msg e@(RideThemDown attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select target
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemyInStaging"
        causedBy attrs
        targets enemies \eid -> phaseModifier attrs GameTarget (ProgressAsDamageTo $ toTarget eid)
      pure e
    _ -> RideThemDown <$> runMessage msg attrs
