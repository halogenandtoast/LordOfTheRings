module LordOfTheRings.Event.Cards.NoiselessMovement (noiselessMovement) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype NoiselessMovement = NoiselessMovement EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Choose an enemy in the staging area. That enemy does not make engagement checks
-- this round. Then, if your threat is 20 or less and this is the first time you played a
-- copy of Noiseless Movement this round, return this card to your hand instead of
-- discarding it.
noiselessMovement :: EventCard NoiselessMovement
noiselessMovement =
  eventWith NoiselessMovement Cards.noiselessMovement
    $ criteriaL
    .~ exists (EnemyInStagingArea <> EnemyNotImmuneToPlayerCards)

instance RunMessage NoiselessMovement where
  runMessage msg e@(NoiselessMovement attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (EnemyInStagingArea <> EnemyNotImmuneToPlayerCards)
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemyInStaging"
        causedBy attrs
        targets enemies \eid -> roundModifier attrs eid NoEngagementChecks
      returnIfFirstCopyThisRound pid card
      pure e
    _ -> NoiselessMovement <$> runMessage msg attrs
