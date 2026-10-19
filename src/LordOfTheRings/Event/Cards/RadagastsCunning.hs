module LordOfTheRings.Event.Cards.RadagastsCunning (radagastsCunning) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype RadagastsCunning = RadagastsCunning EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Quest Action: Choose an enemy in the staging area. Until the end of the phase, that
-- enemy does not contribute its threat.
radagastsCunning :: EventCard RadagastsCunning
radagastsCunning =
  eventWith RadagastsCunning Cards.radagastsCunning (criteriaL .~ (DuringPhase QuestPhase <> exists EnemyInStagingArea))

instance RunMessage RadagastsCunning where
  runMessage msg e@(RadagastsCunning attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select EnemyInStagingArea
      push
        $ questionLabelWith (ikey "label.chooseEnemyInStaging") (Just $ toTarget attrs) [] pid
          [targetLabel eid [CreateEffect $ Effect (toSource attrs) (toTarget eid) [DoesNotContributeThreat] UntilEndOfPhase] | eid <- enemies]
      pure e
    _ -> RadagastsCunning <$> runMessage msg attrs
