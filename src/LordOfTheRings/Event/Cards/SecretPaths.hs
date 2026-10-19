module LordOfTheRings.Event.Cards.SecretPaths (secretPaths) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype SecretPaths = SecretPaths EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Quest Action: Choose a location in the staging area. Until the end of the phase, that
-- location does not contribute its threat.
secretPaths :: EventCard SecretPaths
secretPaths = eventWith SecretPaths Cards.secretPaths (criteriaL .~ (DuringPhase QuestPhase <> exists LocationInStagingArea))

instance RunMessage SecretPaths where
  runMessage msg e@(SecretPaths attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      locations <- select LocationInStagingArea
      push
        $ questionLabelWith (ikey "label.chooseLocationInStaging") (Just $ toTarget attrs) [] pid
          [targetLabel lid [CreateEffect $ Effect (toSource attrs) (toTarget lid) [DoesNotContributeThreat] UntilEndOfPhase] | lid <- locations]
      pure e
    _ -> SecretPaths <$> runMessage msg attrs
