module LordOfTheRings.Event.Cards.SpeakYourPromise (speakYourPromise) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Helpers.Character (getWillpower)
import LordOfTheRings.Helpers.Fellowship (ringBearer)
import LordOfTheRings.Helpers.Staging (getEnemyThreat)

newtype SpeakYourPromise = SpeakYourPromise EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Ready the Ring-bearer to choose an enemy engaged with you. If the
-- Ring-bearer's willpower is greater than that enemy's threat, lower that enemy's attack
-- and defense by the difference until the end of the phase. Readying him is the cost: he
-- must be exhausted.
speakYourPromise :: EventCard SpeakYourPromise
speakYourPromise =
  eventWith SpeakYourPromise Cards.speakYourPromise \a ->
    a & criteriaL .~ (exists (ringBearer <> ExhaustedHero) <> exists (EnemyEngagedWith $ PlayerWithId a.owner))

instance RunMessage SpeakYourPromise where
  runMessage msg e@(SpeakYourPromise attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      bearers <- select (ringBearer <> ExhaustedHero)
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOrRunOneM pid do
        questionLabeled "speakYourPromise.chooseRingBearer"
        causedBy attrs
        targets bearers \hid -> do
          ready hid
          chooseOrRunOneM pid do
            questionLabeled "speakYourPromise.chooseEnemy"
            causedBy attrs
            targets enemies \eid -> do
              willpower <- getWillpower (toTarget hid)
              threat <- getEnemyThreat eid
              let diff = willpower - threat
              when (diff > 0)
                $ phaseModifiers attrs eid [AttackModifier (negate diff), DefenseModifier (negate diff)]
      pure e
    _ -> SpeakYourPromise <$> runMessage msg attrs
