module LordOfTheRings.Event.Cards.FierceDefense (fierceDefense) where

import LordOfTheRings.Classes.HasGame (getsGame)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Game.Base (gamePhase)
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype FierceDefense = FierceDefense EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

engagedWith :: PlayerId -> EnemyMatcher
engagedWith pid = EnemyEngagedWith (PlayerWithId pid) <> not_ UniqueEnemy

-- | The combat action needs the combat phase; the Valour action, any action window.
fierceDefense :: EventCard FierceDefense
fierceDefense =
  eventWith FierceDefense Cards.fierceDefense \a ->
    a & criteriaL .~ (exists (engagedWith a.owner) <> AnyCriteria [DuringPhase CombatPhase, valour a.owner])

-- | Combat Action: Deal 3 damage to a non-unique enemy engaged with you.
-- Valour Action: Discard a non-unique enemy engaged with you.
instance RunMessage FierceDefense where
  runMessage msg e@(FierceDefense attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (engagedWith pid)
      combat <- (== CombatPhase) <$> getsGame gamePhase
      let chooseEnemy f = chooseOrRunOneM pid do
            questionLabeled "fierceDefense.enemy"
            causedBy attrs
            targets enemies f
      chooseOrRunOneM pid do
        when combat $ labeled "fierceDefense.damage" $ chooseEnemy \eid -> dealDamage attrs eid 3
        valourLabeled pid $ chooseEnemy discard
      pure e
    _ -> FierceDefense <$> runMessage msg attrs
