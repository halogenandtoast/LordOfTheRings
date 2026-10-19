module LordOfTheRings.Event.Cards.Feint (feint) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))

newtype Feint = Feint EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Combat Action: Choose an enemy engaged with a player. That enemy cannot attack that
-- player this phase.
feint :: EventCard Feint
feint = eventWith Feint Cards.feint (criteriaL .~ (DuringPhase CombatPhase <> exists (EnemyEngagedWith AnyPlayer)))

instance RunMessage Feint where
  runMessage msg e@(Feint attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      engaged <- select (EnemyEngagedWith AnyPlayer)
      push
        $ questionLabelWith (ikey "label.feint.choose") (Just $ toTarget attrs) [] pid
          [targetLabel eid [CreateEffect $ Effect (toSource attrs) (toTarget eid) [CannotAttack] UntilEndOfPhase] | eid <- engaged]
      pure e
    _ -> Feint <$> runMessage msg attrs
