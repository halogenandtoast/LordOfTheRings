module LordOfTheRings.Event.Cards.ChargeIntoBattle (chargeIntoBattle) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (QuestPhase))

newtype ChargeIntoBattle = ChargeIntoBattle EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only after the staging step.
-- Quest Action: You may declare and resolve an attack against each non-unique enemy in the
-- staging area as if it were engaged with you.
--
-- The enemies are attacked one at a time, in the order the player chooses, as in the
-- combat phase; an enemy immune to player card effects is not one this reaches.
chargeIntoBattle :: EventCard ChargeIntoBattle
chargeIntoBattle =
  eventWith ChargeIntoBattle Cards.chargeIntoBattle
    (criteriaL .~ DuringPhase QuestPhase <> TargetHasModifier GameTarget AfterStagingStep)

instance RunMessage ChargeIntoBattle where
  runMessage msg e@(ChargeIntoBattle attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      enemies <- select (EnemyInStagingArea <> NotEnemy UniqueEnemy <> EnemyNotImmuneToPlayerCards)
      for_ enemies \en -> phaseModifier attrs en (AttackedAsIfEngagedWith pid)
      push $ ChooseAttackTarget pid enemies
      pure e
    _ -> ChargeIntoBattle <$> runMessage msg attrs
