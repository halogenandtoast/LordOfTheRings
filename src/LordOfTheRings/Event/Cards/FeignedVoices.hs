module LordOfTheRings.Event.Cards.FeignedVoices (feignedVoices) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Silvan))

newtype FeignedVoices = FeignedVoices EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Combat Action: Return a Silvan ally you control to your hand to choose an enemy
-- engaged with a player. That enemy cannot attack that player this phase.
feignedVoices :: EventCard FeignedVoices
feignedVoices =
  eventWith FeignedVoices Cards.feignedVoices \a ->
    a & criteriaL .~ (DuringPhase CombatPhase <> controlsAlly a.owner (AllyWithTrait Silvan) <> exists (EnemyEngagedWith AnyPlayer))

instance RunMessage FeignedVoices where
  runMessage msg e@(FeignedVoices attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      returnAllyToHand pid attrs (AllyWithTrait Silvan)
      enemies <- select (EnemyEngagedWith AnyPlayer)
      chooseOneM pid do
        questionLabeled "chooseEnemy"
        causedBy attrs
        targets enemies \en -> cannotAttackEngagedPlayer attrs en
      pure e
    _ -> FeignedVoices <$> runMessage msg attrs
