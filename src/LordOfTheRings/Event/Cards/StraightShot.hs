module LordOfTheRings.Event.Cards.StraightShot (straightShot) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Weapon))

newtype StraightShot = StraightShot EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust a Weapon attachment to choose a non-unique enemy with 0 [defense].
-- Discard the chosen enemy.
--
-- The Weapon is one the player controls, as a cost is paid.
straightShot :: EventCard StraightShot
straightShot =
  eventWith StraightShot Cards.straightShot \a ->
    a
      & criteriaL
      .~ (ReadyAttachmentWithTrait a.owner Weapon <> exists targetable)

targetable :: EnemyMatcher
targetable = NotEnemy UniqueEnemy <> EnemyWithDefense 0 <> EnemyNotImmuneToPlayerCards

instance RunMessage StraightShot where
  runMessage msg e@(StraightShot attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      weapons <- readyControlledAttachmentsWithTrait pid Weapon
      enemies <- select targetable
      chooseOrRunOneM pid do
        questionLabeled "straightShot.chooseWeapon"
        causedBy attrs
        targets weapons \w -> do
          exhaust w
          chooseOrRunOneM pid do
            questionLabeled "chooseEnemy"
            causedBy attrs
            targets enemies (discard attrs)
      pure e
    _ -> StraightShot <$> runMessage msg attrs
