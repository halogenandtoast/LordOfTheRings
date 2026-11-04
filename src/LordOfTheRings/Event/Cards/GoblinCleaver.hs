module LordOfTheRings.Event.Cards.GoblinCleaver (goblinCleaver) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Phase (Phase (CombatPhase))
import LordOfTheRings.Trait (Trait (Orc, Weapon))

newtype GoblinCleaver = GoblinCleaver EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Combat Action: Exhaust a Weapon card attached to a hero you control to choose an enemy
-- engaged with you. Deal 2 damage to that enemy. (Deal 3 damage instead if the enemy is an
-- Orc.)
goblinCleaver :: EventCard GoblinCleaver
goblinCleaver =
  eventWith GoblinCleaver Cards.goblinCleaver \a ->
    a
      & criteriaL
      .~ ( DuringPhase CombatPhase
             <> exists (HeroControlledBy (PlayerWithId a.owner) <> HeroWithReadyAttached Weapon)
             <> exists (EnemyEngagedWith (PlayerWithId a.owner) <> EnemyNotImmuneToPlayerCards)
         )

instance RunMessage GoblinCleaver where
  runMessage msg e@(GoblinCleaver attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- map toTarget <$> select (HeroControlledBy $ PlayerWithId pid)
      weapons <- concatMapM (readyAttachedWithTrait Weapon) heroes
      enemies <- select (EnemyEngagedWith (PlayerWithId pid) <> EnemyNotImmuneToPlayerCards)
      chooseOrRunOneM pid do
        questionLabeled "goblinCleaver.chooseWeapon"
        causedBy attrs
        targets weapons \w -> do
          exhaust w
          chooseOrRunOneM pid do
            questionLabeled "chooseEnemy"
            causedBy attrs
            targets enemies \en -> do
              orc <- selectAny (EnemyWithId en <> EnemyWithTrait Orc)
              dealDamage attrs en (if orc then 3 else 2)
      pure e
    _ -> GoblinCleaver <$> runMessage msg attrs

