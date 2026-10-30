module LordOfTheRings.Event.Cards.BattleFury (battleFury) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))
import LordOfTheRings.Sphere (Sphere (Spirit))

newtype BattleFury = BattleFury EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only before the staging step.
battleFury :: EventCard BattleFury
battleFury =
  eventWith BattleFury Cards.battleFury \a ->
    a
      & criteriaL
      .~ (BeforeStaging <> exists (HeroControlledBy (PlayerWithId a.owner) <> ReadyHero) <> exists (EnemyEngagedWith AnyPlayer))

-- | Quest Action: Exhaust a hero you control to immediately declare it as an attacker
-- (and resolve its attack) against any eligible enemy target. Then, the players as a group
-- may spend 1 spirit resource to commit that hero to the quest.
instance RunMessage BattleFury where
  runMessage msg e@(BattleFury attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      heroes <- select (HeroControlledBy (PlayerWithId pid) <> ReadyHero)
      chooseOrRunOneM pid do
        questionLabeled "battleFury.hero"
        causedBy attrs
        targets heroes \h -> do
          exhaust h
          -- a ranged hero may attack an enemy engaged with another player
          ranged <- selectAny (HeroWithId h <> HeroWithKeyword Ranged)
          enemies <- select (EnemyEngagedWith $ if ranged then AnyPlayer else PlayerWithId pid)
          chooseOrRunOneM pid do
            questionLabeled "battleFury.enemy"
            causedBy attrs
            targets enemies \eid -> attackEnemyWith pid attrs (toTarget h) eid 0
          groupMaySpend pid attrs Spirit 1 $ commitToQuest h
      pure e
    _ -> BattleFury <$> runMessage msg attrs
