module LordOfTheRings.Event.Cards.HandsUponTheBow (handsUponTheBow) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))

newtype HandsUponTheBow = HandsUponTheBow EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust a character you control with ranged to immediately declare it as an
-- attacker (and resolve its attack) against an enemy in the staging area. It gets +1
-- attack during this attack.
handsUponTheBow :: EventCard HandsUponTheBow
handsUponTheBow =
  eventWith HandsUponTheBow Cards.handsUponTheBow \a ->
    a & criteriaL .~ (AnyCriteria [exists (readyRangedHeroes a.owner), exists (readyRangedAllies a.owner)] <> exists EnemyInStagingArea)

readyRangedHeroes :: PlayerId -> HeroMatcher
readyRangedHeroes pid = HeroControlledBy (PlayerWithId pid) <> HeroWithKeyword Ranged <> ReadyHero

readyRangedAllies :: PlayerId -> AllyMatcher
readyRangedAllies pid = AllyControlledBy (PlayerWithId pid) <> AllyWithKeyword Ranged <> ReadyAlly

instance RunMessage HandsUponTheBow where
  runMessage msg e@(HandsUponTheBow attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      archers <- selectCharacters (readyRangedHeroes pid) (readyRangedAllies pid)
      enemies <- select EnemyInStagingArea
      chooseOneM pid do
        questionLabeled "handsUponTheBow.attacker"
        causedBy attrs
        targets archers \c -> chooseOneM pid do
          questionLabeled "chooseEnemyToAttack"
          causedBy attrs
          targets enemies \en -> attackEnemyWith pid attrs c en 1
      pure e
    _ -> HandsUponTheBow <$> runMessage msg attrs
