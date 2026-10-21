module LordOfTheRings.Event.Cards.QuickStrike (quickStrike) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted

newtype QuickStrike = QuickStrike EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust a character you control to immediately declare it as an attacker (and
-- resolve its attack) against any eligible enemy target.
quickStrike :: EventCard QuickStrike
quickStrike =
  eventWith QuickStrike Cards.quickStrike \a ->
    a
      & criteriaL
      .~ ( exists (EnemyEngagedWith $ PlayerWithId a.owner)
            <> AnyCriteria [HeroExists (HeroControlledBy (PlayerWithId a.owner) <> ReadyHero), AllyExists (AllyControlledBy (PlayerWithId a.owner) <> ReadyAlly)]
         )

instance RunMessage QuickStrike where
  runMessage msg e@(QuickStrike attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      attackers <- selectCharacters (HeroControlledBy (PlayerWithId pid) <> ReadyHero) (AllyControlledBy (PlayerWithId pid) <> ReadyAlly)
      enemies <- select (EnemyEngagedWith $ PlayerWithId pid)
      chooseOneM pid do
        questionLabeled "quickStrike.exhaust"
        causedBy attrs
        targets attackers \c -> chooseOneM pid do
          questionLabeled "quickStrike.enemy"
          causedBy c
          targets enemies \eid -> pushAll [AttackWith c, ResolveAttack pid eid]
      pure e
    _ -> QuickStrike <$> runMessage msg attrs
