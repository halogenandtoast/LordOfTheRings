module LordOfTheRings.Event.Cards.ConeyInATrap (coneyInATrap) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger, Warrior))

newtype ConeyInATrap = ConeyInATrap EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a unique character with the Ranger trait and another unique
-- character with the Warrior trait. Response: After you engage an enemy, that enemy
-- cannot attack you until the end of the round.
coneyInATrap :: EventCard ConeyInATrap
coneyInATrap =
  eventWith ConeyInATrap Cards.coneyInATrap \a ->
    let you = PlayerWithId a.owner
        unique trait = (HeroControlledBy you <> HeroWithTrait trait, AllyControlledBy you <> UniqueAlly <> AllyWithTrait trait)
     in a
          & windowL ?~ PlayerEngagedEnemyWindow After a.owner
          & criteriaL .~ DistinctCharacters [unique Ranger, unique Warrior]

instance RunMessage ConeyInATrap where
  runMessage msg e@(ConeyInATrap attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ [eid | Window _ (EnemyEngaged eid _) <- windows] \eid -> roundModifier attrs eid CannotAttack
      pure e
    _ -> ConeyInATrap <$> runMessage msg attrs
