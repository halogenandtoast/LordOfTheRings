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
--
-- ponytail: two characters that are each both Ranger and Warrior do not count; the
-- criteria have no way to tell two such characters from one.
coneyInATrap :: EventCard ConeyInATrap
coneyInATrap =
  eventWith ConeyInATrap Cards.coneyInATrap \a ->
    let you = PlayerWithId a.owner
        character traits without =
          AnyCriteria
            [ exists $ HeroControlledBy you <> foldMap HeroWithTrait traits <> foldMap (not_ . HeroWithTrait) without
            , exists $ AllyControlledBy you <> UniqueAlly <> foldMap AllyWithTrait traits <> foldMap (not_ . AllyWithTrait) without
            ]
     in a
          & windowL ?~ PlayerEngagedEnemyWindow After a.owner
          & criteriaL
          .~ AnyCriteria
            [ character [Ranger] [Warrior] <> character [Warrior] []
            , character [Warrior] [Ranger] <> character [Ranger] []
            ]

instance RunMessage ConeyInATrap where
  runMessage msg e@(ConeyInATrap attrs) = case msg of
    PlayEvent _ card windows | card.id == toId attrs -> do
      for_ [eid | Window _ (EnemyEngaged eid _) <- windows] \eid -> roundModifier attrs eid CannotAttack
      pure e
    _ -> ConeyInATrap <$> runMessage msg attrs
