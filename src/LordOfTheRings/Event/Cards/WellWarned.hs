module LordOfTheRings.Event.Cards.WellWarned (wellWarned) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Noble, Scout))

newtype WellWarned = WellWarned EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

-- | Play only if you control a unique character with the Noble trait and another unique
-- character with the Scout trait. Response: After a player engages an enemy, reduce his
-- threat by X, where X is that enemy's printed threat.
wellWarned :: EventCard WellWarned
wellWarned =
  eventWith WellWarned Cards.wellWarned \a ->
    let you = PlayerWithId a.owner
        unique trait = (HeroControlledBy you <> HeroWithTrait trait, AllyControlledBy you <> UniqueAlly <> AllyWithTrait trait)
     in a
          & windowL ?~ AnyEnemyEngagedWindow After
          & criteriaL .~ DistinctCharacters [unique Noble, unique Scout]

instance RunMessage WellWarned where
  runMessage msg e@(WellWarned attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      let engaged = [(eid, p) | Window _ (EnemyEngaged eid p) <- windows]
      chooseOrRunOneM pid do
        questionLabeled "chooseEnemy"
        causedBy attrs
        for_ engaged \(eid, p) -> targeting eid do
          x <- fromMaybe 0 . cdThreat . toCardDef <$> getEnemy eid
          lowerThreatBy attrs p x
      pure e
    _ -> WellWarned <$> runMessage msg attrs
