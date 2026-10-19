module LordOfTheRings.Event.Cards.RainOfArrows (rainOfArrows) where

import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Keyword (Keyword (Ranged))

newtype RainOfArrows = RainOfArrows EventAttrs
  deriving anyclass (IsEvent)
  deriving newtype (Show, Eq, Entity)

-- | Action: Exhaust a character you control with the ranged keyword to choose a player.
-- Deal 1 damage to each enemy engaged with that player.
rainOfArrows :: EventCard RainOfArrows
rainOfArrows =
  eventWith RainOfArrows Cards.rainOfArrows \a ->
    a
      & criteriaL
      .~ AnyCriteria
        [ HeroExists (HeroControlledBy (PlayerWithId a.owner) <> ReadyHero <> HeroWithKeyword Ranged)
        , AllyExists (AllyControlledBy (PlayerWithId a.owner) <> ReadyAlly <> AllyWithKeyword Ranged)
        ]

instance RunMessage RainOfArrows where
  runMessage msg e@(RainOfArrows attrs) = case msg of
    PlayEvent pid card _ | card.id == toId attrs -> do
      archers <-
        selectCharacters
          (HeroControlledBy (PlayerWithId pid) <> ReadyHero <> HeroWithKeyword Ranged)
          (AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> AllyWithKeyword Ranged)
      order <- getPlayerOrder
      volleys <- for order \p -> (p,) <$> select (EnemyEngagedWith $ PlayerWithId p)
      choose <- choosePlayer pid (Just $ toTarget attrs) \p ->
        [DealDamage (toSource attrs) (toTarget eid) 1 | eid <- fromMaybe [] (lookup p volleys)]
      push
        $ questionLabelWith "Exhaust a ranged character" (Just $ toTarget attrs) [] pid
          [targetLabel c [Exhaust c, choose] | c <- archers]
      pure e
    _ -> RainOfArrows <$> runMessage msg attrs
