module LordOfTheRings.Event.Cards.ExpertTrackers (expertTrackers) where

import LordOfTheRings.Card (cdThreat, toCardDef)
import LordOfTheRings.Event.CardDefs qualified as Cards
import LordOfTheRings.Event.Import.Lifted
import LordOfTheRings.Trait (Trait (Ranger, Scout))

newtype ExpertTrackers = ExpertTrackers EventAttrs
  deriving anyclass (IsEvent, HasModifiersFor)
  deriving newtype (Show, Eq, Entity)

trackers :: PlayerId -> (HeroMatcher, AllyMatcher)
trackers pid =
  ( HeroControlledBy (PlayerWithId pid) <> ReadyHero <> HeroMatchAny [HeroWithTrait Scout, HeroWithTrait Ranger]
  , AllyControlledBy (PlayerWithId pid) <> ReadyAlly <> AllyMatchAny [AllyWithTrait Scout, AllyWithTrait Ranger]
  )

-- | Response: After you engage an enemy, exhaust a Scout or Ranger character to place X
-- progress tokens on a location. X is the engaged enemy's printed threat.
expertTrackers :: EventCard ExpertTrackers
expertTrackers =
  eventWith ExpertTrackers Cards.expertTrackers \a ->
    a
      & windowL ?~ PlayerEngagedEnemyWindow After a.owner
      & criteriaL .~ uncurry (CharactersAtLeast 1) (trackers a.owner) <> exists AnyLocation

instance RunMessage ExpertTrackers where
  runMessage msg e@(ExpertTrackers attrs) = case msg of
    PlayEvent pid card windows | card.id == toId attrs -> do
      characters <- uncurry selectCharacters (trackers pid)
      chooseOrRunOneM pid do
        questionLabeled "expertTrackers.exhaust"
        causedBy attrs
        targets characters exhaust
      for_ (take 1 [eid | Window _ (EnemyEngaged eid _) <- windows]) \eid -> do
        x <- fromMaybe 0 . cdThreat . toCardDef <$> getEnemy eid
        locations <- select AnyLocation
        chooseOrRunOneM pid do
          questionLabeled "expertTrackers.location"
          causedBy attrs
          targets locations \l -> placeProgress attrs l x
      pure e
    _ -> ExpertTrackers <$> runMessage msg attrs
