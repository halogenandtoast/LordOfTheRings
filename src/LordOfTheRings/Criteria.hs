module LordOfTheRings.Criteria (module LordOfTheRings.Criteria) where

import LordOfTheRings.Card.CardType
import LordOfTheRings.Id
import LordOfTheRings.Matcher.Types
import LordOfTheRings.Phase (Phase)
import LordOfTheRings.Prelude

data Criterion
  = NoCriteria
  | LocationExists LocationMatcher
  | EnemyExists EnemyMatcher
  | HeroExists HeroMatcher
  | AllyExists AllyMatcher
  | CardInHand PlayerId CardType
  | PlayerExists PlayerMatcher
  | DuringPhase Phase
  | -- | no enemy or location in play is that card
    CardNotInPlay CardId
  | Criteria [Criterion]
  | AnyCriteria [Criterion]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup Criterion where
  NoCriteria <> x = x
  x <> NoCriteria = x
  Criteria xs <> Criteria ys = Criteria (xs <> ys)
  Criteria xs <> y = Criteria (xs <> [y])
  x <> Criteria ys = Criteria (x : ys)
  x <> y = Criteria [x, y]

instance Monoid Criterion where
  mempty = NoCriteria

class Exists a where
  exists :: a -> Criterion

instance Exists LocationMatcher where
  exists = LocationExists

instance Exists EnemyMatcher where
  exists = EnemyExists

instance Exists HeroMatcher where
  exists = HeroExists

instance Exists AllyMatcher where
  exists = AllyExists
