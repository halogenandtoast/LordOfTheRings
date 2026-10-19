module LordOfTheRings.Matcher.Types (module LordOfTheRings.Matcher.Types) where

import LordOfTheRings.Id
import LordOfTheRings.Name
import LordOfTheRings.Prelude

data PlayerMatcher
  = PlayerWithId PlayerId
  | FirstPlayer
  | ActivePlayer
  | AnyPlayer
  | PlayerMatchAll [PlayerMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup PlayerMatcher where
  AnyPlayer <> x = x
  x <> AnyPlayer = x
  PlayerMatchAll xs <> PlayerMatchAll ys = PlayerMatchAll (xs <> ys)
  PlayerMatchAll xs <> y = PlayerMatchAll (xs <> [y])
  x <> PlayerMatchAll ys = PlayerMatchAll (x : ys)
  x <> y = PlayerMatchAll [x, y]

instance Monoid PlayerMatcher where
  mempty = AnyPlayer

data HeroMatcher
  = HeroWithId HeroId
  | HeroControlledBy PlayerMatcher
  | HeroWithTitle Name
  | ReadyHero
  | ExhaustedHero
  | CommittedHero
  | AttackingHero
  | AnyHero
  | NotHero HeroMatcher
  | HeroMatchAll [HeroMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup HeroMatcher where
  AnyHero <> x = x
  x <> AnyHero = x
  HeroMatchAll xs <> HeroMatchAll ys = HeroMatchAll (xs <> ys)
  HeroMatchAll xs <> y = HeroMatchAll (xs <> [y])
  x <> HeroMatchAll ys = HeroMatchAll (x : ys)
  x <> y = HeroMatchAll [x, y]

instance Monoid HeroMatcher where
  mempty = AnyHero

instance Not HeroMatcher where
  not_ = NotHero

data AllyMatcher
  = AllyWithId AllyId
  | AllyControlledBy PlayerMatcher
  | AllyWithTitle Name
  | ReadyAlly
  | ExhaustedAlly
  | CommittedAlly
  | AttackingAlly
  | AnyAlly
  | NotAlly AllyMatcher
  | AllyMatchAll [AllyMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup AllyMatcher where
  AnyAlly <> x = x
  x <> AnyAlly = x
  AllyMatchAll xs <> AllyMatchAll ys = AllyMatchAll (xs <> ys)
  AllyMatchAll xs <> y = AllyMatchAll (xs <> [y])
  x <> AllyMatchAll ys = AllyMatchAll (x : ys)
  x <> y = AllyMatchAll [x, y]

instance Monoid AllyMatcher where
  mempty = AnyAlly

instance Not AllyMatcher where
  not_ = NotAlly

data EnemyMatcher
  = EnemyWithId EnemyId
  | EnemyInStagingArea
  | EnemyEngagedWith PlayerMatcher
  | AnyEnemy
  | NotEnemy EnemyMatcher
  | EnemyMatchAll [EnemyMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup EnemyMatcher where
  AnyEnemy <> x = x
  x <> AnyEnemy = x
  EnemyMatchAll xs <> EnemyMatchAll ys = EnemyMatchAll (xs <> ys)
  EnemyMatchAll xs <> y = EnemyMatchAll (xs <> [y])
  x <> EnemyMatchAll ys = EnemyMatchAll (x : ys)
  x <> y = EnemyMatchAll [x, y]

instance Monoid EnemyMatcher where
  mempty = AnyEnemy

instance Not EnemyMatcher where
  not_ = NotEnemy

data LocationMatcher
  = LocationWithId LocationId
  | LocationInStagingArea
  | IsActiveLocation
  | AnyLocation
  | NotLocation LocationMatcher
  | LocationMatchAll [LocationMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup LocationMatcher where
  AnyLocation <> x = x
  x <> AnyLocation = x
  LocationMatchAll xs <> LocationMatchAll ys = LocationMatchAll (xs <> ys)
  LocationMatchAll xs <> y = LocationMatchAll (xs <> [y])
  x <> LocationMatchAll ys = LocationMatchAll (x : ys)
  x <> y = LocationMatchAll [x, y]

instance Monoid LocationMatcher where
  mempty = AnyLocation

instance Not LocationMatcher where
  not_ = NotLocation
