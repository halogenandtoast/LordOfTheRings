module LordOfTheRings.Matcher.Types (module LordOfTheRings.Matcher.Types) where

import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.Id
import LordOfTheRings.Name
import LordOfTheRings.Keyword (Keyword)
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Trait

data PlayerMatcher
  = PlayerWithId PlayerId
  | FirstPlayer
  | ActivePlayer
  | AnyPlayer
  | NotPlayer PlayerMatcher
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
  | HeroWithTrait Trait
  | HeroWithSphere Sphere
  | HeroWithKeyword Keyword
  | ReadyHero
  | ExhaustedHero
  | CommittedHero
  | AttackingHero
  | PrisonerHero
  | -- | a hero with an attachment, objective or condition of the trait attached
    HeroWithAttached Trait
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
  | AllyWithTrait Trait
  | AllyWithSphere Sphere
  | AllyWithKeyword Keyword
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

data AttachMatcher
  = AttachToHero HeroMatcher
  | AttachToCharacter
  | AttachToEnemy EnemyMatcher
  | AttachToLocation LocationMatcher
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EnemyMatcher
  = EnemyWithId EnemyId
  | EnemyInStagingArea
  | EnemyWithTrait Trait
  | EnemyWithCardCode CardCode
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
  | LocationWithTrait Trait
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

-- | A card by what is printed on it, wherever it is.
data CardMatcher
  = CardWithCode CardCode
  | CardWithTrait Trait
  | CardWithType CardType
  | AnyCard
  | NotCard CardMatcher
  | CardMatchAll [CardMatcher]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Semigroup CardMatcher where
  AnyCard <> x = x
  x <> AnyCard = x
  CardMatchAll xs <> CardMatchAll ys = CardMatchAll (xs <> ys)
  CardMatchAll xs <> y = CardMatchAll (xs <> [y])
  x <> CardMatchAll ys = CardMatchAll (x : ys)
  x <> y = CardMatchAll [x, y]

instance Monoid CardMatcher where
  mempty = AnyCard

cardIs :: CardDef -> CardMatcher
cardIs = CardWithCode . cdCardCode

cardMatches :: (HasCardDef a) => CardMatcher -> a -> Bool
cardMatches matcher (toCardDef -> def) = case matcher of
  CardWithCode code -> cdCardCode def == code
  CardWithTrait trait -> trait `member` cdCardTraits def
  CardWithType cType -> cdCardType def == cType
  AnyCard -> True
  NotCard m -> not $ cardMatches m def
  CardMatchAll ms -> all (`cardMatches` def) ms
