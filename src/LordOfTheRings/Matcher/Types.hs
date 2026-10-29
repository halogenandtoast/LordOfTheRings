module LordOfTheRings.Matcher.Types (module LordOfTheRings.Matcher.Types) where

import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Doomed))
import LordOfTheRings.Name
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Trait

data PlayerMatcher
  = PlayerWithId PlayerId
  | FirstPlayer
  | -- | the player an action is offered to, while its criteria are checked
    ActivePlayer
  | AnyPlayer
  | NotPlayer PlayerMatcher
  | -- | a player whose threat is at least this, as Valour reads
    PlayerWithThreatAtLeast Int
  | PlayerWithCardsInHand Int
  | PlayerMatchAll [PlayerMatcher]
  | -- | a player with a hidden card in their play area, as The Blood of Gondor deals them
    PlayerWithHiddenCards
  | -- | a player with at least this many cards in their deck, as a cost to discard them
    PlayerWithCardsInDeck Int
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

data CharacterMatcher = CharacterWithTitle Name | CharacterWithTrait Trait
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

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
  | -- | a hero defending the attack being resolved
    DefendingHero
  | PrisonerHero
  | -- | a hero with an attachment, objective or condition of the trait attached
    HeroWithAttached Trait
  | -- | a hero with a ready attachment of the trait attached
    HeroWithReadyAttached Trait
  | -- | a hero with a resource in its pool
    HeroWithResources
  | -- | a hero with at least this many resources in its pool
    HeroWithResourcesAtLeast Int
  | -- | a hero with a copy of the card attached
    HeroWithAttachment CardCode
  | AnyHero
  | NotHero HeroMatcher
  | HeroMatchAll [HeroMatcher]
  | -- | a hero matching any of them
    HeroMatchAny [HeroMatcher]
  | -- | a hero whose printed resource icon is the sphere's, not one it gains
    HeroWithPrintedSphere Sphere
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
  | AllyWithCardCode CardCode
  | AllyWithTrait Trait
  | AllyWithSphere Sphere
  | AllyWithKeyword Keyword
  | ReadyAlly
  | ExhaustedAlly
  | CommittedAlly
  | AttackingAlly
  | -- | an ally defending the attack being resolved
    DefendingAlly
  | UniqueAlly
  | -- | an ally that entered play this round
    AllyEnteredPlayThisRound
  | -- | an ally with an attachment of the trait attached
    AllyWithAttached Trait
  | AnyAlly
  | NotAlly AllyMatcher
  | AllyMatchAll [AllyMatcher]
  | -- | an ally matching any of them
    AllyMatchAny [AllyMatcher]
  | -- | an objective ally, an encounter card
    ObjectiveAlly
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
  | -- | a hero or an ally matching its matcher
    AttachToCharacterMatching HeroMatcher AllyMatcher
  | AttachToAlly AllyMatcher
  | AttachToEnemy EnemyMatcher
  | AttachToLocation LocationMatcher
  | -- | played into the staging area unattached, as a Trap
    PlayIntoStagingArea
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EnemyMatcher
  = EnemyWithId EnemyId
  | EnemyInStagingArea
  | EnemyWithTrait Trait
  | EnemyWithCardCode CardCode
  | EnemyEngagedWith PlayerMatcher
  | -- | an enemy dealt a shadow card still face down
    EnemyWithFacedownShadow
  | -- | an enemy whose shadow card has turned face up, as its attack resolves
    EnemyWithRevealedShadow
  | UniqueEnemy
  | -- | an enemy with an attachment of the trait attached
    EnemyWithAttached Trait
  | -- | an enemy player card effects can reach
    EnemyNotImmuneToPlayerCards
  | -- | an enemy whose engagement cost is higher than the player's threat
    EnemyWithEngagementCostAboveThreatOf PlayerId
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
  | LocationWithCardCode CardCode
  | -- | a location player card effects can reach
    LocationNotImmuneToPlayerCards
  | AnyLocation
  | NotLocation LocationMatcher
  | LocationMatchAll [LocationMatcher]
  | -- | a location matching any of them
    LocationMatchAny [LocationMatcher]
  | -- | a location in a matching player's play area, under their control, as Osgiliath's
    LocationControlledBy PlayerMatcher
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

-- | A card wherever it is; state-dependent matchers require matchCard.
data CardMatcher
  = CardWithCode CardCode
  | CardWithTrait Trait
  | CardSharesTraitWithHero HeroMatcher
  | CardWithType CardType
  | CardWithSphere Sphere
  | AnyCard
  | NotCard CardMatcher
  | CardMatchAll [CardMatcher]
  | CardMatchAny [CardMatcher]
  | -- | a card printed with the Doomed keyword, Doomed X among them
    CardWithDoomed
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

cardMatches :: HasCardDef a => CardMatcher -> a -> Bool
cardMatches matcher (toCardDef -> def) = case matcher of
  CardWithCode code -> cdCardCode def == code
  CardWithTrait trait -> trait `member` cdCardTraits def
  CardSharesTraitWithHero _ -> error "CardSharesTraitWithHero requires matchCard"
  CardWithType cType -> cdCardType def == cType
  CardWithSphere sphere -> cdSphere def == Just sphere
  AnyCard -> True
  NotCard m -> not $ cardMatches m def
  CardMatchAll ms -> all (`cardMatches` def) ms
  CardMatchAny ms -> any (`cardMatches` def) ms
  CardWithDoomed -> any (\case Doomed _ -> True; _ -> False) (cdKeywords def)
