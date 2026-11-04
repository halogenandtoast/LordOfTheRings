module LordOfTheRings.Matcher.Types (module LordOfTheRings.Matcher.Types) where

import LordOfTheRings.Attachment.CardDefs qualified as Attachments
import LordOfTheRings.Card.CardDef
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Doomed, Guarded, GuardedBy))
import LordOfTheRings.Location.Grid (Pos)
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
  | -- | a player who may resolve a Valour trigger: his threat is 40 or higher, 80 or higher
    -- while the main quest has the dire keyword
    PlayerAtValourThreat
  | PlayerWithCardsInHand Int
  | PlayerMatchAll [PlayerMatcher]
  | -- | a player with a hidden card in their play area, as The Blood of Gondor deals them
    PlayerWithHiddenCards
  | -- | a player with at least this many cards in their deck, as a cost to discard them
    PlayerWithCardsInDeck Int
  | -- | a player whose cards follow the FAQ's errata
    PlayerUsingErrata
  | -- | a player whose threat is equal to or higher than the enemy's engagement cost, as City
    -- Guard is "considered to be engaged with" each
    PlayerWithThreatAtLeastEngagementCostOf EnemyId
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
  | -- | in play in the staging area, under no player's control (The Long Arm of Mordor)
    HeroInStagingArea
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
  | -- | a hero with a ready copy of the card attached, as a Master card exhausts The One Ring
    HeroWithReadyAttachment CardCode
  | -- | a hero with a player card attached, as "with a player attachment" reads
    HeroWithPlayerAttachment
  | -- | a hero with a ready player card attached, as Bartering chooses one
    HeroWithReadyPlayerAttachment
  | AnyHero
  | NotHero HeroMatcher
  | HeroMatchAll [HeroMatcher]
  | -- | a hero matching any of them
    HeroMatchAny [HeroMatcher]
  | -- | a hero whose printed resource icon is the sphere's, not one it gains
    HeroWithPrintedSphere Sphere
  | -- | a hero with at least this many restricted cards attached, as Forth, The Three
    -- Hunters! reads "at least 2 restricted attachments"
    HeroWithRestrictedAttachmentsAtLeast Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The hero with The One Ring attached, as Master cards read.
heroWithTheOneRing :: HeroMatcher
heroWithTheOneRing = HeroWithAttachment (cdCardCode Attachments.theOneRing)

-- | The hero with The One Ring attached, the ring ready, as a Master card exhausts it.
heroWithReadyOneRing :: HeroMatcher
heroWithReadyOneRing = HeroWithReadyAttachment (cdCardCode Attachments.theOneRing)

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
  | -- | an ally with a player card attached, as "with a player attachment" reads
    AllyWithPlayerAttachment
  | -- | an ally with a ready player card attached, as Bartering chooses one
    AllyWithReadyPlayerAttachment
  | -- | an ally with a ready attachment of the trait attached, as Swift and Strong
    -- exhausts a Weapon; any attachment without one, as Wiglaf exhausts one
    AllyWithReadyAttached (Maybe Trait)
  | -- | an ally with an attachment of the card code attached, as "Limit 1 per ally" reads
    AllyWithAttachment CardCode
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
  | -- | a quest card in play, a side quest among them, as The Long Defeat
    AttachToQuest
  | -- | a player's threat dial, as Favor of the Valar
    AttachToPlayer
  | -- | played into the staging area unattached, as a Trap
    PlayIntoStagingArea
  | -- | played into its owner's play area unattached, as Followed
    PlayIntoPlayArea
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data EnemyMatcher
  = EnemyWithId EnemyId
  | EnemyInStagingArea
  | -- | an enemy in the Stormcaller's area, or considered to be (Flight of the Stormcaller)
    EnemyInStormcallerArea
  | EnemyWithTrait Trait
  | EnemyWithCardCode CardCode
  | -- | an enemy of the title, whatever its subtitle or version, as "Thane Ulchor" reads of
    -- the scenario's own copy
    EnemyWithTitle Text
  | -- | the enemy whose card this is, as a card taken from a deck is found once in play
    EnemyWithCardId CardId
  | EnemyEngagedWith PlayerMatcher
  | -- | an enemy dealt a shadow card still face down, one player card effects can choose
    -- (see 'shadowsPlayerCardsCanAffect'): only player cards look for one
    EnemyWithFacedownShadow
  | -- | an enemy whose shadow card has turned face up, as its attack resolves
    EnemyWithRevealedShadow
  | UniqueEnemy
  | -- | an enemy with an attachment of the trait attached
    EnemyWithAttached Trait
  | -- | an enemy player card effects can reach
    EnemyNotImmuneToPlayerCards
  | -- | an enemy with any card attached to it, as Ranger Spear reads
    EnemyWithAnyAttachment
  | -- | an enemy whose engagement cost is higher than the player's threat
    EnemyWithEngagementCostAboveThreatOf PlayerId
  | -- | an enemy with at least 1 resource token on it, as The Shattered Monument reads
    EnemyWithResources
  | -- | an enemy attached to a matching location by the Grapple keyword (The Thing in the
    -- Depths)
    EnemyGrappledWith LocationMatcher
  | AnyEnemy
  | NotEnemy EnemyMatcher
  | EnemyMatchAll [EnemyMatcher]
  | -- | an enemy with the keyword, printed or gained, as "with the ambush keyword" reads
    EnemyWithKeyword Keyword
  | -- | an enemy guarding a card: an objective, a captive, a location or a player card, as
    -- "an enemy guarding a card" reads (Unseen Danger)
    EnemyGuardingACard
  | -- | an enemy guarding a hero as its captive, as Carried Away reads
    EnemyGuardingAHero
  | -- | an enemy with as much damage as its hit points, as an Indestructible enemy may have
    -- (The Great Worm)
    EnemyWithNoHitPointsRemaining
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Thane Ulchor, whichever of his versions the scenario uses (A Shadow in the East's
-- Ulchor's Guard set reads him by title).
thaneUlchor :: EnemyMatcher
thaneUlchor = EnemyWithTitle "Thane Ulchor"

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
  | LocationWithKeyword Keyword
  | -- | a location with the Discover keyword, whatever its value (MEC69 rules)
    LocationWithDiscover
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
  | -- | a location whose id sorts before the given one's: of several copies, the first
    -- stands for them all, as an ability that does not stack is triggered by one
    LocationWithIdBelow LocationId
  | UniqueLocation
  | -- | a location with the same title as the active location, the active one among them,
    -- as Explore Secret Ways reads
    LocationWithActiveLocationTitle
  | -- | a location player card effects may discard from play
    LocationDiscardableByPlayerCards
  | -- | the location lying there on the Island Map (Temple of the Deceived)
    LocationAtPosition Pos
  | -- | a location that can enter the staging area, as Shrine to Morgoth cannot
    LocationCanEnterStagingArea
  | -- | a location guarding a card, as 'EnemyGuardingACard' (Find Durin's Key)
    LocationGuardingACard
  | -- | a location with a player card attached to it, as South Away! reads
    LocationWithPlayerCardAttachment
  | -- | a location with any card attached to it, as Haldan draws for each
    LocationWithAttachment
  | -- | a location with no progress tokens on it, as Woodmen's Path attaches to
    LocationWithoutProgress
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
  | -- | a card printed with a Valour trigger, as Hope Rekindled reads
    CardWithValour
  | -- | a unique card, as "a non-unique location" reads it negated
    UniqueCard
  | -- | a card of the title, whatever its subtitle: a copy, as Heavy Curse reads
    CardWithTitle Text
  | -- | a card printed with the guarded keyword, Guarded (X) among them, as "a Guarded
    -- attachment" reads (The King's Return)
    CardWithGuarded
  | -- | a card printed with exactly this cost, as Long Lake Fisherman names
    CardWithPrintedCost Int
  | -- | a card printed with at least this cost, as "printed cost 3 or more" reads
    CardWithPrintedCostAtLeast Int
  | -- | a card printed with the keyword, as "a restricted attachment" reads
    CardWithKeyword Keyword
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

-- | An Easterling treachery, as Warrior of Rhûn attaches the top one in the encounter
-- discard pile: one that counts as an attachment once on an enemy, an Easterling
-- attachment ('attachedWithTrait' Easterling).
easterlingTreachery :: CardMatcher
easterlingTreachery = CardWithType TreacheryType <> CardWithTrait Easterling

-- | A side quest, the players' or the encounter deck's.
sideQuestCard :: CardMatcher
sideQuestCard = CardMatchAny [CardWithType PlayerSideQuestType, CardWithType EncounterSideQuestType]

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
  CardWithValour -> cdCardCode def `elem` valourCards
  UniqueCard -> cdUnique def
  CardWithTitle title -> let Name t _ = cdName def in t == title
  CardWithGuarded -> any (\case Guarded -> True; GuardedBy _ -> True; _ -> False) (cdKeywords def)
  CardWithPrintedCost n -> cdCost def == Just (StaticCost n)
  CardWithPrintedCostAtLeast n -> case cdCost def of
    Just (StaticCost c) -> c >= n
    _ -> False
  CardWithKeyword k -> k `member` cdKeywords def

-- | The player cards printed with a Valour trigger.
valourCards :: [CardCode]
valourCards = ["10003", "10004", "10063", "10088", "10117", "10119", "10144", "17058", "17084", "17110", "17113", "17118"]
