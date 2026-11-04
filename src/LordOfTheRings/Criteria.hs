module LordOfTheRings.Criteria (module LordOfTheRings.Criteria) where

import LordOfTheRings.Campaign (CampaignLogKey)
import LordOfTheRings.Card.CardType
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Heading (HeadingMatcher)
import LordOfTheRings.Id
import LordOfTheRings.Matcher.Types
import LordOfTheRings.Modifier (ModifierType (CannotCancelShadowEffects, CannotCancelWhenRevealedEffects))
import LordOfTheRings.Target (Target (GameTarget))
import LordOfTheRings.Phase (Phase)
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Trait (Trait)

data Criterion
  = NoCriteria
  | LocationExists LocationMatcher
  | EnemyExists EnemyMatcher
  | HeroExists HeroMatcher
  | AllyExists AllyMatcher
  | -- | a card of that type in hand that the unique rule lets into play
    CardInHand PlayerId CardType
  | -- | a matching card in hand that the unique rule lets into play
    CardInHandMatching PlayerId CardMatcher
  | PlayerExists PlayerMatcher
  | DuringPhase Phase
  | -- | no enemy or location in play is that card
    CardNotInPlay CardId
  | -- | a matching player has this many resources of the sphere to spend
    ResourcesOfSphere PlayerMatcher Sphere Int
  | -- | an ally in any player's discard pile whose printed cost the player has the
    -- sphere's resources for
    AffordableAllyInDiscard PlayerId Sphere
  | -- | an ally in hand affordable after a reduction, with a minimum cost
    AffordableAllyInHand PlayerId Sphere Int Int
  | -- | in the quest phase, before any character is committed
    BeforeCommitting
  | -- | in the quest phase, before its staging step, as "Play only before the staging step"
    BeforeStaging
  | -- | nothing stops a player card cancelling an encounter card's effects
    EncounterEffectsCancelable
  | -- | the treachery card being revealed is the first revealed this round
    FirstTreacheryRevealedThisRound
  | -- | a player played a copy of the card this round, as "Only 1 copy of Tighten Our Belts
    -- can be played by the players each round" reads
    CardPlayedThisRound CardCode
  | NotCriterion Criterion
  | -- | the target has, or lacks, the modifier
    TargetHasModifier Target ModifierType
  | TargetLacksModifier Target ModifierType
  | -- | at least this many characters match: heroes and allies
    CharactersAtLeast Int HeroMatcher AllyMatcher
  | -- | a different character for each pair of matchers, heroes and allies: "a Rohan
    -- character and another Gondor character"
    DistinctCharacters [(HeroMatcher, AllyMatcher)]
  | -- | a matching card is in the victory display
    CardInVictoryDisplay CardMatcher
  | -- | fewer than this many matching cards are in the victory display, as "Limit 3 copies
    -- of None Return in the victory display" reads
    FewerInVictoryDisplay Int CardMatcher
  | -- | a matching card is in the player's discard pile
    CardInDiscard PlayerId CardMatcher
  | -- | a matching card is in any player's discard pile, as The Houses of Healing needs a
    -- hero there
    CardInAnyDiscard CardMatcher
  | -- | the main quest has no keywords, printed or gained, as Backtrack reads
    MainQuestWithoutKeywords
  | Criteria [Criterion]
  | AnyCriteria [Criterion]
  | -- | at least this many enemies match
    EnemiesAtLeast Int EnemyMatcher
  | -- | the heading reads as the matcher says, as "if you are on-course"
    HeadingMatches HeadingMatcher
  | -- | the game is dealt in nightmare mode, as a setup card changes a stage's ability
    InNightmareMode
  | -- | in campaign mode, the log holds a card under the key, as a secret yet unflipped
    CampaignRecorded CampaignLogKey
  | -- | the characters the player controls have at least this many different traits
    -- between them, as The Free Peoples reads
    DifferentTraitsAtLeast PlayerId Int
  | -- | a Condition attachment is in play, as Bulwark of the West discards
    ConditionAttachmentExists
  | -- | a matching card is in the player's hand, whether or not it could be played, as
    -- Kahliel discards a Harad ally
    CardInHandToDiscard PlayerId CardMatcher
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

-- | Each hero the player controls has the sphere's printed resource icon, as "Play only
-- if each hero you control has a printed [lore] resource icon" reads.
eachHeroHasPrintedIcon :: PlayerId -> Sphere -> Criterion
eachHeroHasPrintedIcon pid sphere = NotCriterion $ HeroExists (HeroControlledBy (PlayerWithId pid) <> NotHero (HeroWithPrintedSphere sphere))

-- | The player controls a different unique character with each trait, as "Play only if you
-- control a unique character with the Warrior trait and another unique character with the
-- Scout trait" reads.
uniqueCharactersWith :: PlayerId -> [Trait] -> Criterion
uniqueCharactersWith pid = DistinctCharacters . map \trait ->
  (HeroControlledBy you <> HeroWithTrait trait, AllyControlledBy you <> UniqueAlly <> AllyWithTrait trait)
 where
  you = PlayerWithId pid

-- | The player may resolve a Valour trigger: his threat is 40 or higher.
valour :: PlayerId -> Criterion
valour pid = PlayerExists (PlayerWithId pid <> PlayerWithThreatAtLeast 40)

-- | The player's cards follow the FAQ's errata, as a card's errata'd ability needs.
usesErrata :: PlayerId -> Criterion
usesErrata pid = PlayerExists (PlayerWithId pid <> PlayerUsingErrata)

-- | The player's cards are played as printed.
withoutErrata :: PlayerId -> Criterion
withoutErrata = NotCriterion . usesErrata

-- | The player controls a matching ally, as a card that returns one to hand needs.
controlsAlly :: PlayerId -> AllyMatcher -> Criterion
controlsAlly pid m = AllyExists (AllyControlledBy (PlayerWithId pid) <> m)

-- | Nothing stops a player card cancelling a shadow effect.
shadowEffectsCancelable :: Criterion
shadowEffectsCancelable = EncounterEffectsCancelable <> TargetLacksModifier GameTarget CannotCancelShadowEffects

-- | Nothing stops a player card cancelling a When Revealed effect.
whenRevealedCancelable :: Criterion
whenRevealedCancelable = EncounterEffectsCancelable <> TargetLacksModifier GameTarget CannotCancelWhenRevealedEffects
