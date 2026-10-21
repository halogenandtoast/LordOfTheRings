module LordOfTheRings.Criteria (module LordOfTheRings.Criteria) where

import LordOfTheRings.Card.CardType
import LordOfTheRings.Id
import LordOfTheRings.Matcher.Types
import LordOfTheRings.Modifier (ModifierType)
import LordOfTheRings.Target (Target)
import LordOfTheRings.Phase (Phase)
import LordOfTheRings.Prelude
import LordOfTheRings.Sphere (Sphere)

data Criterion
  = NoCriteria
  | LocationExists LocationMatcher
  | EnemyExists EnemyMatcher
  | HeroExists HeroMatcher
  | AllyExists AllyMatcher
  | -- | a card of that type in hand that the unique rule lets into play
    CardInHand PlayerId CardType
  | PlayerExists PlayerMatcher
  | DuringPhase Phase
  | -- | no enemy or location in play is that card
    CardNotInPlay CardId
  | -- | a matching player has this many resources of the sphere to spend
    ResourcesOfSphere PlayerMatcher Sphere Int
  | -- | in the quest phase, before any character is committed
    BeforeCommitting
  | -- | nothing stops a player card cancelling an encounter card's effects
    EncounterEffectsCancelable
  | NotCriterion Criterion
  | -- | the target has, or lacks, the modifier
    TargetHasModifier Target ModifierType
  | TargetLacksModifier Target ModifierType
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

-- | The player controls a matching ally, as a card that returns one to hand needs.
controlsAlly :: PlayerId -> AllyMatcher -> Criterion
controlsAlly pid m = AllyExists (AllyControlledBy (PlayerWithId pid) <> m)
