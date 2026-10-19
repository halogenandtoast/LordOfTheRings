module LordOfTheRings.Matcher.Window (module LordOfTheRings.Matcher.Window) where

import LordOfTheRings.Id
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Timing
import LordOfTheRings.Trait
import LordOfTheRings.Window

data WindowMatcher
  = EntersPlayWindow Timing Target
  | PhaseBeginsWindow Timing Phase
  | CommittedToQuestWindow Timing Target
  | EnemyEngagedWindow Timing EnemyId
  | DealtDamageWindow Timing Target
  | TraveledWindow Timing LocationId
  | AnyTraveledWindow Timing
  | -- | an enemy destroyed by an attack, by any attackers or with the given one among them
    EnemyDestroyedByAttackWindow Timing (Maybe Target)
  | CharacterDestroyedWindow Timing
  | EnemyRevealedWindow Timing
  | TreacheryRevealedWindow Timing
  | ShadowEffectWindow Timing
  | -- | any encounter card revealed from the encounter deck
    CardRevealedWindow Timing
  | -- | a character, or any character, declared as a defender
    DefenderDeclaredWindow Timing (Maybe Target)
  | AllyLeftPlayWindow Timing
  | -- | a hero with the trait, controlled by the player, left play
    HeroLeftPlayWindow Timing PlayerId Trait
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

windowMatches :: WindowMatcher -> Window -> Bool
windowMatches matcher (Window timing wType) = case (matcher, wType) of
  (EntersPlayWindow t target, EntersPlay target') -> t == timing && target == target'
  (PhaseBeginsWindow t phase, PhaseBegins phase') -> t == timing && phase == phase'
  (CommittedToQuestWindow t target, CommittedToQuest target') -> t == timing && target == target'
  (EnemyEngagedWindow t eid, EnemyEngaged eid' _) -> t == timing && eid == eid'
  (DealtDamageWindow t target, DealtDamage target' _) -> t == timing && target == target'
  (TraveledWindow t lid, Traveled lid') -> t == timing && lid == lid'
  (AnyTraveledWindow t, Traveled _) -> t == timing
  (DefenderDeclaredWindow t who, DeclaredAsDefender target _) -> t == timing && maybe True (== target) who
  (EnemyDestroyedByAttackWindow t who, EnemyDestroyedByAttack _ attackers) -> t == timing && maybe True (`elem` attackers) who
  (CharacterDestroyedWindow t, CharacterDestroyed _) -> t == timing
  (EnemyRevealedWindow t, Revealed (EnemyTarget _)) -> t == timing
  (TreacheryRevealedWindow t, Revealed (TreacheryTarget _)) -> t == timing
  (ShadowEffectWindow t, ShadowEffectTriggered _ _) -> t == timing
  (CardRevealedWindow t, Revealed _) -> t == timing
  (AllyLeftPlayWindow t, LeftPlay (AllyTarget _) _ _) -> t == timing
  (HeroLeftPlayWindow t pid trait, LeftPlay (HeroTarget _) pid' traits) -> t == timing && pid == pid' && trait `member` traits
  _ -> False
