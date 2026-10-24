module LordOfTheRings.Matcher.Window (module LordOfTheRings.Matcher.Window) where

import LordOfTheRings.Id
import LordOfTheRings.Matcher.Types (CardMatcher, HeroMatcher, cardMatches)
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Timing
import LordOfTheRings.Trait
import LordOfTheRings.Window

data WindowMatcher
  = EntersPlayWindow Timing Target
  | PhaseBeginsWindow Timing Phase
  | CommittedToQuestWindow Timing Target
  | EnemyEngagedWindow Timing EnemyId
  | -- | any enemy engaged with the player
    PlayerEngagedEnemyWindow Timing PlayerId
  | OptionallyEngagedWindow Timing PlayerId
  | EnemyEntersStagingAreaWindow Timing
  | GainedResourcesWindow Timing Target
  | DealtDamageWindow Timing Target
  | TraveledWindow Timing LocationId
  | AnyTraveledWindow Timing
  | -- | an enemy destroyed by an attack, by any attackers or with the given one among them
    EnemyDestroyedByAttackWindow Timing (Maybe Target)
  | {- | an enemy engaged with a player other than the given one, destroyed by an attack
    the character was among
    -}
    EnemyOfOtherPlayerDestroyedByAttackWindow Timing PlayerId Target
  | CharacterDestroyedWindow Timing
  | HeroDestroyedWindow Timing
  | AllyDestroyedWindow Timing
  | EnemyRevealedWindow Timing
  | TreacheryRevealedWindow Timing
  | ShadowEffectWindow Timing
  | ExploredWindow Timing LocationId
  | PhaseEndsWindow Timing Phase
  | -- | at the end of the round
    RoundEndsWindow
  | ProgressPlacedWindow Timing Target
  | QuestedUnsuccessfullyWindow Timing
  | QuestedSuccessfullyWindow Timing
  | -- | after an attack the character, or any character, defended
    DefendedAttackWindow Timing (Maybe Target)
  | PlayedFromHandWindow Timing Target
  | LocationRevealedWindow Timing
  | ShadowEffectAgainstWindow Timing Target
  | AnyEnemyAttacksWindow Timing
  | -- | a character whose printed card matches left play
    CharacterLeftPlayWindow Timing CardMatcher
  | -- | after an attack the character made resolved
    AttackedWindow Timing Target
  | ShadowEffectResolvedWindow Timing EnemyId
  | EnemyAttacksWindow Timing EnemyId
  | -- | any encounter card revealed from the encounter deck
    CardRevealedWindow Timing
  | -- | a character, or any character, declared as a defender
    DefenderDeclaredWindow Timing (Maybe Target)
  | AllyLeftPlayWindow Timing
  | -- | a character, or any character, declared as an attacker
    AttackerDeclaredWindow Timing (Maybe Target)
  | -- | a hero with the trait, controlled by the player, left play
    HeroLeftPlayWindow Timing PlayerId Trait
  | -- | an ally with the trait, controlled by the player, left play
    AllyWithTraitLeftPlayWindow Timing PlayerId Trait
  | -- | a character, or one with the trait, readied during the refresh phase
    ReadiedWindow Timing (Maybe Trait)
  | -- | a character healed by anything but the source
    HealedByOtherWindow Timing Source
  | -- | an ally entered play, under the player and with the trait when given
    AllyEnteredPlayWindow Timing (Maybe PlayerId) (Maybe Trait)
  | -- | a card, or any card, discarded from the top of the player's deck
    DiscardedFromDeckWindow Timing PlayerId (Maybe CardId)
  | -- | the player played a matching card from their hand
    PlayedCardFromHandWindow Timing PlayerId CardMatcher
  | -- | any hero dealt damage
    HeroDealtDamageWindow Timing
  | -- | the character itself left play, for its own abilities to answer
    LeftPlayWindow Timing Target
  | -- | a character sharing a trait with the given one left play
    LeftPlaySharingTraitWindow Timing Target
  | -- | the character attacked alone, and dealt damage beyond what the enemy had left
    AttackedAloneWithExcessWindow Timing Target
  | -- | an enemy destroyed by an attack a matching hero made
    EnemyDestroyedByHeroWindow Timing HeroMatcher
  | -- | an encounter or quest card's effect raised the player's threat
    ThreatRaisedByEncounterWindow Timing PlayerId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

windowMatches :: WindowMatcher -> Window -> Bool
windowMatches matcher (Window timing wType) = case (matcher, wType) of
  (EntersPlayWindow t target, EntersPlay target') -> t == timing && target == target'
  (PhaseBeginsWindow t phase, PhaseBegins phase') -> t == timing && phase == phase'
  (CommittedToQuestWindow t target, CommittedToQuest target') -> t == timing && target == target'
  (EnemyEngagedWindow t eid, EnemyEngaged eid' _) -> t == timing && eid == eid'
  (PlayerEngagedEnemyWindow t pid, EnemyEngaged _ pid') -> t == timing && pid == pid'
  (OptionallyEngagedWindow t pid, OptionallyEngaged _ pid') -> t == timing && pid == pid'
  (EnemyEntersStagingAreaWindow t, EntersStagingArea (EnemyTarget _)) -> t == timing
  (GainedResourcesWindow t target, GainedResources target') -> t == timing && target == target'
  (DealtDamageWindow t target, DealtDamage target' _) -> t == timing && target == target'
  (TraveledWindow t lid, Traveled lid') -> t == timing && lid == lid'
  (AnyTraveledWindow t, Traveled _) -> t == timing
  (DefenderDeclaredWindow t who, DeclaredAsDefender target _) -> t == timing && maybe True (== target) who
  (EnemyDestroyedByAttackWindow t who, EnemyDestroyedByAttack _ attackers) -> t == timing && maybe True (`elem` attackers) who
  (EnemyOfOtherPlayerDestroyedByAttackWindow t pid who, EngagedEnemyDestroyedByAttack pid' attackers) -> t == timing && pid /= pid' && who `elem` attackers
  (CharacterDestroyedWindow t, CharacterDestroyed _ _) -> t == timing
  (HeroDestroyedWindow t, CharacterDestroyed (HeroTarget _) _) -> t == timing
  (AllyDestroyedWindow t, CharacterDestroyed (AllyTarget _) _) -> t == timing
  (EnemyRevealedWindow t, Revealed (EnemyTarget _)) -> t == timing
  (TreacheryRevealedWindow t, Revealed (TreacheryTarget _)) -> t == timing
  (ShadowEffectWindow t, ShadowEffectTriggered _ _) -> t == timing
  (ExploredWindow t lid, Explored lid') -> t == timing && lid == lid'
  (PhaseEndsWindow t phase, PhaseEnds phase') -> t == timing && phase == phase'
  (RoundEndsWindow, RoundEnds) -> True
  (ProgressPlacedWindow t target, ProgressPlaced target' _) -> t == timing && target == target'
  (QuestedUnsuccessfullyWindow t, QuestedUnsuccessfully) -> t == timing
  (QuestedSuccessfullyWindow t, QuestedSuccessfully) -> t == timing
  (DefendedAttackWindow t who, DefendedAttack target _) -> t == timing && maybe True (== target) who
  (PlayedFromHandWindow t target, PlayedFromHand target') -> t == timing && target == target'
  (LocationRevealedWindow t, Revealed (LocationTarget _)) -> t == timing
  (ShadowEffectAgainstWindow t target, ShadowEffectAgainst target') -> t == timing && target == target'
  (AnyEnemyAttacksWindow t, EnemyAttacking _ _) -> t == timing
  (CharacterLeftPlayWindow t m, CharacterLeftPlay c) -> t == timing && cardMatches m c
  (AttackedWindow t target, AttackedEnemy target' _) -> t == timing && target == target'
  (ShadowEffectResolvedWindow t eid, ShadowEffectResolved _ eid') -> t == timing && eid == eid'
  (EnemyAttacksWindow t eid, EnemyAttacking eid' _) -> t == timing && eid == eid'
  (AllyLeftPlayWindow t, LeftPlay (AllyTarget _) _ _) -> t == timing
  (AttackerDeclaredWindow t who, DeclaredAsAttacker target _) -> t == timing && maybe True (== target) who
  (HeroLeftPlayWindow t pid trait, LeftPlay (HeroTarget _) pid' traits) -> t == timing && pid == pid' && trait `member` traits
  (AllyWithTraitLeftPlayWindow t pid trait, LeftPlay (AllyTarget _) pid' traits) -> t == timing && pid == pid' && trait `member` traits
  (ReadiedWindow t trait, Readied _ traits) -> t == timing && maybe True (`member` traits) trait
  (HealedByOtherWindow t source, Healed _ source') -> t == timing && source /= source'
  (AllyEnteredPlayWindow t pid trait, AllyEnteredPlay _ pid' traits) ->
    t == timing && maybe True (== pid') pid && maybe True (`member` traits) trait
  (DiscardedFromDeckWindow t pid card, DiscardedFromDeck pid' card') -> t == timing && pid == pid' && maybe True (== card') card
  (PlayedCardFromHandWindow t pid m, PlayedCardFromHand pid' code) -> t == timing && pid == pid' && cardMatches m code
  (HeroDealtDamageWindow t, DealtDamage (HeroTarget _) _) -> t == timing
  (LeftPlayWindow t target, LeftPlay target' _ _) -> t == timing && target == target'
  (AttackedAloneWithExcessWindow t target, AttackedAlone target' _ n) -> t == timing && target == target' && n > 0
  (ThreatRaisedByEncounterWindow t pid, ThreatRaisedByEncounter pid') -> t == timing && pid == pid'
  -- these read the game, so "LordOfTheRings.Helpers.Window" matches them
  (LeftPlaySharingTraitWindow {}, _) -> False
  (EnemyDestroyedByHeroWindow {}, _) -> False
  (CardRevealedWindow {}, _) -> False
  _ -> False
