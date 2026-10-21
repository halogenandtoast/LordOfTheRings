module LordOfTheRings.Modifier (module LordOfTheRings.Modifier) where

import LordOfTheRings.Card.CardType (CardType)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Keyword (Keyword)
import LordOfTheRings.Matcher.Types (CardMatcher, EnemyMatcher)
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Sphere
import LordOfTheRings.Target (Target)
import LordOfTheRings.Trait

data ModifierType
  = WillpowerModifier Int
  | AttackModifier Int
  | DefenseModifier Int
  | HitPointModifier Int
  | ThreatModifier Int
  | EngagementCostModifier Int
  | GainTrait Trait
  | GainResourceIcon Sphere
  | ReturnToHandAtEndOfPhase
  | ShuffleIntoDeckAtEndOfPhase
  | -- | a card in the staging area adds nothing to the staging total
    DoesNotContributeThreat
  | -- | on a card in play: player card effects do not touch it
    ImmuneToPlayerCardEffects
  | CannotAttack
  | -- | on an enemy: it cannot attack that player, though it may attack others
    CannotAttackPlayer PlayerId
  | CannotAttackOrDefend
  | CannotHaveAttachments
  | -- | no attachment with the restricted keyword can be attached to it
    CannotHaveRestrictedAttachments
  | -- | on the game: the total threat in the staging area is raised by this much
    StagingThreatBonus Int
  | -- | on the game: reveal this many more encounter cards during staging
    AdditionalStagingReveals Int
  | NoEngagementChecks
  | SkipStagingStep
  | -- | on the game: the players, as a group, cannot play more than 1 ally each round
    OneAllyPerRound
  | AllyPlayedThisRound
  | -- | on an ally: it entered play this round
    EnteredPlayThisRound
  | -- | damage an attack deals past the character's remaining hit points raises threat
    ExcessDamageToThreat
  | CannotBeOptionallyEngagedWhileOthersInStaging
  | DoesNotCountDefense
  | -- | on a location: when the players may travel and there is no active location, they
    -- must travel here
    MustBeTraveledTo
  | LeavesPlayToTopOfEncounterDeck
  | ShuffledIntoEncounterDeckWhenDefeated
  | ReturnToTopOfEncounterDeckAfterCombat
  | -- | on a location: the players cannot travel there now (its travel cost cannot be paid)
    CannotBeTraveledTo
  | -- | on a player
    CannotDrawCards
  | DoesNotReadyDuringRefresh
  | DoesNotExhaustToAttack
  | DoesNotExhaustToDefend
  | -- | on a character: matching attachments played onto it cost this much less
    ReduceCostToAttach CardMatcher Int
  | -- | on an enemy: any player's characters may attack it, wherever it is
    AttackableByAnyCharacter
  | -- | when attacking an enemy with the trait, attack is raised by this much
    AttackBonusAgainst Trait Int
  | -- | the hero may attack an enemy in the staging area when attacking alone
    CanAttackStagingAreaAlone
  | -- | the character may be declared as an attacker against enemies in the staging area
    CanAttackStagingArea
  | -- | when attacking a matching enemy, attack is raised by this much
    AttackBonusAgainstMatching EnemyMatcher Int
  | -- | the character is discarded once the attack it makes is over
    DiscardAfterAttack
  | -- | on an ally, or its card: it is discarded at the end of the round
    DiscardAtEndOfRound
  | -- | on a hero: while paying for a matching card it has the sphere's resource icon
    GainResourceIconFor CardMatcher Sphere
  | -- | on a player: the enemies engaged with them cannot attack them
    EngagedEnemiesCannotAttack
  | -- | on a player: they may declare any number of defenders against each attack
    MayDeclareMultipleDefenders
  | -- | on a player
    CannotCommitCharacters
  | GainKeyword Keyword
  | -- | on a character: it defends against enemies with the trait without exhausting
    DoesNotExhaustToDefendAgainst Trait
  | CannotCommitToQuest
  | -- | on a card in play: its abilities cannot be used, nor its Forced effects trigger
    CannotTriggerAbilities
  | -- | on a hero
    CannotCollectResources
  | -- | on the game: resources of any sphere may be spent as this sphere's
    ResourcesSpendableAs Sphere
  | CannotBeHealed
  | -- | on a character: no single effect heals it of more than this
    HealLimit Int
  | -- | on an enemy: only characters with the trait or the keyword can attack it or defend
    -- against it
    OnlyFoughtBy Trait Keyword
  | -- | on an enemy: damage from its undefended attacks is dealt to this character
    UndefendedDamageTo Target
  | -- | on an ally: the resources on it can pay for matching cards its controller plays
    ResourcesPayFor CardMatcher
  | -- | on the game: players cannot play matching cards
    CannotPlay CardMatcher
  | -- | on the game: attachments' printed text boxes are treated as blank
    BlankAttachments
  | -- | on the game: every character must commit to the quest
    MustCommitAll
  | -- | on the game: each card dealt for an escape test adds this much more
    EscapeBonus Int
  | -- | on a character: damage dealt to it is dealt to the given character instead
    DamageRedirectedTo Target
  | -- | on a character: it commits to the quest without exhausting
    DoesNotExhaustToCommit
  | -- | on the game: characters with the trait commit to the quest without exhausting,
    -- those that come into play or gain it later too
    TraitDoesNotExhaustToCommit Trait
  | -- | on the game: no player card may cancel an encounter card's effects
    CannotCancelEncounterEffects
  | -- | on the game: matching cards cost this much more to play from hand
    IncreaseCost CardMatcher Int
  | -- | on a player: matching cards they play from hand cost this much less
    ReduceCostOf CardMatcher Int
  | -- | on a card in hand, as its card id: it costs this much more to play (less, when
    -- negative)
    CostModifier Int
  | -- | on a player, or on the game for every player: the next matching card played
    -- costs this much less, to no less than 1
    ReduceNextCostOf CardMatcher Int
  | -- | on a character: it is defending the attack being resolved
    Defending
  | -- | on a card in hand, as its card id: its player may discard this many cards from
    -- the top of their deck to lower its cost by as much
    ReducedByDiscardingFromDeck Int
  | -- | on a character: the enemy it attacks alone does not count its defense
    IgnoresDefenseWhenAttackingAlone
  | -- | on a player: an enemy engaged them this round
    EngagedAnEnemyThisRound
  | -- | on a player: they played a copy of the card this phase
    PlayedThisPhase CardCode
  | -- | on a card: the card type its controller named
    NamedCardType CardType
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
