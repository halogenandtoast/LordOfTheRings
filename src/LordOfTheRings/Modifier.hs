module LordOfTheRings.Modifier (module LordOfTheRings.Modifier) where

import LordOfTheRings.Keyword (Keyword)
import LordOfTheRings.Matcher.Types (CardMatcher)
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
  | CannotAttack
  | CannotAttackOrDefend
  | CannotHaveAttachments
  | -- | on the game: the total threat in the staging area is raised by this much
    StagingThreatBonus Int
  | -- | on the game: reveal this many more encounter cards during staging
    AdditionalStagingReveals Int
  | NoEngagementChecks
  | SkipStagingStep
  | -- | on the game: the players, as a group, cannot play more than 1 ally each round
    OneAllyPerRound
  | AllyPlayedThisRound
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
  | -- | when attacking an enemy with the trait, attack is raised by this much
    AttackBonusAgainst Trait Int
  | -- | the hero may attack an enemy in the staging area when attacking alone
    CanAttackStagingAreaAlone
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
  | -- | on the game: no player card may cancel an encounter card's effects
    CannotCancelEncounterEffects
  | -- | on the game: matching cards cost this much more to play from hand
    IncreaseCost CardMatcher Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
