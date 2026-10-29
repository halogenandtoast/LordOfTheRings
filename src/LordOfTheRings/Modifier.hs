module LordOfTheRings.Modifier (module LordOfTheRings.Modifier) where

import LordOfTheRings.Card.CardType (CardType)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Id (PlayerId)
import LordOfTheRings.Keyword (Keyword)
import LordOfTheRings.Matcher.Types (CardMatcher, EnemyMatcher, PlayerMatcher)
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Sphere
import LordOfTheRings.Target (Target)
import LordOfTheRings.Trait

data ModifierType
  = WillpowerModifier Int
  | AttackModifier Int
  | -- | on an enemy: its attack is raised by this much when undefended, as Goblin
    -- Swordsman's
    AttackModifierIfUndefended Int
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
  | -- | on a location: no card effect, player or encounter, places or removes its tokens
    ImmuneToCardEffects
  | -- | on a location: player card effects cannot place progress on it, as Retake the City
    -- reads for the staging area
    NoProgressFromPlayerCards
  | -- | on a location: its quest points are raised by this much, as an X reads
    QuestPointModifier Int
  | -- | on an enemy, or a character; on a player, none of their characters may attack,
    -- as Hobbit-sense reads
    CannotAttack
  | -- | on a character: it does not exhaust, so cannot commit, attack or defend
    CannotExhaust
  | CannotReady
  | -- | on an enemy: characters with ranged may attack it in the staging area
    AttackableInStagingAreaByRanged
  | -- | on an enemy: it cannot attack that player, though it may attack others
    CannotAttackPlayer PlayerId
  | -- | on a player: the enemies engaged with them cannot attack them, as Out of Sight reads
    EngagedEnemiesCannotAttack
  | CannotAttackOrDefend
  | -- | on an enemy: allies cannot defend while it attacks, as Snow Warg's
    CannotBeDefendedByAllies
  | CannotHaveAttachments
  | -- | no attachment with the restricted keyword can be attached to it
    CannotHaveRestrictedAttachments
  | -- | on the game: the total threat in the staging area is raised by this much
    StagingThreatBonus Int
  | -- | on the game: the archery total is raised by this much, as Pinned Down reads
    ArcheryBonus Int
  | -- | on the game: archery damage must be assigned to allies, if able, as An Untimely
    -- End reads
    ArcheryDamageToAllies
  | -- | on the game, or a character: characters use their willpower instead of defense,
    -- as Against the Shadow reads
    WillpowerForDefense
  | -- | on the game: characters use their willpower instead of attack when attacking
    -- enemies, as The Passage Out reads
    WillpowerForAttack
  | -- | on the game: attacks on enemies place progress on them instead of damage, as The
    -- Passage Out reads
    ProgressInsteadOfAttackDamage
  | -- | on a hero: resources from card effects are not placed in its pool, as Garden of
    -- Poisons reads
    CannotGainResourcesFromCardEffects
  | -- | on the game: reveal this many more encounter cards during staging
    AdditionalStagingReveals Int
  | -- | on the game: reveal fewer encounter cards, allowing staging to reveal none
    FewerStagingReveals Int
  | NoEngagementChecks
  | -- | on the game: enemies with ambush make no engagement check as they enter play
    IgnoreAmbush
  | SkipStagingStep
  | -- | on the game: the players, as a group, cannot play more than 1 ally each round
    OneAllyPerRound
  | AllyPlayedThisRound
  | -- | on the game: a copy of the card was played this round
    PlayedThisRound CardCode
  | -- | on an ally: it entered play this round
    EnteredPlayThisRound
  | -- | damage an attack deals past the character's remaining hit points raises threat
    ExcessDamageToThreat
  | -- | on an enemy: for each point of damage its attack deals past the character's remaining
    -- hit points, the player damages another character they control
    ExcessDamageToOtherCharacters
  | CannotBeOptionallyEngagedWhileOthersInStaging
  | -- | on an enemy, or on the game for every enemy: matching players cannot optionally
    -- engage it
    CannotBeOptionallyEngagedBy PlayerMatcher
  | -- | on an enemy in the staging area: it counts as engaged with each matching player while
    -- it stays there, as Durin's Bane, and is dealt a new shadow card for each attack
    EngagedWithEachPlayer PlayerMatcher
  | DoesNotCountDefense
  | -- | on a character: its attack counts as 0
    DoesNotCountAttack
  | -- | on an enemy: its attack is undefended, no character may defend against it
    CannotBeDefended
  | -- | on a location: at most this much more progress can be placed on it now
    ProgressLimit Int
  | -- | on a character committed to the quest: its willpower does not count toward it
    DoesNotCountWillpower
  | -- | on a character: it is discarded from play once its willpower is 0, as The
    -- Mountains' Peaks reads
    DiscardedAtNoWillpower
  | -- | on a location: when the players may travel and there is no active location, they
    -- must travel here
    MustBeTraveledTo
  | LeavesPlayToTopOfEncounterDeck
  | ShuffledIntoEncounterDeckWhenDefeated
  | ReturnToTopOfEncounterDeckAfterCombat
  | -- | on a location: the players cannot travel there now (its travel cost cannot be paid)
    CannotBeTraveledTo
  | -- | on a location: the players may travel there while 1 other location is active,
    -- making 2 active locations, as Ruined Tower reads
    TravelableWithActiveLocation
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
  | -- | on a player: they may declare any number of defenders against each attack
    MayDeclareMultipleDefenders
  | -- | on a player
    CannotCommitCharacters
  | GainKeyword Keyword
  | -- | the card loses a printed keyword, as The Last Battle loses siege
    LoseKeyword Keyword
  | -- | on a character: it defends against enemies with the trait without exhausting
    DoesNotExhaustToDefendAgainst Trait
  | CannotCommitToQuest
  | -- | on a card in play: its abilities cannot be used, nor its Forced effects trigger
    CannotTriggerAbilities
  | -- | on a card in play: its printed text box is treated as blank, as Arkham's: it has no
    -- abilities or keywords, and its constant effects give no modifiers
    Blank
  | -- | on a hero
    CannotCollectResources
  | -- | on a hero: no resources can be spent from its pool, as Orc Vanguard's
    CannotSpendResources
  | -- | on a hero: it collects this many more resources in the resource phase
    CollectsAdditionalResources Int
  | -- | on the game: resources of any sphere may be spent as this sphere's
    ResourcesSpendableAs Sphere
  | CannotBeHealed
  | -- | on an enemy: no character may attack it
    CannotBeAttacked
  | -- | on an enemy: it has dealt damage this round, as Rock Adder reads
    DealtDamageThisRound
  | -- | on a character or enemy: damage dealt to it is canceled
    CannotTakeDamage
  | -- | on an enemy: the damage of attacks on it is canceled
    CannotTakeCombatDamage
  | -- | on an enemy: characters attacking it with ranged, engaged with another player,
    -- add nothing to the attack
    ImmuneToRangedDamage
  | -- | on a character: no single effect heals it of more than this
    HealLimit Int
  | -- | on an enemy: at most this much more damage can be placed on it now, as Mumak's
    -- 3 a round
    DamageLimit Int
  | -- | on an objective ally in the staging area: it is committed to the current quest, as
    -- Celador reads
    CountsAsCommitted
  | -- | on an enemy: only characters with the trait or the keyword can attack it or defend
    -- against it
    OnlyFoughtBy Trait Keyword
  | -- | on an enemy: only characters with the keyword can attack it or defend against it,
    -- as Moria Bats reads for ranged
    OnlyFoughtByKeyword Keyword
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
  | -- | on an encounter card: its When Revealed effect cannot be canceled
    WhenRevealedCannotBeCanceled
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
  | -- | as 'ReduceNextCostOf', to no less than 0
    LowerNextCostOf CardMatcher Int
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
  | -- | on an ally, as its card id: it returns to its owner's hand once the players quest
    -- successfully this phase, as Bofur's
    ReturnToHandIfQuestedSuccessfully
  | -- | on a side quest: the players quest against it this phase, as the current quest
    CurrentQuest
  | -- | on the game: the next enemy to enter the staging area takes the modifier instead,
    -- for as long as the effect lasts
    NextEnemyInStaging ModifierType
  | -- | on an enemy: it cannot leave play unless destroyed; an effect that would discard
    -- it, return it to the encounter deck or remove it from the game does nothing
    CannotLeavePlay
  | -- | on a character or enemy: any amount of damage dealt to it is reduced to this much
    DamageReducedTo Int
  | -- | on the game: matching enemies engaged with players add their threat to the staging
    -- area too, as Conflict at the Crossroads reads
    EngagedEnemiesAddThreat EnemyMatcher
  | -- | on a player: matching cards in their discard pile may be played as if from hand,
    -- by discarding the modifier's source, then go to the bottom of their deck, as a
    -- Record's Action reads
    MayPlayFromDiscard CardMatcher
  | -- | on an enemy: it cannot engage the player, by engagement check or choice, as
    -- Pippin's
    CannotEngagePlayer PlayerId
  | -- | on a player: their undefended attacks' damage may go to this character instead of
    -- a hero, as White Tower Watchman's
    UndefendedDamageMayGoTo Target
  | -- | on the game: the next encounter card revealed, if of this type, is discarded
    -- without resolving, as Minas Tirith Lampwright's
    DiscardNextRevealed CardType
  | -- | on a card: no time counter can be removed from it, as Tharbad Hideout reads for
    -- the current quest
    CannotRemoveTimeCounters
  | -- | on a player, or on the game for every player: the next card played from hand
    -- gains Doomed N, as Gríma's Action reads; spent with it
    NextCardGainsDoomed Int
  | -- | on a card in hand, as its card id: its player may give it Doomed N as they play
    -- it from hand, as Greyflood Wanderer reads; it then gains 'GainKeyword' of it
    MayGainDoomed Int
  | -- | on an enemy or location: it is considered out of play, while the card giving it this
    -- is in play, as Saruman's chosen card; no matcher finds it
    ConsideredOutOfPlay
  | -- | on an ally, or its card: it returns to its owner's hand at the end of the round, as
    -- the Eagle Gwaihir puts into play does
    ReturnToHandAtEndOfRound
  | -- | on a card in hand or in play, as its card id: at the end of the round it is shuffled
    -- into its owner's deck, as the card Message from Elrond passes is
    ShuffleIntoOwnersDeckAtEndOfRound
  | -- | on a hero: a player spent resources from its pool this round
    SpentResourcesThisRound
  | -- | on an enemy: defeated, it stays where it is for its own Forced effect to place it,
    -- as Mugash is attached to a hero
    StaysInPlayWhenDefeated
  | -- | on a player: they refresh no more than this many characters in the refresh phase,
    -- as Heart of Fangorn reads
    RefreshAtMost Int
  | -- | on a player: they draw this many cards in the resource phase instead of 1, as
    -- Hithaeglir Stream reads (0: they draw none, as Plains of Enedwaith)
    ResourcePhaseDraws Int
  | -- | on a player: their threat cannot be reduced, as The Empty Mug reads
    CannotReduceThreat
  | -- | on a player: the threat at which they are eliminated, 50, moves by this much, as
    -- Double Dealings lowers it by 10
    EliminationThreatModifier Int
  | -- | on an enemy: the damage its attack deals raises the player's threat as much, as
    -- Seedy Inn's shadow reads
    AttackDamageRaisesThreatOf PlayerId
  | -- | on an enemy: the damage its attack deals past the character's remaining hit points
    -- is placed on the active location, as Collapsed Tower's shadow reads
    ExcessDamageToActiveLocation
  | -- | on a player: before he commits his first character to the quest he discards a
    -- card at random from his hand, and with none he cannot commit, as A Weary Passage reads
    DiscardRandomToCommit
  | -- | on a player: card effects cannot make him draw cards, as No End in Sight reads; the
    -- draw of the resource phase is not one
    CannotDrawCardsFromCardEffects
  | -- | on a location in the staging area: progress is placed on it before it can be placed
    -- on the active location, as Grim Foothills reads
    TakesProgressBeforeActiveLocation
  | -- | on the game: progress questing successfully would place on the quest is dealt as
    -- damage to the enemy instead, as Ride Them Down reads
    ProgressAsDamageTo Target
  | -- | on a character: it was dealt damage while defending the attack being resolved
    DamagedWhileDefending
  | -- | on a player: they draw this many more cards in the resource phase, in the same
    -- draw, as Bilbo Baggins reads
    AdditionalResourcePhaseDraws Int
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Modifier = Modifier
  { modifierSource :: Source
  , modifierType :: ModifierType
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)
