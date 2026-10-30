module LordOfTheRings.Window (module LordOfTheRings.Window, module X) where

import LordOfTheRings.Card (Card)
import LordOfTheRings.Card.Code (CardCode)
import LordOfTheRings.Id
import LordOfTheRings.Phase
import LordOfTheRings.Source
import LordOfTheRings.Prelude
import LordOfTheRings.Target
import LordOfTheRings.Trait
import LordOfTheRings.Timing as X

data Window = Window {windowTiming :: Timing, windowType :: WindowType}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data WindowType
  = EntersPlay Target
  | PhaseBegins Phase
  | CommittedToQuest Target
  | EnemyEngaged EnemyId PlayerId
  | -- | the player engaged the enemy in the optional engagement step
    OptionallyEngaged EnemyId PlayerId
  | -- | an enemy or location entered the staging area
    EntersStagingArea Target
  | -- | a card effect placed resources in the hero's pool
    GainedResources Target
  | DealtDamage Target Int
  | Traveled LocationId
  | -- | a card exhausted, as an objective does
    Exhausted Target
  | DeclaredAsDefender Target EnemyId
  | DeclaredAsAttacker Target EnemyId
  | -- | characters attack the enemy, their attack about to deal its damage
    EnemyAttacked EnemyId
  | -- | an attack destroyed the enemy: the characters that attacked, and its traits as it left
    EnemyDestroyedByAttack EnemyId [Target] (Set Trait)
  | -- | an attack destroyed an enemy engaged with the player: the characters that attacked
    EngagedEnemyDestroyedByAttack PlayerId [Target]
  | CharacterDestroyed Target Card
  | -- | an encounter card revealed from the encounter deck
    Revealed Target
  | -- | a location about to leave play as an explored location
    Explored LocationId
  | EnemyAttacking EnemyId PlayerId
  | PhaseEnds Phase
  | RoundEnds
  | -- | the staging step begins, before its cards are revealed
    StagingStepBegins
  | -- | the staging step ends, its cards revealed
    StagingStepEnds
  | ProgressPlaced Target Int
  | QuestedUnsuccessfully
  | QuestedSuccessfully
  | -- | an attack the character made has resolved
    AttackedEnemy Target EnemyId
  | -- | an attack the character defended has resolved
    DefendedAttack Target EnemyId
  | -- | a card played from its owner's hand, as its card id
    PlayedFromHand Target
  | ShadowEffectResolved CardId EnemyId
  | -- | a shadow card dealt to an enemy: the enemy, the card, and the player it is engaged with
    ShadowCardDealt EnemyId CardId PlayerId
  | -- | a shadow card's effect about to resolve: the card, and the enemy it was dealt to
    ShadowEffectTriggered CardId EnemyId
  | -- | a shadow effect about to resolve during an attack the character defends
    ShadowEffectAgainst Target
  | -- | a character left play, as its card
    CharacterLeftPlay Card
  | -- | a character left play: who controlled it, and its traits as it left
    LeftPlay Target PlayerId (Set Trait)
  | -- | a character attacked an enemy alone: the damage the attack dealt beyond what the
    -- enemy had left
    AttackedAlone Target EnemyId Int
  | -- | an encounter or quest card's effect raised the player's threat, by this much
    ThreatRaisedByEncounter PlayerId Int
  | -- | a character readied during the refresh phase, with its traits
    Readied Target (Set Trait)
  | -- | damage was healed from a character, by the source
    Healed Target Source
  | -- | an ally entered play: who controls it, and its traits
    AllyEnteredPlay Target PlayerId (Set Trait)
  | -- | a card discarded from the top of the player's deck, as its card id
    DiscardedFromDeck PlayerId CardId
  | -- | the player played a card from their hand, as its code
    PlayedCardFromHand PlayerId CardCode
  | -- | a player's threat was raised, by anything
    ThreatRaised PlayerId
  | -- | an ally changed control
    ChangedControl Target
  | -- | the player is playing the event, its effects yet to resolve
    PlayingEvent PlayerId CardId
  | -- | an attack the characters made dealt the enemy this much damage
    AttackDealtDamage EnemyId [Target] Int
  | -- | the player let the enemy's attack go undefended
    AttackUndefended PlayerId EnemyId
  | -- | an enemy was defeated: damage destroyed it
    EnemyDefeated EnemyId
  | -- | the enemy's attack's damage destroyed the character, after it is destroyed: who
    -- controlled it. Not its shadow card's damage, nor its leaving play another way.
    CharacterDestroyedByAttack EnemyId Target PlayerId
  | -- | this many time counters were removed from the card
    TimeCountersRemoved Target Int
  | -- | the last time counter was removed from the card
    LastTimeCounterRemoved Target
  | -- | the player raised his threat from the Doomed keyword
    ThreatRaisedByDoomed PlayerId
  | -- | the player drew this many cards, by one draw
    CardsDrawn PlayerId Int
  | -- | a card effect reduced the player's threat: the player, and the card's source
    ThreatReduced PlayerId Source
  | -- | the players advanced to a quest stage, its When Revealed resolved
    AdvancedToStage
  | -- | an attack the character defended has resolved, and it took no damage defending it
    DefendedWithoutDamage Target EnemyId
  | -- | the player played a card from anywhere, hand, discard pile or deck, as its code
    PlayedCard PlayerId CardCode
  | -- | the player is about to be eliminated by reaching his threat elimination level
    EliminatedByThreat PlayerId
  | -- | a quest card was defeated, before what is attached to it is discarded
    QuestDefeated Target
  | -- | a card effect discarded the ally from play: its traits as it left
    AllyDiscardedByCardEffect Target (Set Trait)
  | -- | an enemy destroyed, as its card, which has left play by now
    EnemyDestroyed Card
  | -- | a card discarded from the player's hand, as its card id
    DiscardedFromHand PlayerId CardId
  | -- | an exhausted card readied, by anything, in any phase
    BecameReady Target
  | -- | it became Day or Night, as The Wastes of Eriador's Time objective flipped
    Became TimeOfDay
  | -- | the When Revealed effects of the card just revealed were canceled
    WhenRevealedCanceled Target
  | -- | the captured cards underneath the card were rescued, as Jailor Gornákh answers
    -- (Escape from Mount Gram)
    CapturedCardsRescued Card [Card]
  | -- | the shadow card's effect raised its enemy's attack
    ShadowIncreasedAttack CardId EnemyId
  | -- | the card was paid for from these heroes' and allies' pools, as Master Ironsmith reads
    PaidFrom Target [Target]
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Whether it is Day or Night, as The Wastes of Eriador's Time objective shows.
data TimeOfDay = Daytime | Nighttime
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkWhen :: WindowType -> Window
mkWhen = Window When

mkAfter :: WindowType -> Window
mkAfter = Window After
