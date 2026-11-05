{-# LANGUAGE TemplateHaskell #-}

module LordOfTheRings.Window (module LordOfTheRings.Window, module X) where

import Data.Aeson.TH (deriveJSON)
import LordOfTheRings.Card (Card, EncounterCard, PlayerCard)
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
  | -- | the character destroyed was under the player's control, opened with
    -- 'CharacterDestroyed'
    CharacterDestroyedUnder PlayerId Target
  | -- | an encounter card revealed from the encounter deck
    Revealed Target
  | -- | a location about to leave play as an explored location
    Explored LocationId
  | -- | the card of the location just explored, opened with 'Explored' after it left play,
    -- for what is read of it then, as its printed quest points
    ExploredCard Card
  | -- | a location has left play, however it did
    LocationLeftPlay LocationId
  | -- | an enemy entered play, however it did: revealed, put into play or turned faceup
    EnemyEnteredPlay EnemyId
  | -- | an enemy left play, however it did: defeated, discarded, placed under a card or
    -- shuffled away, as "if there are no Nazgûl enemies in play" is read after it
    EnemyLeftPlay EnemyId
  | -- | the player looked at one or more cards in the encounter deck
    LookedAtEncounterDeck PlayerId
  | -- | the player turned 1 or more hidden cards faceup, at once (The Blood of Gondor)
    TurnedHiddenCardsFaceup PlayerId
  | EnemyAttacking EnemyId PlayerId
  | -- | the enemy is declared as an attacker against the player, its attack begun, as
    -- Flight to the Ford's campaign card answers
    EnemyDeclaredAsAttacker EnemyId PlayerId
  | PhaseEnds Phase
  | RoundEnds
  | -- | the round begins, before its resource phase, as Well Preserved reads
    RoundBegins
  | -- | the staging step begins, before its cards are revealed
    StagingStepBegins
  | -- | the staging step ends, its cards revealed
    StagingStepEnds
  | ProgressPlaced Target Int
  | -- | progress was placed on the current quest (see 'getCurrentQuest'), as well as the
    -- 'ProgressPlaced' window for its card
    ProgressPlacedOnCurrentQuest Int
  | QuestedUnsuccessfully
  | QuestedSuccessfully
  | -- | the progress from questing successfully has been placed
    PlacedProgressFromQuesting
  | -- | the quest has been resolved, whatever its result, as "after resolving the quest"
    -- reads
    QuestResolved
  | -- | an attack the character made has resolved
    AttackedEnemy Target EnemyId
  | -- | an attack the character defended has resolved
    DefendedAttack Target EnemyId
  | -- | a card played from its owner's hand, as its card id
    PlayedFromHand Target
  | ShadowEffectResolved CardId EnemyId
  | -- | a shadow card dealt to an enemy: the enemy, the card, and the player it is engaged with
    ShadowCardDealt EnemyId CardId PlayerId
  | -- | a shadow card turned faceup as the attack's shadow effects resolve: the enemy, the
    -- card, the defending player and the defending characters. "When ... is dealt a shadow card with ..." is read
    -- here (Nate French, ALeP 3.003; Caleb, ALeP 3.274 #3)
    ShadowCardRevealed EnemyId CardId PlayerId [Target]
  | -- | a shadow card's effect about to resolve: the card, and the enemy it was dealt to
    ShadowEffectTriggered CardId EnemyId
  | -- | a shadow card's effect about to resolve during an attack the character defends:
    -- the card, and the character
    ShadowEffectAgainst CardId Target
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
  | -- | the enemy's attack dealt the character this much damage past what destroys it
    AttackDealtExcessDamage EnemyId Target Int
  | -- | the enemy's attack dealt the card damage, as Dwarven Shield answers
    DamagedByEnemyAttack Target EnemyId
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
  | -- | the player drew cards by the source's effect, alongside 'CardsDrawn'
    CardsDrawnBy PlayerId Source
  | -- | a card effect reduced the player's threat: the player, and the card's source
    ThreatReduced PlayerId Source
  | -- | the players advanced to a quest stage, its When Revealed resolved
    AdvancedToStage
  | -- | an attack the character defended has resolved, and it took no damage defending it
    DefendedWithoutDamage Target EnemyId
  | -- | the player played a card from anywhere, hand, discard pile or deck, as its code
    PlayedCard PlayerId PlayerCard
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
  | -- | the card turned over to the side with this code, as Beneath the Sands' Search
    -- objective flips to On Track
    FlippedTo Target CardCode
  | -- | the When Revealed effects of the card just revealed were canceled
    WhenRevealedCanceled PlayerId Target
  | -- | the captured cards underneath the card were rescued, as Jailor Gornákh answers
    -- (Escape from Mount Gram)
    CapturedCardsRescued Card [Card]
  | -- | the player drew these cards at the beginning of the resource phase, as Círdan the
    -- Shipwright reads
    DrewForResourcePhase PlayerId [CardId]
  | -- | the player drew their setup hand, before deciding whether to mulligan
    SetupHandDrawn PlayerId
  | -- | the player committed these characters to a Sailing test (Dream-chaser rules)
    CommittedToSailingTest PlayerId [Target]
  | -- | a location was turned over to its other side, as an Uncharted location is
    LocationFlipped LocationId
  | -- | a location is discarded by the Mire keyword, having as many mire tokens as its X
    DiscardedByMire LocationId
  | -- | a location became the active location, however it did
    BecameActiveLocation LocationId
  | -- | the Discover keyword of the location resolved: whether it revealed a Hazard card
    DiscoverResolved LocationId Bool
  | -- | the source placed progress on the location, read once it is placed
    ProgressPlacedOnLocationBy Source LocationId
  | -- | the shadow card's effect raised its enemy's attack
    ShadowIncreasedAttack CardId EnemyId
  | -- | the player spent this many resources from the pools of characters he controls, as
    -- one payment, however many pools it was paid from
    ResourcesSpent PlayerId Int
  | -- | the card was paid for from these heroes' and allies' pools, as Master Ironsmith reads
    PaidFrom Target [Target]
  | -- | the attachment was attached to the host, as it entered play or moved: its traits
    Attached Target Target (Set Trait)
  | -- | the committed character was removed from the quest
    RemovedFromQuest Target
  | -- | the player discarded cards from his out-of-play deck (To Catch an Orc)
    DiscardedFromOutOfPlayDeck PlayerId
  | -- | the players bypassed the quest card, as Flight from Moria's stages let them
    QuestBypassed Target
  | -- | the character was committed to an escape test, as Things in the Pools answers
    CommittedToEscapeTest Target
  | -- | the player failed a Hide test (The Black Riders rules insert, "Hide X")
    FailedHideTest PlayerId
  | -- | the player made a successful Hide test
    PassedHideTest PlayerId
  | -- | the cards were discarded from the encounter deck for the player's Fortitude test, as
    -- Gollum answers
    DiscardedForFortitudeTest PlayerId [EncounterCard]
  | -- | the player failed a Fortitude test
    FailedFortitudeTest PlayerId
  | -- | the guarded character went free of encounters, back under its owner's control
    CharacterRescued Target
  | -- | the hidden card was turned faceup: the enemy it put into play (The Blood of Gondor)
    HiddenCardTurnedFaceup Target
  | -- | the attackers against the enemy are declared, before the attack resolves
    AttackersDeclared EnemyId
  | -- | a card was discarded from the encounter deck, as Calaerion answers
    DiscardedFromEncounterDeck CardId
  | -- | the active location was explored, read as it leaves play, alongside 'Explored'
    ActiveLocationExplored LocationId
  | -- | the card of the enemy an attack destroyed, alongside 'EnemyDestroyedByAttack'
    EnemyCardDestroyedByAttack EnemyId CardId
  deriving stock (Show, Eq, Generic)

-- | Whether it is Day or Night, as The Wastes of Eriador's Time objective shows.
data TimeOfDay = Daytime | Nighttime
  deriving stock (Show, Eq, Generic)

mkWhen :: WindowType -> Window
mkWhen = Window When

mkAfter :: WindowType -> Window
mkAfter = Window After

-- Direct codecs avoid specializing Generic traversal for this large sum.
$(concat <$> traverse (deriveJSON defaultOptions) [''Window, ''WindowType, ''TimeOfDay])
