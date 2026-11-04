module LordOfTheRings.Message (module LordOfTheRings.Message) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.Campaign (CampaignLogUpdate)
import LordOfTheRings.Card
import LordOfTheRings.Effect
import LordOfTheRings.Heading (HeadingShift)
import LordOfTheRings.I18n (ikeyWith, intVar)
import LordOfTheRings.Id
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Location.Grid (Pos)
import LordOfTheRings.Log.Block
import LordOfTheRings.Matcher.Types (AllyMatcher, CardMatcher, HeroMatcher)
import LordOfTheRings.Modifier (ModifierType)
import LordOfTheRings.Outcome
import LordOfTheRings.Phase
import LordOfTheRings.Placement
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Trait (Trait)
import LordOfTheRings.Window

data DeckSignifier = EncounterDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CardHandler = ToStaging
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)
data DeckHandler = Shuffle
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data Drawing = Drawing
  { player :: PlayerId
  , count :: Int
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | What characters count toward an escape test.
data EscapeStat
  = EscapeWillpower
  | EscapeAttack
  | -- | as Swarming Mosquitoes' test counts defense
    EscapeDefense
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

{- | An escape test: the player commits characters by exhausting them, then cards are
dealt from the encounter deck, and the test fails if their escape values come to more
than the committed characters' total.
-}
data EscapeTest = EscapeTest
  { escapePlayer :: PlayerId
  , escapeSource :: Source
  , escapeCards :: Int
  , escapeStat :: EscapeStat
  , escapeCommitted :: [Target]
  , escapeOnFail :: [Message]
  , escapeOnPass :: [Message]
  , escapeParty :: [PlayerId]
  -- ^ for a test the party makes: the players still to commit after this one
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

instance FromJSON EscapeTest where
  parseJSON = genericParseJSON defaultOptions . withDefaults [("escapeParty", toJSON ([] :: [PlayerId]))]

data Message
  = Setup
  | BeginPhase Phase
  | DoPhase Phase
  | EndPhase Phase
  | -- | the round goes on after the phase: an additional instance of a phase, if one is to
    -- come, or else the round's next phase
    NextPhase Phase
  | -- | after the current phase there is an additional instance of each phase, in order
    AdditionalPhases [Phase]
  | BeginRound
  | RaiseThreat PlayerId Int
  | -- | the threat raise itself, once the window before it has closed
    DoRaiseThreat PlayerId Int
  | CheckEliminations
  | EliminatePlayer PlayerId
  | GameOver Outcome
  | -- | the players concede: the game ends lost, whatever would stop it ending
    Surrender
  | {- | the scenario has been won in campaign mode: its campaign card's resolution, then
    what every campaign records, the score and the fallen heroes, written to the log the
    scenario holds. The game is over, so nothing pushed here runs.
    -}
    ResolveCampaign
  | -- | in campaign mode, a card writes to the campaign log the scenario holds
    UpdateCampaignLog CampaignLogUpdate
  | Discard Target
  | {- | a character leaves play now, the When window for its leaving having closed: the
    Discard, ReturnToHand or ShuffleIntoOwnersDeck that takes it
    -}
    LeavePlay Message
  | -- | the window for the ally's leaving play has closed: its abilities answer no more
    ForgetLeftPlay AllyId
  | DealDamage Source Target Int
  | HealDamage Source Target Int
  | {- | the player looks at the top card of the encounter deck, and may move it to the
    bottom when the flag says so
    -}
    LookAtTopOfEncounterDeck PlayerId Source Bool
  | MoveTopOfEncounterDeckToBottom
  | -- | the player looks at a shadow card dealt to the enemy
    LookAtShadowCard PlayerId Source EnemyId CardId
  | DiscardFromHand PlayerId CardId
  | ChooseAndDiscardFromHand PlayerId Source Int
  | {- | each player in turn may discard any number of cards from hand, one at a time, with
    the cards discarded so far; the source reads them back once no player is left to ask
    -}
    EachDiscardAnyFromHand Source [PlayerId] [Card]
  | -- | whether the damage the source just dealt has destroyed the card
    CheckDefeated Source Target
  | -- | the card is destroyed by the source: an attack's, for one its damage destroys
    Destroy Source Target
  | PlaceEnemy EnemyId Placement
  | PlaceLocation LocationId Placement
  | SetFirstPlayer PlayerId
  | EndRound
  | WithTarget Target Message
  | PlayCard PlayerId CardId
  | -- | play a card in response to the windows, which its effect may read
    PlayCardInWindow PlayerId CardId [Window]
  | -- | pay for a card being played, from its owner's hand when the flag says so
    PayForCard PlayerId Card Bool [Window]
  | -- | pay for an attachment chosen to go on the host, whose cost the host may lower
    PayToAttach PlayerId Card Bool [Window] Target
  | PlayEvent PlayerId PlayerCard [Window]
  | PutAllyIntoPlay PlayerId CardId
  | -- | the player uses the action of a card in their hand, as Bofur's Quest Action
    UseHandAction PlayerId CardId
  | ReturnToHand Target
  | -- | a card goes from its owner's discard pile to their hand
    ReturnFromDiscardToHand PlayerId CardId
  | -- | an ally leaves play and is shuffled into its owner's deck
    ShuffleIntoOwnersDeck Target
  | -- | an ally leaves play for the bottom of its owner's deck, as Curious Brandybuck
    PutOnBottomOfOwnersDeck Target
  | TakeControl AllyId PlayerId
  | RemoveFromDiscard PlayerId CardId
  | -- | the player plays an attachment of their choice from their discard pile on the host,
    -- paying for it, as Stalwart lets them
    PlayAttachmentFromDiscardOn PlayerId Source Target
  | -- | an ally card put into play under the player's control, keeping its owner
    PutAllyIntoPlayUnder PlayerId PlayerCard
  | PutHeroIntoPlay PlayerId PlayerCard
  | -- | an attachment card, already taken from where it was, put into play by the player
    PutAttachmentIntoPlay PlayerId PlayerCard
  | -- | a player side quest, played, goes to the staging area
    PutSideQuestIntoPlay PlayerId PlayerCard
  | ShuffleDiscardIntoDeck PlayerId
  | -- | a card from the player's deck goes to their hand
    TakeFromDeck PlayerId CardId
  | -- | debug mode: the player looks through their deck and takes any card into hand
    DebugSearchDeck PlayerId
  | -- | debug mode: the player looks through the encounter deck and reveals any card
    DebugSearchEncounterDeck PlayerId
  | LowerThreat PlayerId Int
  | ActionWindow PlayerId
  | -- | an action window within an attack, asked only when the player has an action to take
    OptionalActionWindow PlayerId
  | PayCost CostId
  | PaidCost CostId
  | SpendResources PlayerId Sphere Int
  | -- | the card is played without paying its cost, from wherever it was taken
    PlayAtNoCost PlayerId PlayerCard
  | -- | the card is played from its owner's hand without paying its cost
    PlayFromHandAtNoCost PlayerId CardId
  | {- | the top card of the player's deck is revealed: they may play it at no cost, if
    able, or else it goes to the bottom of their deck
    -}
    PlayTopOfDeckAtNoCostOrBottom PlayerId Source
  | RemoveFromDeck PlayerId CardId
  | MoveToBottomOfDeck PlayerId CardId
  | -- | a card in the player's deck goes to the top of it
    MoveToTopOfDeck PlayerId CardId
  | PlayAlly PlayerId PlayerCard
  | DoStep Int Message
  | Ask PlayerId (Question Message)
  | Flip Target
  | -- | the lead clicks the first stage: its setup resolves on side A, then it turns over
    PerformSetup Target
  | Search PlayerId DeckSignifier CardMatcher CardHandler DeckHandler
  | AddToStaging EncounterCard
  | RevealEncounterCard
  | RevealCard EncounterCard
  | -- | the card is revealed, and an enemy put into play engaged with the player, as
    -- Murzag reads; any other card resolves as 'RevealCard'
    RevealCardEngaged PlayerId EncounterCard
  | WhenRevealed Target
  | -- | the lead clicks the revealed card to resolve its When Revealed effect
    AskToResolveWhenRevealed Target EncounterCard
  | FinishRevelation TreacheryId
  | -- | the revealed enemy or location surges if it has surge now, printed or gained
    ResolveSurge Target EncounterCard
  | {- | only the card's When Revealed effect resolves, as Lieutenant of Mordor's: it is
    not revealed, so no keyword, window or cancel touches it
    -}
    ResolveWhenRevealedOf EncounterCard
  | PutQuestIntoPlay EncounterCard
  | CompleteQuest QuestId
  | -- | the quest card becomes the current quest until the end of the phase: a side quest in
    -- the staging area, or the main quest, putting aside any side quest chosen before
    SetCurrentQuest Target
  | AdvanceQuest
  | -- | a quest card, in play or in the victory display, is shuffled into the quest deck
    ShuffleIntoQuestDeck EncounterCard
  | -- | a quest card, in play, in the victory display or in the quest deck, goes to the
    -- bottom of the quest deck
    PutOnBottomOfQuestDeck EncounterCard
  | AdvanceToQuest EncounterCard
  | RemoveFromEncounterDeck EncounterCard
  | RemoveFromEncounterDiscard EncounterCard
  | {- | the player searches the encounter deck and discard pile for a matching card to
    add to the staging area, then the deck is shuffled
    -}
    SearchEncounterDeckAndDiscard PlayerId Target CardMatcher
  | ShuffleDeck DeckSignifier
  | CheckWindows [Window]
  | ResponseWindow [Window] [PlayerId] [(Source, Int)] Int
  | UseAbility PlayerId Ability [Window]
  | UseCardAbility PlayerId Source Int [Window]
  | CreateEffect Effect
  | -- | the lasting effects of that duration end, as the round's do once the round's end
    -- has resolved
    EndEffects EffectDuration
  | AttachCard AttachmentId Target
  | -- | the character's restricted cards are counted again, after one is discarded
    CheckRestricted Target
  | PlayAttachment PlayerId PlayerCard
  | PlayAttachmentOn PlayerId PlayerCard Target
  | -- | an attachment in the player's hand is put into play on the host, not played
    PutAttachmentIntoPlayFromHand PlayerId CardId Target
  | {- | the hero leaves the game for one from its owner's collection, which takes the
    attachments it can
    -}
    SwapHero HeroId PlayerCard
  | -- | the ally loses the ally card type and gains the hero card type, as Sword-thain
    -- gives it, keeping its tokens, state and what is attached to it
    AllyBecomesHero AllyId
  | -- | the hero an ally became goes back to being an ally
    HeroBecomesAlly HeroId
  | ChooseAndExhaust Source PlayerId Int
  | ChooseAndExhaustHero Source PlayerId
  | -- | the player exhausts a hero the matcher picks out to pay a cost; the ability then
    -- used is told which, by the hero's exhausted window
    PayExhaustHeroCost PlayerId HeroMatcher [Message]
  | -- | the player deals 1 damage to a character they control other than the one given
    DamageAnotherCharacter Source PlayerId Target
  | -- | the player deals 1 damage to an Aflame location in the staging area of their
    -- choice, chosen as it resolves, as Elite Marauder's excess damage reads point by point
    DamageAflameLocation Source PlayerId
  | -- | the windows for an ally having entered play, under whoever controls it as this
    -- resolves: one revealed from the encounter deck, given a player as it resolves
    AllyEnteredPlayUnderController AllyId
  | -- | the enemy's attack is over without it having attacked, as a canceled attack: what
    -- lasted for that attack ends, and no "after it attacks" window opens
    AttackEndedUnresolved EnemyId
  | -- | the player divides the damage among characters they control, a point at a time,
    -- the points assigned so far kept; it is dealt once all are assigned
    DealDamageDivided Source PlayerId Int [Target]
  | -- | as 'DealDamageDivided', among the characters given: the player divides the damage,
    -- the points assigned so far kept
    DealDamageDividedAmong Source PlayerId [Target] Int [Target]
  | DiscardRandomFromHand PlayerId Int
  | RemoveFromQuest Target
  | AttachTreachery TreacheryId Target
  | ClaimObjective ObjectiveId Target
  | PlaceObjective ObjectiveId Placement
  | SetObjectiveGuard ObjectiveId CardId
  | {- | the next encounter card guards the objective: an enemy or location goes to the
    staging area to guard it, any other card is revealed
    -}
    GuardObjective ObjectiveId
  | -- | the first player claims the objective, which nothing guards (see
    -- 'FirstPlayerClaimsWhenFree')
    ClaimFreeObjective ObjectiveId
  | -- | a guarded location revealed, as Across the Ettenmoors' objective-locations: the next
    -- encounter card is revealed guarding it, the location attached to that card until it
    -- leaves play
    GuardLocation LocationId
  | PutObjectiveIntoPlay EncounterCard
  | -- | an objective card comes into play attached to the hero, as a setup attaches it
    PutObjectiveIntoPlayAttached EncounterCard Target
  | -- | an objective card comes into the staging area guarded by the card, as The Enemy's
    -- Trail attaches a Clue card to a Mordor enemy
    PutObjectiveIntoPlayGuardedBy EncounterCard CardId
  | -- | Guarded (X) on a player attachment as it enters play: cards are discarded from the
    -- encounter deck until one of the types is, which goes to the staging area guarding it
    -- (MEC65 rules)
    ResolveGuardedPlayerCard PlayerId PlayerCard [CardType]
  | -- | the encounter card, by its card id, guards the player card, out of play, until it
    -- leaves play
    GuardPlayerCard CardId GuardRelease PlayerCard
  | -- | the encounter card has left play: the player cards it guarded go free
    ReleaseGuardedPlayerCards CardId
  | -- | a Guarded (X) attachment free of encounters: its owner attaches it to an eligible
    -- card, its guarded keyword not resolved again
    AttachFreedCard PlayerId PlayerCard
  | -- | the Deep keyword, after the players travel to the location (Ered Mithrin)
    ResolveDeep LocationId
  | -- | the location is discarded and the top card of the Caves deck takes its place
    ReplaceWithTopOfCavesDeck LocationId
  | -- | the card, taken from where it was, is shuffled into the Caves deck
    ShuffleIntoCavesDeck EncounterCard
  | -- | the card goes to the bottom of the Caves deck, from it or from where it was
    PutOnBottomOfCavesDeck EncounterCard
  | -- | the card leaves the Caves deck, as a search takes it
    TakeFromCavesDeck EncounterCard
  | ShuffleCavesDeck
  | -- | the top card of the Evil Creatures deck enters play for the source: added to the
    -- staging area, or put into play engaged with the player (Journey Up the Anduin)
    AddFromEvilCreaturesDeck Source (Maybe PlayerId)
  | -- | the card the source took from the Evil Creatures deck, now in play, for it to act on
    AddedFromEvilCreaturesDeck Source EncounterCard
  | -- | an encounter card in the player's hand is discarded, to the encounter discard pile,
    -- by an encounter card's effect, as Lost in the Wild's
    DiscardEncounterCardFromHand PlayerId CardId
  | -- | the card leaves play for good; an objective, as Book of Mazarbul, or a location
    RemoveFromGame Target
  | -- | a stage leaves the quest deck unplayed, as exploring The Banks removes stage 2
    RemoveFromQuestDeck EncounterCard
  | TakePrisoner HeroId
  | RescuePrisoner HeroId
  | -- | the character, a hero or an ally, goes to the encounter card with this id as its
    -- captive: under no player's control and out of the game's reach, a guarded objective
    -- until that card leaves play (The Long Arm of Mordor, The Dungeons of Cirith Gurat)
    GuardCharacter Target CardId
  | -- | the guarded character is free of encounters: its owner takes control of it, exhausted
    RescueCharacter Target
  | -- | the hero goes to the staging area, in play under no player's control (The Long Arm
    -- of Mordor)
    MoveHeroToStagingArea HeroId
  | -- | the hero in the staging area returns to its owner's control, exhausted
    TakeControlOfHero HeroId
  | -- | an objective-hero comes into play under the player's control
    PutObjectiveHeroIntoPlayUnder PlayerId EncounterCard
  | -- | a card out of play, an ally card from its owner's discard pile or an objective ally,
    -- comes into play as the captive of the encounter card with this id (see 'GuardCharacter')
    GuardCard Card CardId
  | SetAside EncounterCard
  | -- | a stage whose condition may have just been met checks whether it is defeated
    CheckStage QuestId
  | {- | the player's top card goes face down in front of them as an Orc Guard engaged
    with them
    -}
    PlaceOrcGuard PlayerId
  | EngageOrcGuard PlayerId PlayerCard
  | {- | the player card leaves its owner's deck, hand or discard pile for the staging
    area, face down as a Reanimated Dead enemy with this id (The Dread Realm)
    -}
    Reanimate CardId EnemyId
  | ShuffleIntoEncounterDeck Target
  | {- | a treachery dealt as a shadow card resolves its revelation as if revealed, here
    attaching itself where its placement says
    -}
    PutShadowTreacheryIntoPlay EnemyId CardId Placement
  | -- | the treachery leaves the encounter discard pile attached to the target, its When
    -- Revealed effect not resolved, as Broken Rudder's *Forced effect attaches it
    AttachFromEncounterDiscard EncounterCard Target
  | ShufflePlayerDeck PlayerId
  | {- | the player takes matching cards from those searched in their deck into hand, any
    number of them or only 1 as the flag says, then shuffles their deck
    -}
    ChooseFromSearch PlayerId Source [Card] CardMatcher Bool
  | {- | the player puts up to N matching allies from those searched in their deck into
    play, each with the modifiers until the end of the round, then shuffles their deck
    -}
    ChooseFromSearchIntoPlay PlayerId Source [Card] CardMatcher Int [ModifierType]
  | DiscardTopOfDeck PlayerId Int
  | -- | an enemy card put into play engaged with the player
    PutEnemyIntoPlayEngaged PlayerId EncounterCard
  | -- | a card in the player's discard pile goes to the bottom of their deck
    PutDiscardOnBottomOfDeck PlayerId CardId
  | -- | a card in the player's discard pile goes to the top of their deck
    PutDiscardOnTopOfDeck PlayerId CardId
  | -- | a card in the player's discard pile is shuffled into their deck
    ShuffleFromDiscardIntoDeck PlayerId CardId
  | -- | the top cards of the player's discard pile are shuffled into their deck, as Norbury
    -- Tombs reads: an encounter card's effect, which nothing barring player cards stops
    ShuffleTopOfDiscardIntoDeck PlayerId Int
  | {- | a card in its owner's discard pile responds to the windows, as Hidden Cache does:
    the card is built again to resolve it
    -}
    RespondFromDiscard PlayerId PlayerCard [Window]
  | -- | after travel, an action window for each player with a Travel Action to use
    TravelActions
  | {- | the player looks at the top cards of the encounter deck, reveals 1 of them and
    discards the rest, or moves them to the bottom of the deck when the flag says so
    -}
    RevealOneOfTopEncounterCards PlayerId Source Int Bool
  | {- | the top cards of the encounter deck are discarded, for the source to read back
    on behalf of the target, as Arkham's
    -}
    DiscardTopOfEncounterDeck Source Target Int
  | -- | the cards just discarded from the top of the encounter deck
    DiscardedTopOfEncounterDeck Source Target [EncounterCard]
  | -- | an encounter card, taken from where it was, becomes the active location
    PutIntoPlayAsActiveLocation EncounterCard
  | -- | a location card, taken from where it was, comes into play placed there, as Street
    -- Fighting puts one under the first player's control
    PutLocationIntoPlay EncounterCard Placement
  | -- | set-aside cards go to the staging area
    AddSetAsideToStaging CardMatcher
  | ShuffleEncounterDiscardIntoDeck CardMatcher
  | -- | set-aside cards are shuffled into the encounter deck
    ShuffleSetAsideIntoEncounterDeck [EncounterCard]
  | -- | set-aside cards are removed from the game, as an encounter set a scenario stops using
    RemoveSetAsideFromGame [EncounterCard]
  | -- | matching encounter cards in the victory display are shuffled into the encounter deck
    ShuffleVictoryDisplayIntoEncounterDeck CardMatcher
  | -- | an objective ally in play joins the player as an ally
    PutObjectiveAllyIntoPlay ObjectiveId PlayerId
  | ReduceActiveCost CostId Int
  | AddToVictoryDisplay Target
  | PutShadowLocationIntoPlay EnemyId CardId
  | PutOnTopOfEncounterDeck EncounterCard
  | PutOnBottomOfEncounterDeck [EncounterCard]
  | -- | a player card goes to its owner's discard pile
    PutInDiscard PlayerCard
  | -- | a card goes facedown under the ally, or the Nameless enemy
    AttachFacedown Target PlayerCard
  | -- | the player commits characters to the escape test, one at a time
    MakeEscapeTest EscapeTest
  | ResolveEscapeTest EscapeTest
  | {- | a locate test (The Long Dark): the top card of the encounter deck is discarded, and
    the test is passed if it carries the pass icon; the messages for a fail, then a pass
    -}
    LocateTest Source [Message] [Message]
  | -- | every "Lost:" effect in play triggers (The Long Dark)
    TriggerLostEffects Source
  | -- | the "Lost:" effect of one card in play triggers, the scenario's for its setup card's
    TriggerLostEffectOf Source Target
  | -- | each player in turn chooses and triggers a "Lost:" effect in play, one not among
    -- those already chosen (The Long Dark)
    ChooseLostEffects Source [PlayerId] [Target]
  | {- | the player switches a card in hand with one in their deck, each taking the other's
    place
    -}
    SwapHandWithDeck PlayerId CardId CardId
  | {- | the quest deck goes back to the stage, put into play on its B side, with the
    stages after it to follow
    -}
    ResetToStage EncounterCard [EncounterCard]
  | ReturnToTopOfEncounterDeck Target
  | -- | an encounter card in play goes to the bottom of the encounter deck
    ReturnToBottomOfEncounterDeck Target
  | RemoveProgressFromQuest Source Int
  | -- | a shadow card dealt to the enemy becomes an enemy itself, engaged with the player
    PutShadowCardIntoPlay EnemyId CardId PlayerId
  | -- | a shadow card dealt to the enemy is put into play as an enemy engaged with the player
    PutShadowEnemyIntoPlay EnemyId CardId PlayerId
  | -- | a shadow card dealt to the enemy is added to the staging area as an enemy
    PutShadowEnemyIntoStaging EnemyId CardId
  | RemoveShadowCard EnemyId CardId
  | -- | the shadow card's effect has resolved: the enemy's attack before it, to tell
    -- whether it raised it
    CheckShadowRaisedAttack CardId EnemyId Int
  | -- | the shadow cards dealt to the enemy go to the encounter discard pile, as after an
    -- immediate attack outside the combat phase
    DiscardShadowCards EnemyId
  | ChooseAndDiscard Source PlayerId [Target]
  | DiscardEventsFromHand PlayerId
  | Exhaust Target
  | CommitCharacters PlayerId
  | CommitToQuest Target
  | Staging
  | QuestResolution
  | PlaceProgressOnQuest Int
  | TravelTo LocationId
  | OptionalEngagement PlayerId
  | EngagementCheck PlayerId Int
  | -- | an enemy with ambush entering play: the first player in turn order whose threat
    -- reaches its engagement cost engages it
    AmbushEngagementCheck EnemyId
  | -- | the enemy in the staging area makes engagement checks against each player, as
    -- ambush has it do: the first in turn order whose threat reaches its engagement cost
    -- engages it, as False Accusations reads
    EnemyEngagementChecks EnemyId
  | EngageEnemy PlayerId EnemyId
  | -- | the combat phase begins: the archery in play is dealt among the characters
    ResolveArchery
  | -- | the first player assigns this much archery damage, 1 at a time, among the
    -- characters, the points assigned so far kept; it is dealt once all are assigned
    ArcheryDamage Int [Target]
  | DealShadowCards
  | DealShadowCard EnemyId
  | AttachShadowCard EnemyId EncounterCard
  | EnemyAttacks PlayerId
  | EnemyAttack PlayerId EnemyId
  | DefendWith PlayerId EnemyId Target
  | DeclareDefenders PlayerId EnemyId [Target]
  | {- | the attacked player has declared: the other players still to be asked, in turn,
    whether a sentinel of theirs defends, with the defenders chosen so far
    -}
    DeclareSentinels PlayerId EnemyId [Target] [PlayerId]
  | DefendedByMany PlayerId EnemyId [Target]
  | -- | the players as a group spend resources of a sphere, from any of their heroes
    SpendGroupResources PlayerId Sphere Int
  | {- | as 'SpendResources' (or 'SpendGroupResources' when the flag is set), the heroes
    chosen so far to pay 1 each, once per resource: each hero's are spent at once when
    the last is chosen, a single spend from that hero's pool as Forage for Food reads it
    -}
    SpendResourcesChosen PlayerId Bool Sphere Int [HeroId]
  | -- | the players as a group discard resources from any of their heroes' pools, as
    -- many as they can up to the number, as prowl reads: not spent, so any hero's go
    DiscardGroupResources PlayerId Int
  | {- | every payment ends here: the player spends these resources from these pools, a
    hero's or an ally's, as one payment, then the spent-resources window opens once for
    each player whose characters paid. A card's cost opens its own as it is paid instead
    ('PaidCost'), its payment spanning heroes and allies.
    -}
    SpendResourcesFrom PlayerId [(Target, Int)]
  | -- | spend resources of a sphere, all of them from a single hero's pool
    SpendResourcesFromOneHero PlayerId Sphere Int
  | {- | the player may exhaust a ready character he controls with the trait to lower the
    cost being paid by 1, again and again, then pays the rest: see 'ReducedByExhausting'
    -}
    ExhaustToReduceCost PlayerId CostId Trait
  | -- | spend resources of a sphere, at most 1 from each hero, from heroes not yet used
    SpendResourcesFromDistinctHeroes PlayerId Sphere Int [HeroId]
  | UndefendedAttack PlayerId EnemyId
  | UndefendedDamage PlayerId EnemyId
  | -- | the attack's defenders, none when undefended
    ResolveShadow EnemyId PlayerId [Target]
  | -- | the shadow card dealt to the enemy turns face up
    RevealShadowCard EnemyId CardId
  | -- | the player chooses which of these engaged enemies attacks next
    ChooseEnemyAttack PlayerId [EnemyId]
  | ResolveShadowEffect CardId EnemyId PlayerId [Target]
  | -- | resolve one shadow card dealt to the enemy, unless it was cancelled
    ResolveShadowCard CardId EnemyId PlayerId [Target]
  | AttackDamage EnemyId Target
  | AddToEncounterDiscard [EncounterCard]
  | PlayerAttacks PlayerId
  | -- | the player chooses which of these enemies to attack next
    ChooseAttackTarget PlayerId [EnemyId]
  | DeclareAttackers PlayerId EnemyId
  | {- | the attacking player has declared: the other players still to be asked, in turn,
    whether a ranged character of theirs joins the attack
    -}
    DeclareRangedAttackers PlayerId EnemyId [PlayerId]
  | AttackWith Target
  | ResolveAttack PlayerId EnemyId
  | -- | an attack on the enemy resolved: the player it was engaged with, the attackers, and
    -- the enemy's traits
    AttackResolved EnemyId (Maybe PlayerId) [Target] (Set Trait)
  | AttackStagingAreaAlone PlayerId Target
  | StopAttacking Target
  | Ready Target
  | PlaceTokens Source Target Token Int
  | RemoveTokens Source Target Token Int
  | -- | the card, revealed or turned to its B side, gets its Time X in time counters
    PlaceTimeCounters Target
  | Run [Message]
  | -- | the message's effect itself, framed by its windows: see 'doFrame'
    Do Message
  | -- | the messages, unless the card has left play by the time they run
    WhileInPlay Target [Message]
  | -- | the messages, if the enemy's attack has destroyed a character, as "if this attack
    -- destroys a character" reads once the attack is over (see 'attackDestroyed')
    IfAttackDestroyed EnemyId [Message]
  | -- | the messages, if the enemy's attack dealt damage to one of the characters, as "if
    -- this attack deals damage to a Ship objective" reads once the attack is over
    IfAttackDamaged EnemyId [Target] [Message]
  | -- | resources added to a card that is out of play, kept for it until it enters play
    PlaceResourcesOutOfPlay CardId Int
  | -- | the player deals the damage to a hero the matcher picks out to pay a cost
    PayDamageHeroCost PlayerId HeroMatcher Int [Message]
  | -- | the messages, if the enemy's last attack dealt no damage, as King's Knight reads
    IfAttackDealtNoDamage EnemyId [Message]
  | DrawCards Drawing
  | -- | the draw itself, once nothing has stopped it, as Iron Shackles or Enchanted Stream
    DoDrawCards Drawing
  | Mulligan PlayerId
  | -- | log only: what is narrated until the block closes is filed under it
    OpenLogBlock LogBlock
  | CloseLogBlock UUID
  | -- | a step only one scenario's module knows, by name, as Arkham's
    ScenarioSpecific Text Value
  | {- | an enemy card, taken from where it was, comes into play engaged with the player
    as the enemy given, so the card putting it there can act on it after, as Arkham's
    createEnemy
    -}
    CreateEnemyEngaged PlayerId EnemyId EncounterCard
  | {- | the player takes the top card of the encounter deck facedown into his play area as
    a hidden card (The Blood of Gondor), the discard pile shuffled in first if the deck is
    empty
    -}
    TakeHiddenCard PlayerId
  | -- | the hidden card, if it is still hidden, turns faceup, then the messages run: what
    -- becomes of it
    TurnHiddenCardFaceup PlayerId EncounterCard [Message]
  | -- | the hidden card goes to the encounter discard pile, if it is still hidden
    DiscardHiddenCard PlayerId EncounterCard
  | -- | the ally is placed elsewhere, as Captured! puts Faramir under itself
    PlaceAlly AllyId Placement
  | -- | the shadow card just dealt to the enemy is dealt face up, as Silver Lamp reads: it
    -- still resolves as the enemy attacks
    TurnShadowCardFaceUp EnemyId CardId
  | -- | a card in the first player's hand goes to the second player's hand
    GiveFromHand PlayerId CardId PlayerId
  | -- | a card in the player's hand is shuffled into its owner's deck
    ShuffleFromHandIntoOwnersDeck PlayerId CardId
  | {- | the card, from out of play, is shuffled into the player's deck, as Arkham's
    ShuffleCardsIntoDeck: an encounter card too, as Recurring Nightmare
    -}
    ShuffleCardIntoDeck PlayerId Card
  | {- | the encounter card has entered the player's hand from their deck: its card, out of
    play, answers it, as Recurring Nightmare's Forced effect (as Arkham's drawn encounter
    cards resolve their revelations)
    -}
    EncounterCardEnteredHand PlayerId EncounterCard
  | -- | the enemy's attack on the player has resolved, defended by these: a window for each
    -- that took no damage defending it, before the attack's own After windows
    AttackDefended PlayerId EnemyId [Target]
  | -- | a player card from out of play goes to its owner's hand, as one a Search takes
    PutInHand PlayerCard
  | -- | a card leaves the player's hand for out of play, as for To Catch an Orc's
    -- out-of-play deck
    RemoveFromHand PlayerId CardId
  | -- | as the combat phase begins, the enemies with hinder engaged with players remove progress
    ResolveHinder
  | -- | the player readies this many of the characters, one at a time, as Heart of Fangorn
    -- lets no more than 5 refresh
    RefreshUpTo PlayerId Int [Target]
  | {- | the treachery leaves play for the player's hand, as Ill Tidings is drawn: an
    encounter card in hand cannot leave it
    -}
    DrawTreacheryIntoHand PlayerId TreacheryId
  | -- | as 'SearchEncounterDeckAndDiscard', the enemy found put into play engaged with the
    -- player, as "searches ... for a Boar Clan enemy and puts it into play engaged with him"
    SearchEncounterDeckAndDiscardToEngage PlayerId Target CardMatcher
  | -- | as 'SearchEncounterDeckAndDiscard', the card found revealed, as Forecastle Deck reads
    SearchEncounterDeckAndDiscardToReveal PlayerId Target CardMatcher
  | -- | as 'SearchEncounterDeckAndDiscard', the victory display searched too, as Corsair
    -- Confrontation reads
    SearchEncounterCardsAndVictoryDisplay PlayerId Target CardMatcher
  | {- | the cards go facedown underneath the target, out of play, as The Orcs' Search takes
    them: from a player's hand, deck or discard pile, or a location in play
    -}
    PlaceUnderneath Target [Card]
  | -- | as 'PayExhaustHeroCost', a hero or an ally the matchers pick out
    PayExhaustCharacterCost PlayerId HeroMatcher AllyMatcher [Message]
  | -- | as 'CreateEnemyEngaged', the enemy not yet engaging: it is engaged by a message of
    -- its own, as Faramir's damage comes first
    CreateEnemy EnemyId EncounterCard
  | -- | the card, already taken from wherever it was, goes on top of the player's deck, as
    -- Miruvor goes on its controller's deck rather than its owner's
    PutOnTopOfDeck PlayerId Card
  | -- | the card leaves the victory display, as Caradhras does to become the active location
    RemoveFromVictoryDisplay Card
  | -- | the enemy's attack resolves against another target, as Small Target turns it on
    -- another enemy: see 'LordOfTheRings.Message.Lifted.redirectAttack'
    RedirectAttack EnemyId Target
  | -- | the progress that would have been placed on the source's quest card, canceled: see
    -- 'QuestProgressCanceledBy'
    ProgressCanceled Source Int
  | {- | the players split into areas of their own, each with its own staging area, one
    for each group: see 'LordOfTheRings.Game.Base.Separation'. The encounter cards no
    player holds stay with the first group.
    -}
    SeparatePlayers [[PlayerId]]
  | -- | the area holding the player comes into focus, to resolve what follows
    FocusArea PlayerId
  | -- | the area in focus is to join another at the beginning of the travel phase
    JoinAtTravel
  | -- | the areas to join another do so, in turn order, at the beginning of the travel phase
    JoinAreas
  | -- | the first player's area joins the second's
    JoinArea PlayerId PlayerId
  | -- | the hero leaves play, set aside out of play, as Unexpected Rescue sets aside the
    -- heroes not chosen to start
    SetAsideHero HeroId
  | {- | a card declares the character as a defender of the enemy's attack on the player,
    exhausted or not, from the window after the enemy is declared as an attacker, as
    Anchor Watch reads: it defends in place of the player's own declaration, alongside
    any others declared so
    -}
    DeclareAsDefender PlayerId EnemyId Target
  | -- | the heading shifts one setting on-course or off-course (Dream-chaser rules)
    ShiftHeading Source HeadingShift
  | {- | the first player makes a Sailing test, as a stage with the Sailing keyword reads at
    the start of the quest phase: the heading shifts off-course, then characters commit
    -}
    SailingTest
  | -- | the first player commits characters to the Sailing test, one at a time, with those
    -- committed so far
    CommitToSailingTest PlayerId [Target]
  | -- | the committed characters say how many cards from the top of the encounter deck
    -- are looked at
    ResolveSailingTest [Target]
  | -- | the cards the Sailing test looked at: the success icons found, and those canceled
    SailingTestResult [EncounterCard] Int Int
  | -- | a card the Sailing test discarded, with the characters committed to it: one with an
    -- asterisk by its sailing icon has a Forced effect for it (The Grey Havens Nightmare)
    ResolveSailingDiscard EncounterCard [Target]
  | -- | the Sailing test is over: the cards it discarded and the characters committed to
    -- it, for a card's "*Forced: After this Sailing test ends" (Dream-chaser Nightmare)
    SailingTestEnded [EncounterCard] [Target]
  | -- | the non-Ship enemies leave the encounter deck for the Corsair deck, shuffled
    PrepareCorsairDeck
  | -- | the top card of the Corsair deck is revealed and put into play engaged with the
    -- player, for the source, its discard pile reshuffled first if it has run out
    RevealFromCorsairDeck Source PlayerId
  | -- | as 'RevealFromCorsairDeck', the enemy put into play dealt a shadow card, as Ramming
    -- Ship's shadow reads
    RevealFromCorsairDeckWithShadow Source PlayerId
  | -- | deal a shadow card to the enemy whose card this is, once it has entered play
    DealShadowCardToCard CardId
  | -- | the enemy goes to the staging area of the player's area, as Giant Cave Centipede
    -- moves to a different player's while the players are separated
    PlaceInStagingAreaOf PlayerId EnemyId
  | -- | the card the source revealed from the Corsair deck, as Light Cruiser reads it
    RevealedFromCorsairDeck Source PlayerId EncounterCard
  | -- | the Ship enemy engaged the player from the staging area: its Boarding X resolves
    ResolveBoarding PlayerId EnemyId
  | -- | each player takes a Ship objective of The Dream-chaser's Fleet, one the
    -- Dream-chaser, as "Players prepare their fleet" reads
    PrepareFleet
  | -- | the first of the players chooses one of the ships left, then the rest in turn
    ChooseFleetShip [PlayerId] [EncounterCard]
  | -- | an objective ally card, taken from where it was, enters play under the player
    PutObjectiveAllyIntoPlayUnder PlayerId EncounterCard
  | -- | a card in the player's hand goes to the bottom of his deck
    PutFromHandOnBottomOfDeck PlayerId CardId
  | -- | the player looks at the facedown side of the location, as an Uncharted location's
    -- Lost Island side hides it
    LookAtFacedownSide PlayerId LocationId
  | -- | the top cards of the Uncharted deck go to the staging area, Lost Island side faceup
    AddFromUnchartedDeck Int
  | -- | the location leaves play for the Uncharted deck, which is shuffled
    ShuffleIntoUnchartedDeck LocationId
  | {- | the progress is removed from each Uncharted location in the staging area, and they
    are shuffled so that the players do not know which one is which
    -}
    ShuffleUnchartedLocations
  | {- | the enemy's Grapple keyword resolves: it attaches facedown to the location in play
    its parenthetical picks, the first player choosing among those tied; with none in
    play, one not yet placed goes to the staging area (The Thing in the Depths)
    -}
    ResolveGrapple EnemyId
  | -- | an enemy card, taken from where it was, as a discarded or a shadow card, comes into
    -- play to resolve its Grapple keyword
    GrappleCard EncounterCard
  | -- | the location turns over keeping its progress, as Shrine to Morgoth floods
    FlipKeepingProgress LocationId
  | -- | the top cards of the Grotto deck go to the staging area, Grotto side faceup
    AddFromGrottoDeck Int
  | -- | the top card of the Grotto deck goes to the staging area Underwater side faceup, as
    -- Ancient Depths reads
    AddUnderwaterFromGrottoDeck
  | -- | the location leaves play for the Grotto deck, Grotto side up, which is shuffled
    ShuffleIntoGrottoDeck LocationId
  | -- | the card, out of play, goes to the bottom of the Grotto deck, Grotto side up, as
    -- Flooded Caverns returns one from the victory display
    PutOnBottomOfGrottoDeck EncounterCard
  | -- | the Uncharted location cards are laid out as the Island Map, each in its place, Lost
    -- Island or Temple side faceup (Temple of the Deceived)
    PrepareIslandMap [(Pos, EncounterCard)]
  | -- | the locations on the Island Map orthogonally adjacent to the active location are
    -- in the staging area, the others are not, as the active location changes or turns over
    ArrangeIslandMap
  | -- | the two locations on the Island Map switch places, as Deceitful Island switches the
    -- active location with one next to it
    SwapIslandMapPositions LocationId LocationId
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

uiToRun :: UI Message -> Message
uiToRun = \case
  Label _ msgs -> Run msgs
  TargetLabel _ msgs -> Run msgs
  AbilityLabel pid ability windows msgs -> Run $ UseAbility pid ability windows : msgs

-- | The message framed by its window, as Arkham's: the When window opens, the effect is
-- done, then the After window opens.
doFrame :: Message -> WindowType -> [Message]
doFrame msg w = [CheckWindows [mkWhen w], Do msg, CheckWindows [mkAfter w]]

chooseOne :: HasCallStack => PlayerId -> [UI Message] -> Message
chooseOne _ [] = error "chooseOne: no choices"
chooseOne pid choices = Ask pid $ ChooseOne choices

{- | The next resource of a payment made 1 at a time: the player picks a hero with one left
of those paying, or, once all are picked (or none is left to pick), each hero's resources
are spent at once.
-}
spendChosen :: PlayerId -> Bool -> Sphere -> Int -> [HeroId] -> [(HeroId, Int)] -> Message
spendChosen pid isGroup sphere n chosen payers = case [h | (h, k) <- payers, k > paid h] of
  -- the last hero left to pay pays the rest, unasked, as a player's own spend does
  [h] | n > 0, not isGroup -> SpendResourcesChosen pid isGroup sphere (n - 1) (h : chosen)
  hs@(_ : _) | n > 0 ->
    questionLabel (ikeyWith "label.payMore" [intVar "count" n]) pid
      [targetLabel h [SpendResourcesChosen pid isGroup sphere (n - 1) (h : chosen)] | h <- hs]
  _ -> SpendResourcesFrom pid [(HeroTarget h, paid h) | h <- ordNub (reverse chosen)]
 where
  paid h = length (filter (== h) chosen)

-- | Ask a question with a prompt explaining what is being chosen.
questionLabel :: HasCallStack => Text -> PlayerId -> [UI Message] -> Message
questionLabel prompt = questionLabelWith prompt Nothing []

{- | A question that names the card causing it and the cards it is aimed at, so the
board can mark them.
-}
questionLabelWith
  :: HasCallStack => Text -> Maybe Target -> [Target] -> PlayerId -> [UI Message] -> Message
questionLabelWith _ _ _ _ [] = error "questionLabel: no choices"
questionLabelWith prompt source targets pid choices = Ask pid $ QuestionLabel prompt source targets $ ChooseOne choices

{- | A window's question, which the given message builds afresh when the game changes
under it.
-}
playerWindow :: Text -> Message -> PlayerId -> [UI Message] -> Message
playerWindow prompt rebuild pid = Ask pid . QuestionLabel prompt Nothing [] . PlayerWindow rebuild . ChooseOne

chooseOrRunOne :: HasCallStack => PlayerId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne pid choices = chooseOne pid choices

-- | The messages as one block in the log, opened before them and closed after.
inLogBlock :: MonadRandom m => LogBlockKind -> [Message] -> m [Message]
inLogBlock kind msgs = do
  block <- LogBlock <$> getRandom <*> pure kind
  pure $ OpenLogBlock block : msgs <> [CloseLogBlock block.id]

{- | The enemy the message takes out of play other than by destroying it, as an effect
that discards it, returns it to the encounter deck or adds it to the victory display.
-}
removedEnemy :: Message -> Maybe EnemyId
removedEnemy = \case
  Discard (EnemyTarget eid) -> Just eid
  ReturnToTopOfEncounterDeck (EnemyTarget eid) -> Just eid
  ReturnToBottomOfEncounterDeck (EnemyTarget eid) -> Just eid
  ShuffleIntoEncounterDeck (EnemyTarget eid) -> Just eid
  RemoveFromGame (EnemyTarget eid) -> Just eid
  AddToVictoryDisplay (EnemyTarget eid) -> Just eid
  _ -> Nothing
