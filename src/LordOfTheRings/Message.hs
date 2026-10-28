module LordOfTheRings.Message (module LordOfTheRings.Message) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.Card
import LordOfTheRings.Effect
import LordOfTheRings.Id
import LordOfTheRings.Json (withDefaults)
import LordOfTheRings.Log.Block
import LordOfTheRings.Matcher.Types (CardMatcher, HeroMatcher)
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
data EscapeStat = EscapeWillpower | EscapeAttack
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
  | BeginRound
  | RaiseThreat PlayerId Int
  | CheckEliminations
  | EliminatePlayer PlayerId
  | GameOver Outcome
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
  | CheckDefeated Target
  | Destroy Target
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
  | TakeControl AllyId PlayerId
  | RemoveFromDiscard PlayerId CardId
  | -- | an ally card put into play under the player's control, keeping its owner
    PutAllyIntoPlayUnder PlayerId PlayerCard
  | PutHeroIntoPlay PlayerId PlayerCard
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
  | Search PlayerId DeckSignifier CardMatcher CardHandler DeckHandler
  | AddToStaging EncounterCard
  | RevealEncounterCard
  | RevealCard EncounterCard
  | WhenRevealed Target
  | -- | the lead clicks the revealed card to resolve its When Revealed effect
    AskToResolveWhenRevealed Target EncounterCard
  | FinishRevelation TreacheryId
  | {- | only the card's When Revealed effect resolves, as Lieutenant of Mordor's: it is
    not revealed, so no keyword, window or cancel touches it
    -}
    ResolveWhenRevealedOf EncounterCard
  | PutQuestIntoPlay EncounterCard
  | CompleteQuest QuestId
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
  | AttachCard AttachmentId Target
  | PlayAttachment PlayerId PlayerCard
  | PlayAttachmentOn PlayerId PlayerCard Target
  | -- | an attachment in the player's hand is put into play on the host, not played
    PutAttachmentIntoPlayFromHand PlayerId CardId Target
  | {- | the hero leaves the game for one from its owner's collection, which takes the
    attachments it can
    -}
    SwapHero HeroId PlayerCard
  | ChooseAndExhaust Source PlayerId Int
  | ChooseAndExhaustHero Source PlayerId
  | -- | the player exhausts a hero the matcher picks out to pay a cost; the ability then
    -- used is told which, by the hero's exhausted window
    PayExhaustHeroCost PlayerId HeroMatcher [Message]
  | -- | the player deals 1 damage to a character they control other than the one given
    DamageAnotherCharacter Source PlayerId Target
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
  | PutObjectiveIntoPlay EncounterCard
  | -- | an objective card comes into play attached to the hero, as a setup attaches it
    PutObjectiveIntoPlayAttached EncounterCard Target
  | -- | the card leaves play for good; an objective, as Book of Mazarbul, or a location
    RemoveFromGame Target
  | -- | a stage leaves the quest deck unplayed, as exploring The Banks removes stage 2
    RemoveFromQuestDeck EncounterCard
  | TakePrisoner HeroId
  | RescuePrisoner HeroId
  | SetAside EncounterCard
  | -- | a stage whose condition may have just been met checks whether it is defeated
    CheckStage QuestId
  | {- | the player's top card goes face down in front of them as an Orc Guard engaged
    with them
    -}
    PlaceOrcGuard PlayerId
  | EngageOrcGuard PlayerId PlayerCard
  | ShuffleIntoEncounterDeck Target
  | {- | a treachery dealt as a shadow card resolves its revelation as if revealed, here
    attaching itself where its placement says
    -}
    PutShadowTreacheryIntoPlay EnemyId CardId Placement
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
  | -- | set-aside cards go to the staging area
    AddSetAsideToStaging CardMatcher
  | ShuffleEncounterDiscardIntoDeck CardMatcher
  | -- | set-aside cards are shuffled into the encounter deck
    ShuffleSetAsideIntoEncounterDeck [EncounterCard]
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
  | EngageEnemy PlayerId EnemyId
  | -- | the combat phase begins: the archery in play is dealt among the characters
    ResolveArchery
  | -- | the first player deals this much archery damage, 1 at a time, among the characters
    ArcheryDamage Int
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
  | Run [Message]
  | -- | the messages, unless the card has left play by the time they run
    WhileInPlay Target [Message]
  | {- | the messages, if any of the targets has left play by the time they run, as "if this
    attack destroys a character" reads once the attack has resolved
    -}
    IfAnyLeftPlay [Target] [Message]
  | DrawCards Drawing
  | Mulligan PlayerId
  | -- | log only: what is narrated until the block closes is filed under it
    OpenLogBlock LogBlock
  | CloseLogBlock UUID
  | -- | a step only one scenario's module knows, by name, as Arkham's
    ScenarioSpecific Text Value
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

uiToRun :: UI Message -> Message
uiToRun = \case
  Label _ msgs -> Run msgs
  TargetLabel _ msgs -> Run msgs
  AbilityLabel pid ability windows msgs -> Run $ UseAbility pid ability windows : msgs

chooseOne :: HasCallStack => PlayerId -> [UI Message] -> Message
chooseOne _ [] = error "chooseOne: no choices"
chooseOne pid choices = Ask pid $ ChooseOne choices

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
