module LordOfTheRings.Message (module LordOfTheRings.Message) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.Card
import LordOfTheRings.Effect
import LordOfTheRings.Outcome
import LordOfTheRings.Placement
import LordOfTheRings.Id
import LordOfTheRings.Log.Block
import LordOfTheRings.Matcher.Types (CardMatcher)
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Trait
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
  | DealDamage Source Target Int
  | HealDamage Source Target Int
  | -- | the player looks at the top card of the encounter deck, and may move it to the
    -- bottom when the flag says so
    LookAtTopOfEncounterDeck PlayerId Source Bool
  | MoveTopOfEncounterDeckToBottom
  | DiscardFromHand PlayerId CardId
  | ChooseAndDiscardFromHand PlayerId Source Int
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
  | PayForCard PlayerId Card [Window]
  | PlayEvent PlayerId PlayerCard [Window]
  | PutAllyIntoPlay PlayerId CardId
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
  | ShuffleDiscardIntoDeck PlayerId
  | -- | a card from the player's deck goes to their hand
    TakeFromDeck PlayerId CardId
  | -- | debug mode: the player looks through their deck and takes any card into hand
    DebugSearchDeck PlayerId
  | -- | debug mode: the player looks through the encounter deck and reveals any card
    DebugSearchEncounterDeck PlayerId
  | LowerThreat PlayerId Int
  | ActionWindow PlayerId
  | PayCost CostId
  | PaidCost CostId
  | SpendResources PlayerId Sphere Int
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
  | PutQuestIntoPlay EncounterCard
  | CompleteQuest QuestId
  | AdvanceQuest
  | AdvanceToQuest EncounterCard
  | RemoveFromEncounterDeck EncounterCard
  | RemoveFromEncounterDiscard EncounterCard
  | -- | the player searches the encounter deck and discard pile for a matching card to
    -- add to the staging area, then the deck is shuffled
    SearchEncounterDeckAndDiscard PlayerId Target CardMatcher
  | ShuffleDeck DeckSignifier
  | CheckWindows [Window]
  | ResponseWindow [Window] [PlayerId] [(Source, Int)] Int
  | UseAbility PlayerId Ability [Window]
  | UseCardAbility PlayerId Source Int [Window]
  | CreateEffect Effect
  | AttachCard AttachmentId Target
  | PlayAttachment PlayerId PlayerCard
  | ChooseAndExhaust Source PlayerId Int
  | ChooseAndExhaustHero Source PlayerId
  | DiscardRandomFromHand PlayerId Int
  | RemoveFromQuest Target
  | AttachTreachery TreacheryId Target
  | ClaimObjective ObjectiveId Target
  | PlaceObjective ObjectiveId Placement
  | SetObjectiveGuard ObjectiveId CardId
  | -- | the next encounter card guards the objective: an enemy or location goes to the
    -- staging area to guard it, any other card is revealed
    GuardObjective ObjectiveId
  | PutObjectiveIntoPlay EncounterCard
  | TakePrisoner HeroId
  | RescuePrisoner HeroId
  | SetAside EncounterCard
  | -- | a stage whose condition may have just been met checks whether it is defeated
    CheckStage QuestId
  | -- | the player's top card goes face down in front of them as an Orc Guard engaged
    -- with them
    PlaceOrcGuard PlayerId
  | EngageOrcGuard PlayerId PlayerCard
  | ShuffleIntoEncounterDeck Target
  | -- | a treachery dealt as a shadow card resolves its revelation as if revealed, here
    -- attaching itself where its placement says
    PutShadowTreacheryIntoPlay EnemyId CardId Placement
  | ShufflePlayerDeck PlayerId
  | -- | the player takes matching cards from those searched in their deck into hand, any
    -- number of them or only 1 as the flag says, then shuffles their deck
    ChooseFromSearch PlayerId Source [Card] CardMatcher Bool
  | DiscardTopOfDeck PlayerId Int
  | -- | the player looks at the top cards of the encounter deck, reveals 1 of them and
    -- discards the rest
    RevealOneOfTopEncounterCards PlayerId Source Int
  | -- | set-aside cards go to the staging area
    AddSetAsideToStaging CardMatcher
  | ShuffleEncounterDiscardIntoDeck CardMatcher
  | -- | an objective ally in play joins the player as an ally
    PutObjectiveAllyIntoPlay ObjectiveId PlayerId
  | -- | the quest deck goes back to the stage, put into play on its B side, with the
    -- stages after it to follow
    ResetToStage EncounterCard [EncounterCard]
  | ReturnToTopOfEncounterDeck Target
  | RemoveProgressFromQuest Source Int
  | -- | a shadow card dealt to the enemy becomes an enemy itself, engaged with the player
    PutShadowCardIntoPlay EnemyId CardId PlayerId
  | RemoveShadowCard EnemyId CardId
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
  | EngageEnemy PlayerId EnemyId
  | DealShadowCards
  | DealShadowCard EnemyId
  | AttachShadowCard EnemyId EncounterCard
  | EnemyAttacks PlayerId
  | EnemyAttack PlayerId EnemyId
  | DefendWith PlayerId EnemyId Target
  | DeclareDefenders PlayerId EnemyId [Target]
  | DefendedByMany PlayerId EnemyId [Target]
  | -- | spend resources of a sphere, at most 1 from each hero, from heroes not yet used
    SpendResourcesFromDistinctHeroes PlayerId Sphere Int [HeroId]
  | UndefendedAttack PlayerId EnemyId
  | UndefendedDamage PlayerId EnemyId
  | ResolveShadow EnemyId PlayerId (Maybe Target)
  | -- | the shadow card dealt to the enemy turns face up
    RevealShadowCard EnemyId CardId
  | -- | the player chooses which of these engaged enemies attacks next
    ChooseEnemyAttack PlayerId [EnemyId]
  | ResolveShadowEffect CardId EnemyId PlayerId (Maybe Target)
  | -- | resolve one shadow card dealt to the enemy, unless it was cancelled
    ResolveShadowCard CardId EnemyId PlayerId (Maybe Target)
  | AttackDamage EnemyId Target
  | AddToEncounterDiscard [EncounterCard]
  | PlayerAttacks PlayerId
  | DeclareAttackers PlayerId EnemyId
  | AttackWith Target
  | ResolveAttack PlayerId EnemyId
  | AttackResolved EnemyId [Target]
  | AttackStagingAreaAlone PlayerId Target
  | StopAttacking Target
  | Ready Target
  | PlaceTokens Source Target Token Int
  | RemoveTokens Source Target Token Int
  | Run [Message]
  | DrawCards Drawing
  | Mulligan PlayerId
  | -- | log only: what is narrated until the block closes is filed under it
    OpenLogBlock LogBlock
  | CloseLogBlock UUID
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

uiToRun :: UI Message -> Message
uiToRun = \case
  Label _ msgs -> Run msgs
  TargetLabel _ msgs -> Run msgs
  AbilityLabel pid ability windows msgs -> Run $ UseAbility pid ability windows : msgs

chooseOne :: (HasCallStack) => PlayerId -> [UI Message] -> Message
chooseOne _ [] = error "chooseOne: no choices"
chooseOne pid choices = Ask pid $ ChooseOne choices

-- | Ask a question with a prompt explaining what is being chosen.
questionLabel :: (HasCallStack) => Text -> PlayerId -> [UI Message] -> Message
questionLabel prompt = questionLabelWith prompt Nothing []

-- | A question that names the card causing it and the cards it is aimed at, so the
-- board can mark them.
questionLabelWith :: (HasCallStack) => Text -> Maybe Target -> [Target] -> PlayerId -> [UI Message] -> Message
questionLabelWith _ _ _ _ [] = error "questionLabel: no choices"
questionLabelWith prompt source targets pid choices = Ask pid $ QuestionLabel prompt source targets $ ChooseOne choices

-- | A window's question, which the given message builds afresh when the game changes
-- under it.
playerWindow :: Text -> Message -> PlayerId -> [UI Message] -> Message
playerWindow prompt rebuild pid = Ask pid . QuestionLabel prompt Nothing [] . PlayerWindow rebuild . ChooseOne

chooseOrRunOne :: (HasCallStack) => PlayerId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne pid choices = chooseOne pid choices

-- | The messages as one block in the log, opened before them and closed after.
inLogBlock :: (MonadRandom m) => LogBlockKind -> [Message] -> m [Message]
inLogBlock kind msgs = do
  block <- LogBlock <$> getRandom <*> pure kind
  pure $ OpenLogBlock block : msgs <> [CloseLogBlock block.id]
