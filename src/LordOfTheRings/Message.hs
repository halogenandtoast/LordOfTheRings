module LordOfTheRings.Message (module LordOfTheRings.Message) where

import LordOfTheRings.Ability.Types
import LordOfTheRings.Card
import LordOfTheRings.Effect
import LordOfTheRings.Outcome
import LordOfTheRings.Placement
import LordOfTheRings.Id
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Phase
import LordOfTheRings.Prelude
import LordOfTheRings.Question
import LordOfTheRings.Window

data DeckSignifier = EncounterDeck
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CardMatcher = CardWithCode CardCode
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

cardIs :: CardDef -> CardMatcher
cardIs = CardWithCode . cdCardCode

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
  | CheckDefeated Target
  | Destroy Target
  | PlaceEnemy EnemyId Placement
  | PlaceLocation LocationId Placement
  | SetFirstPlayer PlayerId
  | EndRound
  | WithTarget Target Message
  | PlayCard PlayerId CardId
  | PayForCard PlayerId Card
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
  | FinishRevelation TreacheryId
  | PutQuestIntoPlay EncounterCard
  | CompleteQuest QuestId
  | AdvanceQuest
  | AdvanceToQuest EncounterCard
  | RemoveFromEncounterDeck EncounterCard
  | ShuffleDeck DeckSignifier
  | CheckWindows [Window]
  | ResponseWindow [Window] [PlayerId] [(Source, Int)] Int
  | UseAbility PlayerId Ability [Window]
  | UseCardAbility PlayerId Source Int [Window]
  | CreateEffect Effect
  | AttachCard AttachmentId Target
  | PlayAttachment PlayerId PlayerCard
  | ChooseAndExhaust Source PlayerId Int
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
  | UndefendedAttack PlayerId EnemyId
  | UndefendedDamage PlayerId EnemyId
  | ResolveShadow EnemyId PlayerId (Maybe Target)
  | ResolveShadowEffect CardId EnemyId PlayerId (Maybe Target)
  | AttackDamage EnemyId Target
  | AddToEncounterDiscard [EncounterCard]
  | PlayerAttacks PlayerId
  | DeclareAttackers PlayerId EnemyId
  | AttackWith Target
  | ResolveAttack PlayerId EnemyId
  | StopAttacking Target
  | Ready Target
  | PlaceTokens Source Target Token Int
  | RemoveTokens Source Target Token Int
  | Run [Message]
  | DrawCards Drawing
  | Mulligan PlayerId
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

chooseOrRunOne :: (HasCallStack) => PlayerId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne pid choices = chooseOne pid choices
