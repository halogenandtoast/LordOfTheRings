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
  | PutQuestIntoPlay EncounterCard
  | ShuffleDeck DeckSignifier
  | CheckWindows [Window]
  | ResponseWindow [Window] [PlayerId] [(Source, Int)] Int
  | UseAbility PlayerId Ability [Window]
  | UseCardAbility PlayerId Source Int [Window]
  | CreateEffect Effect
  | Exhaust Target
  | Ready Target
  | PlaceTokens Source Target Token Int
  | RemoveTokens Source Target Token Int
  | Run [Message]
  | DrawCards Drawing
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON)

uiToRun :: UI Message -> Message
uiToRun = \case
  Label _ msgs -> Run msgs
  TargetLabel _ msgs -> Run msgs
  AbilityLabel pid ability windows msgs -> Run $ UseAbility pid ability windows : msgs

chooseOne :: (HasCallStack) => PlayerId -> [UI Message] -> Message
chooseOne _ [] = error "chooseOne: no choices"
chooseOne pid choices = Ask pid $ ChooseOne choices

chooseOrRunOne :: (HasCallStack) => PlayerId -> [UI Message] -> Message
chooseOrRunOne _ [x] = uiToRun x
chooseOrRunOne pid choices = chooseOne pid choices
