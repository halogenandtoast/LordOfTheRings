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
  | DealtDamage Target Int
  | Traveled LocationId
  | DeclaredAsDefender Target EnemyId
  | DeclaredAsAttacker Target EnemyId
  | -- | an attack destroyed the enemy: the characters that attacked
    EnemyDestroyedByAttack EnemyId [Target]
  | CharacterDestroyed Target Card
  | -- | an encounter card revealed from the encounter deck
    Revealed Target
  | -- | a location about to leave play as an explored location
    Explored LocationId
  | EnemyAttacking EnemyId PlayerId
  | PhaseEnds Phase
  | RoundEnds
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
  | -- | a shadow card's effect about to resolve: the card, and the enemy it was dealt to
    ShadowEffectTriggered CardId EnemyId
  | -- | a shadow effect about to resolve during an attack the character defends
    ShadowEffectAgainst Target
  | -- | a character left play, as its card
    CharacterLeftPlay Card
  | -- | a character left play: who controlled it, and its traits as it left
    LeftPlay Target PlayerId (Set Trait)
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
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

mkWhen :: WindowType -> Window
mkWhen = Window When

mkAfter :: WindowType -> Window
mkAfter = Window After
