{- | What Dungeons Deep and Caverns Dim's cards share: the riddle area and its riddles (The
Hobbit: Over Hill and Under Hill rules insert, "The Riddle Area" and "Riddles").

Riddles in the Dark, stage 2, waits in the riddle area beside stage 1 with Gollum, placed
'RiddleArea', and Bilbo Baggins, who stays with the first player. The scenario answers a
riddle in steps: the first player names each item, shuffles his deck and discards from its
top, the players may then take actions, as Bilbo's Magic Ring's (FAQ: "Players can take
actions after step 2, before step 3"), and the matches are counted.
-}
module LordOfTheRings.Scenario.Scenarios.DungeonsDeepAndCavernsDim.Helpers (
  Meta (..),
  Named (..),
  RiddleState (..),
  getMeta,
  getRiddleState,
  answerRiddle,
  riddleDiscard,
  riddlesIgnored,
  hasRiddle,
  matchesNamed,
  gollumEnemy,
  getRiddleStage,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (
  CardCode,
  CardCost (StaticCost),
  CardDef (..),
  CardType,
  EncounterCard,
  HasCardDef,
  toCardCode,
  toCardDef,
 )
import LordOfTheRings.Card.CardDef (Riddle, RiddleItem)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Id (PlayerId, QuestId)
import LordOfTheRings.Matcher (EnemyMatcher (EnemyWithCardCode))
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (ReverseQueue)
import LordOfTheRings.Name (Name (..))
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Source
import LordOfTheRings.Sphere (Sphere)
import LordOfTheRings.Trait (Trait)

-- | An item the first player names: a card type, the treasure type, a sphere or a cost.
data Named
  = NamedCardType CardType
  | NamedTreasure
  | NamedSphere Sphere
  | NamedCost Int
  | NamedTitle Text
  | NamedTrait Trait
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A riddle being answered.
data RiddleState = RiddleState
  { riddleCard :: EncounterCard
  -- ^ the card whose riddle it is
  , riddleSource :: Source
  -- ^ what has it answered, read back once it is answered
  , riddleFromReveal :: Bool
  -- ^ answered instead of resolving the card revealed, which is discarded after
  , riddleAnswerer :: PlayerId
  , riddleToName :: [RiddleItem]
  , riddleNamed :: [Named]
  , riddleDiscards :: Int
  , riddleDiscarded :: [CardCode]
  -- ^ the cards discarded for it so far
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

newtype Meta = Meta {riddle :: Maybe RiddleState}
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta Nothing) $ parseMaybe parseJSON attrs.meta

getRiddleState :: HasGame m => m (Maybe RiddleState)
getRiddleState = getsGame ((.riddle) . getMeta . toAttrs . (.scenario))

-- | The card has a riddle printed on it.
hasRiddle :: HasCardDef a => a -> Bool
hasRiddle = isJust . cdRiddle . toCardDef

{- | The first player answers the riddle printed on the card, unless riddle effects are
ignored (Into the Fire). The source reads the answer back from the message
@ScenarioSpecific "riddle.answered" (source, matches)@.
-}
answerRiddle :: (ReverseQueue m, Sourceable source) => source -> Bool -> EncounterCard -> m ()
answerRiddle source fromReveal card = do
  ignored <- riddlesIgnored
  unless ignored $ for_ (cdRiddle $ toCardDef card) \(r :: Riddle) ->
    push $ ScenarioSpecific "riddle.answer" $ toJSON (toSource source, fromReveal, card, r)

{- | Discard more cards from the top of the answering player's deck for the riddle being
answered, as Bilbo's Magic Ring does.
-}
riddleDiscard :: ReverseQueue m => Int -> m ()
riddleDiscard n = push $ ScenarioSpecific "riddle.discard" (toJSON n)

-- | Stage 3B reads "All riddle effects are ignored."
riddlesIgnored :: HasGame m => m Bool
riddlesIgnored = getsGame (any ((== toCardCode Quests.intoTheFire) . toCardCode) . toList . (.entities.quests))

{- | Whether the card discarded matches the item named: a treasure is of the treasure type
alone, and only a printed cost is matched.
-}
matchesNamed :: CardDef -> Named -> Bool
matchesNamed def = \case
  NamedTreasure -> cdTreasure def
  NamedCardType t -> cdCardType def == t && not (cdTreasure def)
  NamedSphere s -> cdSphere def == Just s
  NamedCost n -> cdCost def == Just (StaticCost n)
  NamedTitle title -> let Name printed _ = cdName def in printed == title
  NamedTrait trait -> trait `member` cdCardTraits def

gollumEnemy :: EnemyMatcher
gollumEnemy = EnemyWithCardCode (toCardCode Enemies.gollumDungeonsDeep)

-- | Riddles in the Dark, while it is in play.
getRiddleStage :: HasGame m => m (Maybe QuestId)
getRiddleStage =
  getsGame
    ( fmap toId
        . find ((== toCardCode Quests.riddlesInTheDark) . toCardCode)
        . toList
        . (.entities.quests)
    )
