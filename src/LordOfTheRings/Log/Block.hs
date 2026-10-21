-- | A block in the log: the entries an event writes, drawn together under its header.
--
-- An event such as an attack spans several answers, so what it is lives in the game's
-- state, as Arkham's @gameLogBlockStack@ does, opened and closed by messages around the
-- event's own. Anything narrated while a block is open is filed under it.
module LordOfTheRings.Log.Block (module LordOfTheRings.Log.Block) where

import Data.Text qualified as T
import Data.UUID qualified as UUID
import LordOfTheRings.Card
import LordOfTheRings.Id
import LordOfTheRings.Json
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

data LogBlockKind
  = -- | an engaged enemy attacks the player
    EnemyAttackBlock PlayerId EnemyId
  | -- | the player's declared characters attack the enemy
    PlayerAttackBlock PlayerId EnemyId [Target]
  | -- | an encounter card revealed, and what it does
    RevealBlock EncounterCard
  | -- | a card ability triggered, forced or not, with what it costs and does
    AbilityBlock PlayerId Source Bool
  | -- | a card played from hand, with what it costs and does
    PlayBlock PlayerId Card
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

data LogBlock = LogBlock
  { logBlockId :: UUID
  , logBlockKind :: LogBlockKind
  }
  deriving stock (Show, Eq, Generic)

instance ToJSON LogBlock where
  toJSON = genericToJSON (aesonOptions $ Just "logBlock")

instance FromJSON LogBlock where
  parseJSON = genericParseJSON (aesonOptions $ Just "logBlock")

instance HasField "id" LogBlock UUID where
  getField = logBlockId

-- | The block's own segment of a group id. Never contains @/@, which nests.
logBlockKey :: LogBlock -> Text
logBlockKey b = kind <> ":" <> UUID.toText b.id
 where
  kind = case b.logBlockKind of
    EnemyAttackBlock {} -> "enemyAttack"
    PlayerAttackBlock {} -> "playerAttack"
    RevealBlock {} -> "reveal"
    AbilityBlock {} -> "ability"
    PlayBlock {} -> "play"

-- | The group id of the innermost of these open blocks, outermost first.
logBlockPath :: [LogBlock] -> Maybe Text
logBlockPath = \case
  [] -> Nothing
  blocks -> Just $ T.intercalate "/" (map logBlockKey blocks)
