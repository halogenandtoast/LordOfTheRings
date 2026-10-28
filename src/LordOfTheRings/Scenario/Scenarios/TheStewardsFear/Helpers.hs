-- | What the cards of The Steward's Fear share: the underworld deck and the cards stacked
-- facedown under locations, the tokens on the current quest, and the attack dealing its
-- damage.
module LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (
  Meta (..),
  getMeta,
  underworldValue,
  placeUnderworldCards,
  revealUnderworldCard,
  shuffleUnderworldIntoEncounterDeck,
  removeTokenFromQuest,
  removeAllTokensFromQuest,
  attackDealingDamage,
  armFalseLead,
  falseLeadArmed,
  claimClue,
  investigation,
  runInvestigation,
) where

import Data.Aeson.Types (parseMaybe)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Ability
import LordOfTheRings.Card (CardCode, EncounterCard, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (HasQueue, peekQueue, push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Runner (QuestAttrs (..))
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Scenario.Types (ScenarioAttrs)
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token
import LordOfTheRings.Window

-- | The scenario's own state: the underworld deck, and the cards facedown under each
-- location, by the location's card id.
data Meta = Meta
  { underworldDeck :: [EncounterCard]
  , underneath :: Map CardId [EncounterCard]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta [] mempty) $ parseMaybe parseJSON attrs.meta

{- | The location's Underworld X: as it enters play, this many cards from the underworld
deck go facedown under it. The Fourth Star's X is the number of players.
-}
underworldValue :: HasGame m => CardCode -> m Int
underworldValue code
  | code `elem` map toCardCode [Locations.sewers, Locations.storehouse, Locations.rootsOfMindolluin] = pure 1
  | code == toCardCode Locations.housesOfTheDead = pure 2
  | code == toCardCode Locations.theFourthStar = length <$> getRemainingPlayers
  | otherwise = pure 0

-- | The top cards of the underworld deck go facedown under the location, as many as are left.
placeUnderworldCards :: ReverseQueue m => LocationId -> Int -> m ()
placeUnderworldCards lid n = push $ ScenarioSpecific "underworld.place" (toJSON (lid, n))

{- | Reveal the top card of the underworld deck and add it to the staging area; a Clue is
discarded instead, as Unwelcome Discovery reads.
-}
revealUnderworldCard :: ReverseQueue m => m ()
revealUnderworldCard = push $ ScenarioSpecific "underworld.reveal" Null

shuffleUnderworldIntoEncounterDeck :: ReverseQueue m => m ()
shuffleUnderworldIntoEncounterDeck = push $ ScenarioSpecific "underworld.shuffle" Null

-- | The tokens on the current quest: the quest, and how many of each kind.
currentQuestTokens :: HasGame m => m [(Target, Token, Int)]
currentQuestTokens =
  getCurrentQuest >>= \case
    Just t@(QuestTarget qid) -> do
      quest <- getsGame (Map.lookup qid . entitiesQuests . gameEntities)
      pure
        [ (t, token, n)
        | Just q <- [quest]
        , token <- [Progress, Resource]
        , let n = countTokens token (questTokens $ toAttrs q)
        , n > 0
        ]
    _ -> pure []

{- | Remove 1 token from the current quest: the first player chooses which kind when it
has both, as Underworld Dissident's shadow reads.
-}
removeTokenFromQuest :: ReverseQueue m => EnemyId -> m ()
removeTokenFromQuest eid = do
  tokens <- currentQuestTokens
  lead <- getLead
  chooseOrRunOneM lead do
    questionLabeled "theStewardsFear.removeToken"
    causedBy (EnemyTarget eid)
    for_ tokens \(q, token, _) ->
      labeled (if token == Progress then "theStewardsFear.progress" else "theStewardsFear.resource")
        $ removeTokens (EnemySource eid) q token 1

-- | Remove all tokens from the current quest, as False Lead's shadow reads.
removeAllTokensFromQuest :: (ReverseQueue m, Sourceable source) => source -> m ()
removeAllTokensFromQuest source = do
  tokens <- currentQuestTokens
  for_ tokens \(q, token, n) -> removeTokens source q token n

{- | The enemy whose attack is dealing its damage, read off the game's queue: the window
after its attack still waits there, with no step before the attack's damage ahead of it.
A character destroyed now is destroyed by that attack.
ponytail: only the attack's damage destroys; a character a shadow effect's damage destroys
during the attack does not count. Read the attack's cause off the damage if a card needs it.
-}
attackDealingDamage :: (MonadTrans t, HasQueue Message m) => t m (Maybe EnemyId)
attackDealingDamage = lift $ go <$> peekQueue
 where
  go = \case
    [] -> Nothing
    CheckWindows ws : rest -> case [eid | Window After (EnemyAttacking eid _) <- ws] of
      eid : _ -> Just eid
      [] -> go rest
    msg : rest
      | beforeDamage msg -> Nothing
      | otherwise -> go rest
  beforeDamage = \case
    DeclareDefenders {} -> True
    DeclareSentinels {} -> True
    DefendWith {} -> True
    UndefendedAttack {} -> True
    ResolveShadow {} -> True
    ResolveShadowCard {} -> True
    AttackDamage {} -> True
    DefendedByMany {} -> True
    UndefendedDamage {} -> True
    _ -> False

falseLeadMarker :: EnemyId -> CardId -> Message
falseLeadMarker eid cid = ScenarioSpecific "falseLead" (toJSON (eid, cid))

{- | False Lead's shadow, resolved, lasts for the attack: its mark waits on the queue until
the attack is over.
-}
armFalseLead :: (MonadTrans t, HasQueue Message m) => EnemyId -> CardId -> t m ()
armFalseLead eid cid = afterAttack eid [falseLeadMarker eid cid]

-- | The False Lead whose shadow lasts for the enemy's attack, if one does.
falseLeadArmed :: (MonadTrans t, HasQueue Message m) => EnemyId -> t m (Maybe CardId)
falseLeadArmed eid = lift $ headMay . mapMaybe marked <$> peekQueue
 where
  marked = \case
    ScenarioSpecific "falseLead" v | Just (eid', cid) <- parseMaybe parseJSON v, eid' == eid -> Just cid
    _ -> Nothing

{- | A Clue claimed into the victory display: "add it to the victory display, and place N
resource tokens on the current quest."
-}
claimClue :: (ReverseQueue m, Sourceable a, Targetable a) => a -> Int -> m ()
claimClue clue n = do
  addToVictoryDisplay clue
  traverse_ (\q -> placeResources clue q n) =<< getCurrentQuest

{- | Conspiracy and The Grand Design: "Forced: After the active location leaves play as an
explored location, place 1 resource token on this quest. If there are 4 or more resource
tokens on it, advance to the next stage."
-}
investigation :: QuestAttrs -> Ability
investigation a = mkAbility a 1 $ forced $ ActiveLocationExploredWindow When

runInvestigation :: Message -> QuestAttrs -> QueueT Message GameT QuestAttrs
runInvestigation msg attrs = case msg of
  UseCardAbility _ (isSource attrs -> True) 1 _ -> do
    placeResources attrs attrs 1
    checkStage attrs.id
    pure attrs
  CheckStage qid | qid == attrs.id -> do
    when (countTokens Resource attrs.questTokens >= 4) $ push (CompleteQuest attrs.id)
    pure attrs
  _ -> runMessage msg attrs
