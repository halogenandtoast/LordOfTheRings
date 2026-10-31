-- | What the cards of The Steward's Fear share: the underworld deck and the cards stacked
-- facedown under locations, and the tokens on the current quest.
module LordOfTheRings.Scenario.Scenarios.TheStewardsFear.Helpers (
  Meta (..),
  getMeta,
  underworldValue,
  placeUnderworldCards,
  revealUnderworldCard,
  shuffleUnderworldIntoEncounterDeck,
  placeUnderLocation,
  revealUnderneath,
  revealRemovedPlot,
  underneathCount,
  removeTokenFromQuest,
  removeAllTokensFromQuest,
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
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Classes.RunMessage
import LordOfTheRings.Entities (entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.GameT (GameT)
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (locationCardId)
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
  , removedPlots :: [EncounterCard]
  -- ^ the Plot cards removed from the game during setup, as Traitors' Den reveals one
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass ToJSON

-- | A game saved before the removed plots were kept has none.
instance FromJSON Meta where
  parseJSON = withObject "Meta" \o -> Meta <$> o .: "underworldDeck" <*> o .: "underneath" <*> o .:? "removedPlots" .!= []

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta [] mempty []) $ parseMaybe parseJSON attrs.meta

{- | The location's Underworld X: as it enters play, this many cards from the underworld
deck go facedown under it. The Fourth Star's X is the number of players.
-}
underworldValue :: HasGame m => CardCode -> m Int
underworldValue code
  | code `elem` map toCardCode [Locations.sewers, Locations.storehouse, Locations.rootsOfMindolluin, Locations.traitorsDen, Locations.archedTunnel] = pure 1
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

-- | The card goes facedown underneath the location, as Murderous Turncoat goes: an enemy
-- leaves play for it.
placeUnderLocation :: ReverseQueue m => LocationId -> EncounterCard -> m ()
placeUnderLocation lid card = push $ ScenarioSpecific "underworld.under" (toJSON (lid, card))

-- | The facedown cards underneath the location are revealed one at a time, the location
-- staying in play, as Arched Tunnel's Travel reads.
revealUnderneath :: ReverseQueue m => LocationId -> m ()
revealUnderneath lid = push $ ScenarioSpecific "underworld.revealUnder" (toJSON lid)

-- | One of the Plot cards removed from the game during setup, at random, is revealed and
-- added to the staging area, as Traitors' Den reads.
revealRemovedPlot :: ReverseQueue m => m ()
revealRemovedPlot = push $ ScenarioSpecific "underworld.plot" Null

-- | The facedown cards underneath the location.
underneathCount :: HasGame m => LocationId -> m Int
underneathCount lid = do
  meta <- getsGame (getMeta . toAttrs . (.scenario))
  cid <- locationCardId . toAttrs <$> getLocation lid
  pure $ length $ Map.findWithDefault [] cid meta.underneath

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
