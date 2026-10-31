{- | What the cards of Encounter at Amon Dîn share: villager tokens, and the Rescued
Villagers and Dead Villagers objectives that keep count of them.

Villager tokens are discarded one at a time: each discarded token puts a damage token on
Dead Villagers, unless Lord Alcaron's controller exhausts him to place it on a location
instead. A token discarded is a 'DoStep' 1 of its 'RemoveTokens', which Lord Alcaron
answers while he may, and Dead Villagers otherwise or once he declines ('DoStep' 2).
-}
module LordOfTheRings.Scenario.Scenarios.EncounterAtAmonDin.Helpers (
  withVillagers,
  getVillagers,
  objectiveOf,
  deadVillagerCount,
  rescuedVillagerCount,
  discardVillagers,
  discardRescuedVillagers,
  villagerDies,
  alcaronMayRedirect,
  rescuedVillagerOr,
) where

import Data.Map.Strict qualified as Map
import LordOfTheRings.Card (CardDef, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame (HasGame, getsGame)
import LordOfTheRings.Classes.HasQueue (push, pushAll)
import LordOfTheRings.Queue (QueueT)
import LordOfTheRings.Entities (entitiesLocations, entitiesObjectives, entitiesQuests)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Location.Types (LocationAttrs, locationTokens, tokensL)
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (objectiveTokens)
import LordOfTheRings.Prelude
import LordOfTheRings.Quest.Types (questTokens)
import LordOfTheRings.Source
import LordOfTheRings.Target
import LordOfTheRings.Token

-- | "Villagers X": the location enters play with X villager tokens on it.
withVillagers :: Int -> LocationAttrs -> LocationAttrs
withVillagers n = tokensL %~ addTokens Villager n

-- | The villager tokens on a location, quest stage or objective.
getVillagers :: HasGame m => Target -> m Int
getVillagers target = do
  entities <- getsGame gameEntities
  pure $ countTokens Villager $ case target of
    LocationTarget lid -> maybe mempty (locationTokens . toAttrs) $ Map.lookup lid (entitiesLocations entities)
    QuestTarget qid -> maybe mempty (questTokens . toAttrs) $ Map.lookup qid (entitiesQuests entities)
    ObjectiveTarget oid -> maybe mempty (objectiveTokens . toAttrs) $ Map.lookup oid (entitiesObjectives entities)
    _ -> mempty

-- | The objective in play that is this card.
objectiveOf :: HasGame m => CardDef -> m (Maybe ObjectiveId)
objectiveOf def =
  getsGame (fmap toId . find ((== toCardCode def) . toCardCode) . toList . entitiesObjectives . gameEntities)

-- | The damage tokens on Dead Villagers.
deadVillagerCount :: HasGame m => m Int
deadVillagerCount = maybe (pure 0) (getDamage . ObjectiveTarget) =<< objectiveOf Objectives.deadVillagers

-- | The villager tokens on Rescued Villagers.
rescuedVillagerCount :: HasGame m => m Int
rescuedVillagerCount = maybe (pure 0) (getVillagers . ObjectiveTarget) =<< objectiveOf Objectives.rescuedVillagers

{- | Discard up to N villager tokens from the card, each one answered as it goes (see the
module's note); how many there were to discard.
-}
discardVillagers :: (ReverseQueue m, Sourceable source, Targetable target) => source -> target -> Int -> m Int
discardVillagers source target n = do
  k <- min n <$> getVillagers (toTarget target)
  when (k > 0) do
    removeTokens source target Villager k
    replicateM_ k $ push $ DoStep 1 (RemoveTokens (toSource source) (toTarget target) Villager 1)
  pure k

{- | Discard N villager tokens from Rescued Villagers, counted as they go: it takes no
damage, discarding a villager for each it would take instead (see Rescued Villagers).
-}
discardRescuedVillagers :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
discardRescuedVillagers source n =
  traverse_ (\o -> dealDamage source (ObjectiveTarget o) n) =<< objectiveOf Objectives.rescuedVillagers

-- | Place a damage token on Dead Villagers.
villagerDies :: (ReverseQueue m, Sourceable source) => source -> m ()
villagerDies source = traverse_ (\o -> dealDamage source (ObjectiveTarget o) 1) =<< objectiveOf Objectives.deadVillagers

-- | Lord Alcaron is ready to place a discarded villager token on a location in play.
alcaronMayRedirect :: HasGame m => m Bool
alcaronMayRedirect =
  (&&)
    <$> selectAny (AllyWithCardCode (toCardCode Objectives.lordAlcaron) <> ReadyAlly)
    <*> selectAny AnyLocation

{- | "Either discard 1 villager token from Rescued Villagers, or ...": the player picks,
and with no villager there to discard, the other happens.
-}
rescuedVillagerOr
  :: (ReverseQueue m, Sourceable source, Targetable source) => source -> PlayerId -> Text -> QueueT Message m () -> m ()
rescuedVillagerOr source pid key orElse = do
  rescued <- rescuedVillagerCount
  if rescued > 0
    then chooseOneM pid do
      questionLabeled "amonDin.villagerOr"
      causedBy source
      labeled "amonDin.discardVillager" $ discardRescuedVillagers source 1
      labeled key orElse
    else pushAll =<< capture orElse
