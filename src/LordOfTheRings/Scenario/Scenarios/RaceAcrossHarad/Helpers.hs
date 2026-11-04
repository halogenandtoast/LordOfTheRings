{- | What the cards of Race Across Harad share: the Orcs' area, with its own quest deck of
stages 1C to 4C, and the progress the Orcs place on their stage.

Cards in the Orcs' area are placed 'OrcsArea': not in the staging area, immune to player
card effects, and they cannot leave it (the scenario gives them 'CannotLeaveStagingArea',
which holds of that area too). While the players are at the Orcs' stage, the cards of the
Orcs' area are in the staging area, and the scenario keeps which they are; they go back
once the players move on (MEC57 rules, "The Orcs' Area").
-}
module LordOfTheRings.Scenario.Scenarios.RaceAcrossHarad.Helpers (
  Meta (..),
  getMeta,
  getOrcsMeta,
  getOrcsStage,
  getPlayersStage,
  atOrcsStage,
  stageOf,
  orcsArea,
  orcsAreaThreat,
  placeOrcsProgress,
  addToOrcsArea,
  toOrcsArea,
  orcEnemy,
  addOrcToOrcsArea,
  addCardToOrcsArea,
  limitProgressThisRound,
  getUrukChieftains,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (CardType (EnemyType), EncounterCard)
import LordOfTheRings.Classes.Entity (toAttrs)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Helpers.Staging (getEnemyThreat, getLocationThreat)
import LordOfTheRings.Id (EnemyId)
import LordOfTheRings.Matcher (AllyMatcher (..), CardMatcher (..), EnemyMatcher (..), cardMatches, select)
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted (ReverseQueue, removeFromEncounterDiscard, roundModifier)
import LordOfTheRings.Modifier (ModifierType (ProgressPerRoundLimit))
import LordOfTheRings.Source (Sourceable)
import LordOfTheRings.Placement (Placement (OrcsArea, StagingArea))
import LordOfTheRings.Prelude
import LordOfTheRings.Scenario.Scenarios.FlightOfTheStormcaller.Helpers (getPlayersStage, stageOf)
import LordOfTheRings.Scenario.Types (ScenarioAttrs (..))
import LordOfTheRings.Target
import LordOfTheRings.Trait (Trait (Orc))

-- | The Orcs' quest stage, the stages under it, the progress on it, and while the players
-- are at the Orcs' stage the cards of the Orcs' area, then in the staging area.
data Meta = Meta
  { orcsStage :: Maybe EncounterCard
  , orcsDeck :: [EncounterCard]
  , orcsProgress :: Int
  , orcsJoined :: Bool
  , orcsAreaCards :: [Target]
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

getMeta :: ScenarioAttrs -> Meta
getMeta attrs = fromMaybe (Meta Nothing [] 0 False []) $ parseMaybe parseJSON attrs.meta

getOrcsMeta :: HasGame m => m Meta
getOrcsMeta = getsGame (getMeta . toAttrs . (.scenario))

-- | The Orcs' stage number, 0 before their area is prepared.
getOrcsStage :: HasGame m => m Int
getOrcsStage = maybe 0 stageOf . (.orcsStage) <$> getOrcsMeta

-- | The players' main quest is at the Orcs' stage: its name and number are theirs.
atOrcsStage :: HasGame m => m Bool
atOrcsStage = do
  orcs <- getOrcsStage
  players <- getPlayersStage
  pure $ orcs > 0 && orcs == players

-- | The cards in the Orcs' area, wherever it is now.
orcsArea :: HasGame m => m [Target]
orcsArea = do
  meta <- getOrcsMeta
  enemies <- getsGame (toList . (.entities.enemies))
  locations <- getsGame (toList . (.entities.locations))
  let placed = [(toTarget e, e.placement) | e <- enemies] <> [(toTarget l, l.placement) | l <- locations]
  pure [t | (t, p) <- placed, p == OrcsArea || (p == StagingArea && t `elem` meta.orcsAreaCards)]

-- | The total threat of the cards in the Orcs' area.
orcsAreaThreat :: HasGame m => m Int
orcsAreaThreat = do
  area <- orcsArea
  sum <$> for area \case
    EnemyTarget eid -> getEnemyThreat eid
    LocationTarget lid -> getLocationThreat lid
    _ -> pure 0

-- | Progress placed on the Orcs' quest stage.
placeOrcsProgress :: ReverseQueue m => Int -> m ()
placeOrcsProgress n = when (n > 0) $ push $ ScenarioSpecific "orcs.progress" (toJSON n)

-- | The enemy or location goes to the Orcs' area: to the staging area as one of its cards,
-- while the players are at the Orcs' stage.
addToOrcsArea :: (ReverseQueue m, Targetable a) => a -> m ()
addToOrcsArea = push . toOrcsArea

-- | 'addToOrcsArea' as a message, for "after this attack".
toOrcsArea :: Targetable a => a -> Message
toOrcsArea = ScenarioSpecific "orcs.add" . toJSON . toTarget

orcEnemy :: CardMatcher
orcEnemy = CardWithType EnemyType <> CardWithTrait Orc

-- | The first Orc enemy of the cards discarded goes from the discard pile to the Orcs'
-- area, as "discard cards ... until an Orc enemy is discarded. Add that enemy to the Orcs'
-- area" reads.
addOrcToOrcsArea :: ReverseQueue m => [EncounterCard] -> m ()
addOrcToOrcsArea cards = for_ (find (cardMatches orcEnemy) cards) \card -> do
  removeFromEncounterDiscard card
  addCardToOrcsArea card

-- | The enemy card, out of play, comes into play in the Orcs' area.
addCardToOrcsArea :: ReverseQueue m => EncounterCard -> m ()
addCardToOrcsArea card = do
  eid <- getRandom
  push $ CreateEnemy eid card
  addToOrcsArea (EnemyTarget eid)

{- | Stages 2B, 3B and 4B: "Forced: After characters are committed to quest, no more than 5
progress can be placed here this round (10 progress instead if each Tamed Mumak in play is
committed to the quest)." Every player has committed once the staging step begins, so it
is read then: with several players, the first done committing would have it read before
the others' Mumaks are.
-}
limitProgressThisRound :: (ReverseQueue m, Sourceable a, Targetable a) => a -> m ()
limitProgressThisRound a = do
  mumaks <- select (AllyWithCardCode "17038")
  committed <- select (AllyWithCardCode "17038" <> CommittedAlly)
  roundModifier a a (ProgressPerRoundLimit $ if length committed == length mumaks then 10 else 5)

-- | Uruk Chieftain, wherever it is.
getUrukChieftains :: HasGame m => m [EnemyId]
getUrukChieftains = select (EnemyWithCardCode "16061")
