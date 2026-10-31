{- | What the cards of Celebrimbor's Secret share: Scour effects, the cards underneath The
Orcs' Search, damage on locations, and Celebrimbor's Mould.

Scour (MEC30): the keyword does nothing by itself. When a card says to trigger a Scour
effect, the effect after the keyword on that card resolves; with several, the first
player chooses the order.
-}
module LordOfTheRings.Scenario.Scenarios.CelebrimborsSecret.Helpers (
  isScour,
  scour,
  triggerEachScour,
  scourCardsInPlay,
  triggerTopmostScourInDiscard,
  getOrcsSearch,
  cardsUnderOrcsSearch,
  placeUnderOrcsSearch,
  placeDamageOnActiveLocation,
  assignDamageAmongLocations,
  getMould,
  mouldController,
  getBellach,
  currentQuestProgress,
  mouldOnHero,
  bellachShielded,
) where

import Data.Aeson.Types (parseMaybe)
import LordOfTheRings.Card (Card, CardDef (..), toCardCode, toCardDef)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasModifiersFor (HasModifiersM, modified)
import LordOfTheRings.Classes.HasQueue (push)
import Data.Map.Strict qualified as Map
import LordOfTheRings.Entities (entitiesEnemies, entitiesLocations, entitiesObjectives, entitiesQuests)
import LordOfTheRings.Enemy.CardDefs qualified as Enemies
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Id
import LordOfTheRings.Keyword (Keyword (Scour))
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Modifier (ModifierType (CannotEngagePlayer, ImmuneToPlayerCardEffects))
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (Objective, objectiveUnderneath)
import LordOfTheRings.Placement (Placement (AttachedTo))
import LordOfTheRings.Prelude
import LordOfTheRings.Source
import LordOfTheRings.Target

-- | The Scour effect of the card resolves.
scour :: (ReverseQueue m, Targetable a) => a -> m ()
scour = push . ScenarioSpecific "scour" . toJSON . toTarget

-- | Whether the message triggers the card's Scour effect.
isScour :: Targetable a => a -> Message -> Bool
isScour a = \case
  ScenarioSpecific "scour" v -> parseMaybe parseJSON v == Just (toTarget a)
  _ -> False

{- | "Trigger each Scour effect currently in play": each enemy and location in play with
Scour, in the order the first player chooses. Ancient Foundation's does not stack with
other copies of it, so only one copy's triggers.
-}
triggerEachScour :: ReverseQueue m => m ()
triggerEachScour = do
  cards <- scourCardsInPlay
  lead <- getLead
  chooseOneAtATimeM lead do
    questionLabeled "celebrimborsSecret.scourOrder"
    targets cards scour

-- | The enemies and locations in play with Scour, one Ancient Foundation among them, and
-- in nightmare mode the setup card's, which the scenario resolves.
scourCardsInPlay :: HasGame m => m [Target]
scourCardsInPlay = do
  es <- getsGame (toList . entitiesEnemies . gameEntities)
  ls <- getsGame (toList . entitiesLocations . gameEntities)
  nightmare <- inNightmareMode
  let scouring :: CardDef -> Bool
      scouring def = Scour `member` cdKeywords def
      (foundations, others) = partition ((== toCardCode Locations.ancientFoundation) . toCardCode) [l | l <- ls, scouring (toCardDef l)]
  pure $ [toTarget e | e <- es, scouring (toCardDef e)] <> map toTarget (take 1 foundations <> others) <> [ScenarioTarget | nightmare]

-- | "Trigger the topmost Scour effect in the discard pile": the scenario resolves it.
triggerTopmostScourInDiscard :: ReverseQueue m => m ()
triggerTopmostScourInDiscard = push $ ScenarioSpecific "scourFromDiscard" Null

getOrcsSearch :: HasGame m => m (Maybe Objective)
getOrcsSearch = objectiveWith Objectives.theOrcsSearch

getMould :: HasGame m => m (Maybe Objective)
getMould = objectiveWith Objectives.celebrimborsMould

objectiveWith :: HasGame m => CardDef -> m (Maybe Objective)
objectiveWith def = getsGame $ find ((== toCardCode def) . toCardCode) . toList . entitiesObjectives . gameEntities

-- | The number of cards facedown underneath The Orcs' Search.
cardsUnderOrcsSearch :: HasGame m => m Int
cardsUnderOrcsSearch = maybe 0 (length . objectiveUnderneath . toAttrs) <$> getOrcsSearch

-- | The card goes facedown underneath The Orcs' Search.
placeUnderOrcsSearch :: ReverseQueue m => Card -> m ()
placeUnderOrcsSearch card = getOrcsSearch >>= traverse_ \o -> placeUnderneath (toId o) [card]

placeDamageOnActiveLocation :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
placeDamageOnActiveLocation source n = select IsActiveLocation >>= traverse_ \l -> placeDamage source l n

{- | The first player assigns the damage among the locations in play, 1 at a time, as
Ancient Foundation's Scour reads: the scenario asks for each.
-}
assignDamageAmongLocations :: (ReverseQueue m, Sourceable source) => source -> Int -> m ()
assignDamageAmongLocations source n = when (n > 0) $ push $ ScenarioSpecific "assignLocationDamage" (toJSON (toSource source, n))

-- | The player controlling the hero Celebrimbor's Mould is attached to.
mouldController :: HasGame m => m (Maybe PlayerId)
mouldController =
  getMould >>= \case
    Just o | AttachedTo host@(HeroTarget _) <- o.placement -> characterController host
    _ -> pure Nothing

getBellach :: HasGame m => m (Maybe EnemyId)
getBellach = headMay <$> select (EnemyWithCardCode $ toCardCode Enemies.bellachCelebrimborsSecret)

-- | The progress on the current quest.
currentQuestProgress :: HasGame m => m Int
currentQuestProgress =
  getCurrentQuest >>= \case
    Just (QuestTarget qid) -> maybe 0 (.progress) <$> getsGame (Map.lookup qid . entitiesQuests . gameEntities)
    _ -> pure 0

-- | Whether Celebrimbor's Mould is attached to a hero.
mouldOnHero :: HasGame m => m Bool
mouldOnHero =
  getMould <&> \case
    Just o | AttachedTo (HeroTarget _) <- o.placement -> True
    _ -> False

-- | "Bellach is immune to player card effects and cannot be engaged."
bellachShielded :: (HasModifiersM m, Sourceable source) => source -> m ()
bellachShielded source =
  getBellach >>= traverse_ \b -> do
    players <- getRemainingPlayers
    modified source b (ImmuneToPlayerCardEffects : map CannotEngagePlayer players)
