-- | What Journey to the Cross-roads' cards share: the Harad enemies, the cards facedown
-- underneath The Cross-roads and The Black Gate.
module LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (
  haradEnemy,
  theCrossRoads,
  getTheBlackGate,
  cardsUnderTheCrossRoads,
  revealFromUnderTheCrossRoads,
  placeUnderTheBlackGate,
  canPlaceUnderTheBlackGate,
  lowestBy,
) where

import LordOfTheRings.Card (Card, toCard, toCardCode)
import LordOfTheRings.Classes.Entity (toAttrs, toId)
import LordOfTheRings.Classes.HasGame
import LordOfTheRings.Classes.HasQueue (push)
import LordOfTheRings.Entities (entitiesObjectives)
import LordOfTheRings.Game.Base (gameEntities)
import LordOfTheRings.Helpers.Modifiers (getModifiers)
import LordOfTheRings.Modifier (ModifierType (CannotBePlacedUnder))
import LordOfTheRings.Id
import LordOfTheRings.Location.CardDefs qualified as Locations
import LordOfTheRings.Location.Types (LocationAttrs (locationUnderneath))
import LordOfTheRings.Matcher
import LordOfTheRings.Message
import LordOfTheRings.Message.Lifted
import LordOfTheRings.Objective.CardDefs qualified as Objectives
import LordOfTheRings.Objective.Types (Objective)
import LordOfTheRings.Prelude
import LordOfTheRings.Target (Target (CardIdTarget), toTarget)
import LordOfTheRings.Trait (Trait (Harad))

haradEnemy :: EnemyMatcher
haradEnemy = EnemyWithTrait Harad

theCrossRoads :: LocationMatcher
theCrossRoads = LocationWithCardCode (toCardCode Locations.theCrossRoadsTheLandOfShadow)

getTheBlackGate :: HasGame m => m (Maybe Objective)
getTheBlackGate =
  getsGame $ find ((== toCardCode Objectives.theBlackGate) . toCardCode) . toList . entitiesObjectives . gameEntities

-- | The cards facedown underneath The Cross-roads, the top one first.
cardsUnderTheCrossRoads :: HasGame m => m [Card]
cardsUnderTheCrossRoads = do
  crossRoads <- select theCrossRoads
  concat <$> for crossRoads \l -> locationUnderneath . toAttrs <$> getLocation l

{- | The top facedown card underneath The Cross-roads is revealed: added to the staging
area, or with a player given put into play engaged with him. The Cross-roads takes it as
the message resolves, so each reveal takes the card on top then.
-}
revealFromUnderTheCrossRoads :: ReverseQueue m => Maybe PlayerId -> m ()
revealFromUnderTheCrossRoads = push . ScenarioSpecific "theCrossRoads.reveal" . toJSON

-- | An option that places the enemy under the Gate must be able to do so in full.
canPlaceUnderTheBlackGate :: HasGame m => EnemyId -> m Bool
canPlaceUnderTheBlackGate eid = do
  gate <- getTheBlackGate
  present <- selectAny (EnemyWithId eid)
  case gate of
    Just g | present -> do
      enemy <- getEnemy eid
      notElem (CannotBePlacedUnder $ toTarget g) <$> getModifiers (CardIdTarget $ (toCard enemy).id)
    _ -> pure False

-- | The enemy is placed facedown under The Black Gate.
placeUnderTheBlackGate :: ReverseQueue m => EnemyId -> m ()
placeUnderTheBlackGate eid = do
  gate <- getTheBlackGate
  allowed <- canPlaceUnderTheBlackGate eid
  for_ gate \g -> when allowed do
    enemy <- getEnemy eid
    placeUnderneath (toId g) [toCard enemy]

-- | The enemies with the lowest value, ties kept for the player to choose among.
lowestBy :: HasGame m => (EnemyId -> m Int) -> [EnemyId] -> m [EnemyId]
lowestBy value enemies = do
  valued <- for enemies \e -> (e,) <$> value e
  pure case minimumMay (map snd valued) of
    Nothing -> []
    Just low -> [e | (e, v) <- valued, v == low]
