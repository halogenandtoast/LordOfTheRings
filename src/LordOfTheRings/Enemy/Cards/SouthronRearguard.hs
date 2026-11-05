module LordOfTheRings.Enemy.Cards.SouthronRearguard (southronRearguard) where

import LordOfTheRings.Card (toCardCode)
import LordOfTheRings.Enemy.CardDefs qualified as Cards
import LordOfTheRings.Enemy.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Quest.CardDefs qualified as Quests
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (getTheBlackGate)

newtype SouthronRearguard = SouthronRearguard EnemyAttrs
  deriving anyclass (IsEnemy, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

southronRearguard :: EnemyCard SouthronRearguard
southronRearguard = enemy SouthronRearguard Cards.southronRearguard

instance HasModifiersFor SouthronRearguard where
  getModifiersFor (SouthronRearguard a) = do
    mains <- getMainQuestCards
    let progress = sum [q.progress | q <- mains, q.flipped, toCardCode q == toCardCode Quests.ambushTheMenOfHarad]
    modifySelf a [CannotHaveAttachments, EngagementCostModifier (negate progress)]
    gate <- getTheBlackGate
    for_ gate \g -> modified a (CardIdTarget a.cardId) [CannotBePlacedUnder $ toTarget g]

instance RunMessage SouthronRearguard where
  runMessage msg e@(SouthronRearguard a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      placeEnemy eid StagingArea
      pure e
    _ -> SouthronRearguard <$> runMessage msg a
