module LordOfTheRings.Location.Cards.MorgulRoad (morgulRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheMorgulVale.Helpers (removeFromTower, towerProgress)

newtype MorgulRoad = MorgulRoad LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

morgulRoad :: LocationCard MorgulRoad
morgulRoad = location MorgulRoad Cards.morgulRoad

-- | Response: After Morgul Road leaves play as an explored location, the first player may
-- remove 1 progress token from To the Tower or deal 1 damage to any enemy in play.
instance HasAbilities MorgulRoad where
  getAbilities (MorgulRoad a) = [mkAbility a 1 $ response $ ExploredWindow When a.id]

-- | Shadow: Return attacking enemy to the staging area after this attack.
instance RunMessage MorgulRoad where
  runMessage msg l@(MorgulRoad a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      progress <- towerProgress
      enemies <- select AnyEnemy
      chooseOneM lead do
        questionLabeled "morgulRoad.choose"
        causedBy a
        when (progress > 0) $ labeled "morgulRoad.removeProgress" $ removeFromTower a 1
        targets enemies \e -> dealDamage a e 1
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> MorgulRoad <$> runMessage msg a
