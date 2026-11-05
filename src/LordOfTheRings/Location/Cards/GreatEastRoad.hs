module LordOfTheRings.Location.Cards.GreatEastRoad (greatEastRoad) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))
import LordOfTheRings.Trait (Trait (Nazgul))

newtype GreatEastRoad = GreatEastRoad LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

greatEastRoad :: LocationCard GreatEastRoad
greatEastRoad = location GreatEastRoad Cards.greatEastRoad

instance HasModifiersFor GreatEastRoad where
  getModifiersFor (GreatEastRoad a) = do
    enemies <- select (EnemyWithTrait Nazgul)
    modifySelf a [ThreatModifier $ 1 + length enemies]
    when (a.placement == StagingArea) $ for_ enemies \eid -> modified a eid [EngagementCostModifier (-5)]

instance RunMessage GreatEastRoad where
  runMessage msg l@(GreatEastRoad a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      attackNextPlayerAfterAttack eid pid
      pure l
    _ -> GreatEastRoad <$> runMessage msg a
