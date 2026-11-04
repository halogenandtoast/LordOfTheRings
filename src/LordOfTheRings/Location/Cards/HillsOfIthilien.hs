module LordOfTheRings.Location.Cards.HillsOfIthilien (hillsOfIthilien) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.JourneyToTheCrossRoads.Helpers (haradEnemy)

newtype HillsOfIthilien = HillsOfIthilien LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

hillsOfIthilien :: LocationCard HillsOfIthilien
hillsOfIthilien = location HillsOfIthilien Cards.hillsOfIthilien

-- | While Hills of Ithilien is the active location, it gains: "Forced: After a player
-- engages a Harad enemy, deal 1 damage to that enemy."
instance HasAbilities HillsOfIthilien where
  getAbilities (HillsOfIthilien a) = [mkAbility a 1 $ forced $ MatchingEnemyEngagedWindow After haradEnemy | a.placement == ActiveLocation]

-- | Shadow: Discard a non-objective attachment you control.
instance RunMessage HillsOfIthilien where
  runMessage msg l@(HillsOfIthilien a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [eid | Window _ (EnemyEngaged eid _) <- ws] \eid -> dealDamage a eid 1
      pure l
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      discardNonObjectiveAttachment a pid
      pure l
    _ -> HillsOfIthilien <$> runMessage msg a
