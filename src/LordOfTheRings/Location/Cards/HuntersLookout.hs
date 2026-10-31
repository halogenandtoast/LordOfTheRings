module LordOfTheRings.Location.Cards.HuntersLookout (huntersLookout) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Objective.Types (objectiveGuard)
import LordOfTheRings.Scenario.Scenarios.TheHuntForGollum.Helpers (mordorEnemies)
import LordOfTheRings.Trait (Trait (Clue))

newtype HuntersLookout = HuntersLookout LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

huntersLookout :: LocationCard HuntersLookout
huntersLookout = location HuntersLookout Cards.huntersLookout

-- | Each Mordor enemy with at least 1 Clue card attached gets +2 defense.
instance HasModifiersFor HuntersLookout where
  getModifiersFor (HuntersLookout a) = do
    guards <- mapMaybe (objectiveGuard . toAttrs) <$> objectivesWithTrait Clue
    enemies <- mordorEnemies
    for_ [e | (e, cid) <- enemies, cid `elem` guards] \e -> modified a e [DefenseModifier 2]

-- | Shadow: Attacking enemy gets +1 defense for each Clue card in play until the end of the
-- round.
instance RunMessage HuntersLookout where
  runMessage msg l@(HuntersLookout a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      clues <- length <$> objectivesWithTrait Clue
      when (clues > 0) $ roundModifier a eid (DefenseModifier clues)
      pure l
    _ -> HuntersLookout <$> runMessage msg a
