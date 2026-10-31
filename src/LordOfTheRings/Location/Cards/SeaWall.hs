module LordOfTheRings.Location.Cards.SeaWall (seaWall) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AStormOnCobasHaven.Helpers (guardedObjectives)

newtype SeaWall = SeaWall LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

seaWall :: LocationCard SeaWall
seaWall = location SeaWall Cards.seaWall

-- | Sea-wall gets +2 threat and +2 quest points for each objective guarded by an encounter
-- card.
instance HasModifiersFor SeaWall where
  getModifiersFor (SeaWall a) = do
    x <- length <$> guardedObjectives
    when (x > 0) $ modifySelf a [ThreatModifier (2 * x), QuestPointModifier (2 * x)]

-- | Shadow: Attacking enemy gets +1 attack for each objective guarded by an encounter card.
instance RunMessage SeaWall where
  runMessage msg l@(SeaWall a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      x <- length <$> guardedObjectives
      when (x > 0) $ enemyGetsAttack (CardIdSource cid) eid x
      pure l
    _ -> SeaWall <$> runMessage msg a
