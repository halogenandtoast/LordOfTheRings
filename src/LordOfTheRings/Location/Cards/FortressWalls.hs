module LordOfTheRings.Location.Cards.FortressWalls (fortressWalls) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype FortressWalls = FortressWalls LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

fortressWalls :: LocationCard FortressWalls
fortressWalls = location FortressWalls Cards.fortressWalls

-- | While Fortress Walls is the active location, for each resource spent by a player, raise
-- that player's threat by 1. A player spends resources from his heroes' pools.
-- Shadow: If your threat is 40 or higher, attacking enemy makes an additional attack
-- against you after this one.
instance RunMessage FortressWalls where
  runMessage msg l@(FortressWalls a) = case msg of
    RemoveTokens (PlayerSource pid) (HeroTarget _) Resource n | a.placement == ActiveLocation && n > 0 -> do
      encounterRaisesThreat pid n
      FortressWalls <$> runMessage msg a
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      threat <- (.threat) <$> getPlayer pid
      when (threat >= 40) $ additionalAttack pid eid
      pure l
    _ -> FortressWalls <$> runMessage msg a
