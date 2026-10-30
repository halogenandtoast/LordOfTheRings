module LordOfTheRings.Location.Cards.FogBank (fogBank) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype FogBank = FogBank LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

fogBank :: LocationCard FogBank
fogBank = location FogBank Cards.fogBank

-- | While Fog Bank is the active location, Ship enemies do not make engagement checks,
-- cannot attack, and cannot be attacked.
instance HasModifiersFor FogBank where
  getModifiersFor (FogBank a) = when (a.placement == ActiveLocation) do
    ships <- select shipEnemy
    for_ ships \s -> modified a s [NoEngagementChecks, CannotAttack, CannotBeAttacked]

-- | Shadow: If your heading is at the worst setting, attacking enemy makes an additional
-- attack after this one.
instance RunMessage FogBank where
  runMessage msg l@(FogBank a) = case msg of
    ResolveShadowEffect cid eid pid _ | cid == a.cardId -> do
      whenM atWorstHeading $ additionalAttack pid eid
      pure l
    _ -> FogBank <$> runMessage msg a
