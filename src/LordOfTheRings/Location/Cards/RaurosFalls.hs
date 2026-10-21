module LordOfTheRings.Location.Cards.RaurosFalls (raurosFalls) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype RaurosFalls = RaurosFalls LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

raurosFalls :: LocationCard RaurosFalls
raurosFalls = location RaurosFalls Cards.raurosFalls

-- | While Rauros Falls is the active location, all characters must commit to the current
-- quest during the quest phase.
instance HasModifiersFor RaurosFalls where
  getModifiersFor (RaurosFalls a) = when (a.placement == ActiveLocation) $ modified a GameTarget [MustCommitAll]

-- | Shadow: After this attack resolves, return attacking enemy to the staging area.
instance RunMessage RaurosFalls where
  runMessage msg l@(RaurosFalls a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> RaurosFalls <$> runMessage msg a
