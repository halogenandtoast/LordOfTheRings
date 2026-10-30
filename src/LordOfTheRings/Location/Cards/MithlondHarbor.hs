module LordOfTheRings.Location.Cards.MithlondHarbor (mithlondHarbor) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype MithlondHarbor = MithlondHarbor LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

mithlondHarbor :: LocationCard MithlondHarbor
mithlondHarbor = location MithlondHarbor Cards.mithlondHarbor

-- | While Mithlond Harbor is in the staging area, progress cannot be placed on locations in
-- the staging area.
instance HasModifiersFor MithlondHarbor where
  getModifiersFor (MithlondHarbor a) = when (a.placement == StagingArea) do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ProgressLimit 0]

-- | Shadow: Attacking enemy gets +1 attack for each damage token on the active location.
instance RunMessage MithlondHarbor where
  runMessage msg l@(MithlondHarbor a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      damage <- sum <$> (traverse (getDamage . LocationTarget) =<< select IsActiveLocation)
      when (damage > 0) $ enemyGetsAttack (CardIdSource cid) eid damage
      pure l
    _ -> MithlondHarbor <$> runMessage msg a
