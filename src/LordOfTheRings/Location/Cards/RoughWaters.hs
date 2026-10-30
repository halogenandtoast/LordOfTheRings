module LordOfTheRings.Location.Cards.RoughWaters (roughWaters) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (StagingArea))

newtype RoughWaters = RoughWaters LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

roughWaters :: LocationCard RoughWaters
roughWaters = location RoughWaters Cards.roughWaters

-- | While Rough Waters is in the staging area, cancel the first sailing success icon found
-- during each Sailing test.
instance HasModifiersFor RoughWaters where
  getModifiersFor (RoughWaters a) = when (a.placement == StagingArea) $ modified a GameTarget [CancelFirstSailingIcon]

-- | Shadow: If you are off-course, attacking enemy gets +2 attack.
instance RunMessage RoughWaters where
  runMessage msg l@(RoughWaters a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      whenM isOffCourse $ enemyGetsAttack (CardIdSource cid) eid 2
      pure l
    _ -> RoughWaters <$> runMessage msg a
