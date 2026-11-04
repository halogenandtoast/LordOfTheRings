module LordOfTheRings.Location.Cards.PelennorField (pelennorField) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Trait (Trait (Harad))

newtype PelennorField = PelennorField LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

pelennorField :: LocationCard PelennorField
pelennorField = location PelennorField Cards.pelennorField

-- | While at least 1 enemy is in the staging area, progress cannot be placed on locations
-- in the staging area.
instance HasModifiersFor PelennorField where
  getModifiersFor (PelennorField a) =
    whenM (selectAny EnemyInStagingArea) do
      staged <- select LocationInStagingArea
      for_ staged \l -> modified a l [ProgressLimit 0]

-- | Shadow: Attacking enemy gets +1 attack (+2 attack instead if it has the Harad trait).
instance RunMessage PelennorField where
  runMessage msg l@(PelennorField a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == (toAttrs a).cardId -> do
      haradrim <- selectAny (EnemyWithId eid <> EnemyWithTrait Harad)
      enemyGetsAttack a eid (if haradrim then 2 else 1)
      pure l
    _ -> PelennorField <$> runMessage msg a
