module LordOfTheRings.Location.Cards.BarrenHills (barrenHills) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype BarrenHills = BarrenHills LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

barrenHills :: LocationCard BarrenHills
barrenHills = location BarrenHills Cards.barrenHills

-- | While Barren Hills is the active location, ignore ambush.
instance HasModifiersFor BarrenHills where
  getModifiersFor (BarrenHills a) = when (a.placement == ActiveLocation) $ modified a GameTarget [IgnoreAmbush]

-- | Shadow: Return attacking enemy to the staging area after it attacks.
instance RunMessage BarrenHills where
  runMessage msg l@(BarrenHills a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> BarrenHills <$> runMessage msg a
