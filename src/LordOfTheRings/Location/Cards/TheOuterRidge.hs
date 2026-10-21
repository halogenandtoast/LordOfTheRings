module LordOfTheRings.Location.Cards.TheOuterRidge (theOuterRidge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))

newtype TheOuterRidge = TheOuterRidge LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

theOuterRidge :: LocationCard TheOuterRidge
theOuterRidge = location TheOuterRidge Cards.theOuterRidge

-- | While The Outer Ridge is the active location, each location in the staging area gets
-- +1 threat.
instance HasModifiersFor TheOuterRidge where
  getModifiersFor (TheOuterRidge a) = when (a.placement == ActiveLocation) do
    staged <- select LocationInStagingArea
    for_ staged \l -> modified a l [ThreatModifier 1]

-- | Shadow: After this attack resolves, return attacking enemy to the staging area.
instance RunMessage TheOuterRidge where
  runMessage msg l@(TheOuterRidge a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> TheOuterRidge <$> runMessage msg a
