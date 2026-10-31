module LordOfTheRings.Location.Cards.MountainsOfShadow (mountainsOfShadow) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)

newtype MountainsOfShadow = MountainsOfShadow LocationAttrs
  deriving anyclass (IsLocation, HasAbilities, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainsOfShadow :: LocationCard MountainsOfShadow
mountainsOfShadow = location MountainsOfShadow Cards.mountainsOfShadow

-- | While Mountains of Shadow is the active location, it gains: "Forced: After you turn 1
-- or more hidden cards faceup, take 1 hidden card."
-- Shadow: Either take 2 hidden cards, or this attack is considered undefended. An
-- undefended attack already is, so it asks nothing.
instance RunMessage MountainsOfShadow where
  runMessage msg l@(MountainsOfShadow a) = case msg of
    -- ponytail: answered for each hidden card turned, not once for each batch of them
    TurnHiddenCardFaceup pid _ _ | a.placement == ActiveLocation -> do
      takeHiddenCard pid
      MountainsOfShadow <$> runMessage msg a
    ResolveShadowEffect cid _ pid defenders | cid == a.cardId -> do
      unless (null defenders) do
        chooseOneM pid do
          questionLabeled "mountainsOfShadow.choose"
          causedBy a
          labeled "mountainsOfShadow.take" $ replicateM_ 2 (takeHiddenCard pid)
          labeled "mountainsOfShadow.undefended" $ doStep 1 msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid pid _) | cid == a.cardId -> do
      makeUndefended pid eid
      pure l
    _ -> MountainsOfShadow <$> runMessage msg a
