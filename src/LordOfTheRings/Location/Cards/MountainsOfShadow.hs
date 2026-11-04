module LordOfTheRings.Location.Cards.MountainsOfShadow (mountainsOfShadow) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheBloodOfGondor.Helpers (takeHiddenCard)

newtype MountainsOfShadow = MountainsOfShadow LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

mountainsOfShadow :: LocationCard MountainsOfShadow
mountainsOfShadow = location MountainsOfShadow Cards.mountainsOfShadow

-- | While Mountains of Shadow is the active location, it gains: "Forced: After you turn 1
-- or more hidden cards faceup, take 1 hidden card." Once for each time any are turned.
instance HasAbilities MountainsOfShadow where
  getAbilities (MountainsOfShadow a) = [mkAbility a 1 $ forced $ TurnedHiddenCardsFaceupWindow After | a.placement == ActiveLocation]

-- | Shadow: Either take 2 hidden cards, or this attack is considered undefended. An
-- undefended attack already is, so it asks nothing.
instance RunMessage MountainsOfShadow where
  runMessage msg l@(MountainsOfShadow a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 ws -> do
      for_ [pid | Window _ (TurnedHiddenCardsFaceup pid) <- ws] takeHiddenCard
      pure l
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
