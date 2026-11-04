module LordOfTheRings.Location.Cards.HauntedPath (hauntedPath) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.ThePassingOfTheGreyCompany.Helpers (hasThreatAtLeast35)

newtype HauntedPath = HauntedPath LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

hauntedPath :: LocationCard HauntedPath
hauntedPath = location HauntedPath Cards.hauntedPath

-- | Haunted Path gets +1 threat for each player with threat 35 or higher.
instance HasModifiersFor HauntedPath where
  getModifiersFor (HauntedPath a) = do
    x <- length <$> (filterM hasThreatAtLeast35 =<< getRemainingPlayers)
    when (x > 0) $ modifySelf a [ThreatModifier x]

-- | Travel: Raise each player's threat by 1 to travel here.
-- Shadow: Either raise your threat by 2, or return attacking enemy to the staging area after
-- this attack.
instance RunMessage HauntedPath where
  runMessage msg l@(HauntedPath a) = case msg of
    TravelTo lid | lid == a.id -> do
      eachPlayer (`raiseThreat` 1)
      HauntedPath <$> runMessage msg a
    ResolveShadowEffect cid _ pid _ | cid == a.cardId -> do
      chooseOneM pid do
        questionLabeled "hauntedPath.choose"
        causedBy a
        labeled "hauntedPath.raiseThreat" $ encounterRaisesThreat pid 2
        labeled "hauntedPath.return" $ doStep 1 msg
      pure l
    DoStep 1 (ResolveShadowEffect cid eid _ _) | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> HauntedPath <$> runMessage msg a
