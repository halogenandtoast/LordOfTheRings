module LordOfTheRings.Location.Cards.BanksOfMorthond (banksOfMorthond) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Placement (Placement (ActiveLocation))
import LordOfTheRings.Scenario.Scenarios.TheStoneOfErech.Helpers (currentNightObjective)

newtype BanksOfMorthond = BanksOfMorthond LocationAttrs
  deriving anyclass (IsLocation, HasAbilities)
  deriving newtype (Show, Eq, ToJSON, Entity)

banksOfMorthond :: LocationCard BanksOfMorthond
banksOfMorthond = location BanksOfMorthond Cards.banksOfMorthond

instance HasModifiersFor BanksOfMorthond where
  getModifiersFor (BanksOfMorthond a) = when (a.placement == ActiveLocation) do
    locations <- select LocationInStagingArea
    for_ locations \lid -> modified a lid [ThreatModifier (-1)]

instance RunMessage BanksOfMorthond where
  runMessage msg l@(BanksOfMorthond a) = case msg of
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      traverse_ (\oid -> placeProgress a oid 1) =<< currentNightObjective
      pure l
    _ -> BanksOfMorthond <$> runMessage msg a
