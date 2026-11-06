module LordOfTheRings.Location.Cards.MerchantsDistrict (merchantsDistrict) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Phase (Phase (TravelPhase))
import LordOfTheRings.Scenario.Scenarios.TheBattleOfLakeTown.Helpers (lakeTownObjectives)

newtype MerchantsDistrict = MerchantsDistrict LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

merchantsDistrict :: LocationCard MerchantsDistrict
merchantsDistrict = location MerchantsDistrict Cards.merchantsDistrict

instance HasAbilities MerchantsDistrict where
  getAbilities (MerchantsDistrict a) = [anyPlayer $ restricted a 1 (DuringPhase TravelPhase) $ ActionAbility Free]

instance RunMessage MerchantsDistrict where
  runMessage msg l@(MerchantsDistrict a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      n <- length <$> getPlayerOrder
      towns <- lakeTownObjectives
      for_ towns \oid -> dealDamage a oid (2 * n)
      addToVictoryDisplay a
      pure l
    ResolveShadowEffect cid _ _ defenders | cid == a.cardId -> do
      towns <- lakeTownObjectives
      for_ towns \oid -> dealDamage (CardIdSource cid) oid (if null defenders then 4 else 1)
      pure l
    _ -> MerchantsDistrict <$> runMessage msg a
