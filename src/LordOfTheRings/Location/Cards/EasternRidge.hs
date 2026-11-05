module LordOfTheRings.Location.Cards.EasternRidge (easternRidge) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheBattleOfFiveArmies.Helpers (stagesWithoutProgress)

newtype EasternRidge = EasternRidge LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

easternRidge :: LocationCard EasternRidge
easternRidge = location EasternRidge Cards.easternRidge

instance HasAbilities EasternRidge where
  getAbilities (EasternRidge a) = [mkAbility a 1 $ forced RoundEndsWindow]

instance RunMessage EasternRidge where
  runMessage msg l@(EasternRidge a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      n <- stagesWithoutProgress
      eachPlayer \pid -> encounterRaisesThreat pid n
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      n <- stagesWithoutProgress
      phaseModifier (CardIdSource cid) eid (DefenseModifier n)
      pure l
    _ -> EasternRidge <$> runMessage msg a
