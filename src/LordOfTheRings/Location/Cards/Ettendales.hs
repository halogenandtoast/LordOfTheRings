module LordOfTheRings.Location.Cards.Ettendales (ettendales) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.FlightToTheFord.Helpers (reduceRingBearersLife)

newtype Ettendales = Ettendales LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

ettendales :: LocationCard Ettendales
ettendales = location Ettendales Cards.ettendales

instance HasAbilities Ettendales where
  getAbilities (Ettendales a) = [mkAbility a 1 $ forced $ ExploredWindow When a.id]

instance RunMessage Ettendales where
  runMessage msg l@(Ettendales a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      shuffleTopOfBurdenDeckIntoEncounterDeck
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      enemyGetsAttack (CardIdSource cid) eid 2
      ifThisAttackDestroys eid msg
      pure l
    DoStep 1 (ResolveShadowEffect cid _ _ _) | cid == a.cardId -> do
      reduceRingBearersLife 1
      pure l
    _ -> Ettendales <$> runMessage msg a
