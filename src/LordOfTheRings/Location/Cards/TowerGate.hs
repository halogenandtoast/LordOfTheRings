module LordOfTheRings.Location.Cards.TowerGate (towerGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype TowerGate = TowerGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

towerGate :: LocationCard TowerGate
towerGate = location TowerGate Cards.towerGate

-- | Forced: After travelling to Tower Gate, each player places the top card of his deck,
-- face down in front of him, as if it just engaged him from the staging area. These cards
-- are called "Orc Guard", and act as enemies with: 1 hit point, 1 attack, and 1 defense.
instance HasAbilities TowerGate where
  getAbilities (TowerGate a) = [mkAbility a 1 $ forced $ TraveledWindow After a.id]

instance RunMessage TowerGate where
  runMessage msg l@(TowerGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer $ push . PlaceOrcGuard
      pure l
    _ -> TowerGate <$> runMessage msg a
