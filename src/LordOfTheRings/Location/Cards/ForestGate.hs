module LordOfTheRings.Location.Cards.ForestGate (forestGate) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype ForestGate = ForestGate LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

forestGate :: LocationCard ForestGate
forestGate = location ForestGate Cards.forestGate

-- | Response: After you travel to Forest Gate, the first player may draw 2 cards.
instance HasAbilities ForestGate where
  getAbilities (ForestGate a) = [mkAbility a 1 $ response $ TraveledWindow After a.id]

instance RunMessage ForestGate where
  runMessage msg l@(ForestGate a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      lead <- getLead
      push $ DrawCards $ Drawing lead 2
      pure l
    _ -> ForestGate <$> runMessage msg a
