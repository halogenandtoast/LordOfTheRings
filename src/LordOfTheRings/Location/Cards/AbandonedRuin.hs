module LordOfTheRings.Location.Cards.AbandonedRuin (abandonedRuin) where

import LordOfTheRings.Helpers.Treasure (guardUndiscoveredTreasure)
import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted

newtype AbandonedRuin = AbandonedRuin LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | A burden of the Ered Mithrin campaign, from The King's Quest. Its Setup, into the Caves
-- deck, is the campaign's.
abandonedRuin :: LocationCard AbandonedRuin
abandonedRuin = location AbandonedRuin Cards.abandonedRuin

{- | Response: After Abandoned Ruin becomes the active location, look at 3 random
undiscovered treasures and attach 1 of them to Abandoned Ruin as a guarded objective. When
that treasure is free of encounters, the players claim it and attach it to an eligible
character. Limit once per game for the group.
-}
instance HasAbilities AbandonedRuin where
  getAbilities (AbandonedRuin a) =
    [limited (GroupLimit PerGame 1) $ mkAbility a 1 $ response $ LocationBecameActiveWindow After (LocationWithId a.id)]

instance RunMessage AbandonedRuin where
  runMessage msg l@(AbandonedRuin a) = case msg of
    UseCardAbility pid (isSource a -> True) 1 _ -> do
      guardUndiscoveredTreasure pid a a.cardId
      pure l
    _ -> AbandonedRuin <$> runMessage msg a
