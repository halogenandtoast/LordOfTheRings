module LordOfTheRings.Location.Cards.AbandonedCamp (abandonedCamp) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.AcrossTheEttenmoors.Helpers

newtype AbandonedCamp = AbandonedCamp LocationAttrs
  deriving anyclass (IsLocation)
  deriving newtype (Show, Eq, ToJSON, Entity)

-- | Guarded. Safe. Immune to player card effects.
abandonedCamp :: LocationCard AbandonedCamp
abandonedCamp = location AbandonedCamp Cards.abandonedCamp

instance HasModifiersFor AbandonedCamp where
  getModifiersFor (AbandonedCamp a) = safeModifiers a

-- | Response: After you travel to Abandoned Camp, each player may return 1 card from his
-- discard pile to his hand.
instance HasAbilities AbandonedCamp where
  getAbilities (AbandonedCamp a) = mkAbility a 1 (response $ TraveledWindow After a.id) : safeAbilities a

instance RunMessage AbandonedCamp where
  runMessage msg l@(AbandonedCamp a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      eachPlayer \pid -> do
        discarded <- (.discard) <$> getPlayer pid
        unless (null discarded) $ chooseOneM pid do
          questionLabeled "abandonedCamp.return"
          causedBy a
          showing discarded
          targets discarded \c -> returnFromDiscardToHand pid c.id
          labeled "abandonedCamp.none" (pure ())
      pure l
    _ -> do
      runSafe msg a
      AbandonedCamp <$> runMessage msg a
