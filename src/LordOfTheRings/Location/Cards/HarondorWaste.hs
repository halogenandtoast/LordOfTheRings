module LordOfTheRings.Location.Cards.HarondorWaste (harondorWaste) where

import LordOfTheRings.Location.CardDefs qualified as Cards
import LordOfTheRings.Location.Import.Lifted
import LordOfTheRings.Scenario.Scenarios.TheCrossingsOfPoros.Helpers (withRandomFromLargestSetAside)

newtype HarondorWaste = HarondorWaste LocationAttrs
  deriving anyclass (IsLocation, HasModifiersFor)
  deriving newtype (Show, Eq, ToJSON, Entity)

harondorWaste :: LocationCard HarondorWaste
harondorWaste = location HarondorWaste Cards.harondorWaste

instance HasAbilities HarondorWaste where
  getAbilities (HarondorWaste a) = [mkAbility a 1 $ forced $ LocationBecameActiveWindow After (LocationWithId a.id)]

{- | Forced: After Harondor Waste becomes the active location, choose 1 random card from
the set aside encounter set with the most cards remaining and place it in the encounter
discard pile. Then, shuffle the encounter discard pile into the encounter deck.
Shadow: Return attacking enemy to the staging area after this attack.
-}
instance RunMessage HarondorWaste where
  runMessage msg l@(HarondorWaste a) = case msg of
    UseCardAbility _ (isSource a -> True) 1 _ -> do
      withRandomFromLargestSetAside (discardSetAside . pure)
      shuffleEncounterDiscardIntoDeck AnyCard
      pure l
    ResolveShadowEffect cid eid _ _ | cid == a.cardId -> do
      returnToStagingAfterAttack eid
      pure l
    _ -> HarondorWaste <$> runMessage msg a
